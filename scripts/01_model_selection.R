# load packages ----
library(tidyverse)
library(glmnet)
library(caret)
library(MASS)

# load data ----
load("data/cleaned/combined_data.rda")

# remove id variables (not needed for regression, but useful later)
modeling_data <- combined_data %>%
  dplyr::select(-c(id, off_team, def_team))

# bounding logistic regressions ----
## small model (bound for highest simplicity) ----
small_model <- glm(formula = 
    play_type ~ down + distance + yardline + goal_to_go + score_differential +
    time * half + off_pass_strength + off_run_strength + def_pass_strength + def_run_strength,
    
    data = modeling_data, family = "binomial")

summary(small_model)

# no big surprises, run when short distances and pass on late downs
# run is level 2, pass is level 1, so negative coefficients means passing correlated

# little graph to make visualizing point easier
vars <- c('intercept', 'down2', 'down3', 'down4','distancemedium', 'distanceshort', 'yardline', 'goal_to_go', 'score_differential',
          'time', 'half', 'off_pass_strength', 'off_run_strength', 'def_pass_strength', 'def_run_strength', 'time:half') %>% as_tibble()

coefficients <- summary(small_model)$coefficients %>% as_tibble()

coefficients %>%
  bind_cols(vars) %>%
  ggplot(aes(x = reorder(value, -Estimate), y = Estimate)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2) +
  theme(legend.position = "none")

probabilities <- predict(small_model, type = "response")
predictions <- if_else(probabilities > 0.5, 1, 0)
true_play_call <- combined_data$play_type 

table(Predicted = predictions, Actual = true_play_call)

# roughly 62% accuracy with simple model
# null model gives 53% accuracy (just choose pass)

## big model (bound for highest complexity) ----
big_model <- glm(play_type ~ (.)^2, family = "binomial", data = modeling_data)
summary(big_model)

# stepwise aic selection ----
selection_process <- stepAIC(big_model, scope = list(lower = small_model, upper = big_model), 
                      direction = "both", k = log(nrow(combined_data)))

## chosen model ----
chosen_model <- glm(formula = play_type ~ off_pass_strength + off_run_strength + 
                      def_pass_strength + def_run_strength + down + distance + 
                      yardline + goal_to_go + score_differential + time + half + 
                      off_pass_strength:half + off_run_strength:distance + def_pass_strength:score_differential + 
                      def_run_strength:half + down:distance + down:goal_to_go + 
                      down:time + down:half + distance:score_differential + distance:time + 
                      yardline:half + score_differential:time + time:half,
                    
                    family = "binomial", data = modeling_data)

summary(chosen_model)

# notes about interactions (13 total):
# large effect being anything e+-02 and higher

# off_pass_strength:half — big negative
# off_run_strength:distance - big positive for short distance, big negative for medium
# def_pass_strength:score_differential — small negative
# def_run_strength:half — big positive
# down:distance — very impactful, goes both ways
# down:goal_to_go — big negative
# down:time — small negative
# down:half — big positive decreasing as down increases
# distance:score_differential — small to large negative
# distance:time — small positive for short, small negative for medium
# yardline:half — small positive
# score_differential:time — small negative
# time:half — small negative

# rest of single-var conclusions are stable

# cross-validation to confirm robustness ----
# double check smaller effect interactions are meaningful
cv_control <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 20,
                           classProbs = TRUE,
                           savePredictions = "final")

cv_model <- train(play_type ~ off_pass_strength + off_run_strength + 
                    def_pass_strength + def_run_strength + down + distance + 
                    yardline + goal_to_go + score_differential + time + half + 
                    off_pass_strength:half + off_run_strength:distance + def_pass_strength:score_differential + 
                    def_run_strength:half + down:distance + down:goal_to_go + 
                    down:time + down:half + distance:score_differential + distance:time + 
                    yardline:half + score_differential:time + time:half,
                
                    family = "binomial", method = "glm",
                  trControl = cv_control, data = modeling_data)

summary(cv_model)

# accuracy remains pretty constant (~ 64%)
# sd is very low (0.0038)

# green for positive effects, transparent bars are for interaction terms
# lots of key interactions favor run-specific interactions
bind_cols(union("Intercept", cv_model$coefnames), cv_model$finalModel$coefficients) %>% janitor::clean_names() %>%
  mutate(is_interaction = if_else(str_detect(x1, ":"), 1, 0)) %>%
  ggplot(aes(x = reorder(x1, -abs(x2)), y = abs(x2), fill = as.factor(sign(x2)),
             alpha = as.factor(is_interaction))) +
  geom_col() +
  labs(x = NULL, y = "Magnitude of Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  scale_alpha_manual(values = c(1, 0.5))
