# load packages ----
library(tidyverse)
library(rpart)
library(rpart.plot)

# load data ----
load("data/cleaned/combined_data.rda")

modeling_data <- combined_data %>%
  select(-c(off_team, def_team))

# interaction model ----
no_interaction_model <- glm(formula = play_type ~ off_pass_strength + off_run_strength + 
                            def_pass_strength + def_run_strength + down + distance + 
                            yardline + goal_to_go + score_differential + time + half + 
                            off_pass_strength:half + off_run_strength:distance + def_pass_strength:score_differential + 
                            def_run_strength:half + down:distance + down:goal_to_go + 
                            down:time + down:half + distance:score_differential + distance:time + 
                            yardline:half + score_differential:time + time:half,
                         
                         family = "binomial", data = modeling_data)

interaction_model <- glm(formula = play_type ~ (off_pass_strength + off_run_strength + 
                      def_pass_strength + def_run_strength + down + distance + 
                      yardline + goal_to_go + score_differential + time + half + 
                      off_pass_strength:half + off_run_strength:distance + def_pass_strength:score_differential + 
                      def_run_strength:half + down:distance + down:goal_to_go + 
                      down:time + down:half + distance:score_differential + distance:time + 
                      yardline:half + score_differential:time + time:half) * id,
                    
                    family = "binomial", data = modeling_data)

# test for model fit ----
anova(no_interaction_model, interaction_model, test = "Chisq")
summary(interaction_model)

## list of interaction with significance (< 0.05) ----
# id term (not interaction. but difference between two leagues overall)
# off_pass_strength:idnfl (negative)
# off_run_strength:idnfl (negative)
# def_pass_strength:idnfl  (positive)
# yardline:idnfl (positive)
# goal_to_go:idnfl (positive)
# time:idnfl (positive)
# half:idnfl (positive)
# off_pass_strength:half:idnfl (positive)
# def_run_strength:half:idnfl (negative)
# down3:time:idnfl (positive)
# down4:half:idnfl (negative)
# time:half:idnfl (negative)  

## difference in coef between two models ----
coefs_before <- bind_cols(names(no_interaction_model$coefficients), no_interaction_model$coefficients) %>%
  rename(vars = `...1`, coefficient_before = `...2`)

coefs_after <- bind_cols(names(interaction_model$coefficients), interaction_model$coefficients) %>%
  rename(vars = `...1`, coefficient_after = `...2`)

coef_differences <- coefs_before %>%
  left_join(coefs_after, by = join_by(vars == vars)) %>%
  mutate(diff = coefficient_after - coefficient_before) %>%
  arrange(-diff)


# visualization ideas ----
altered_data <- model.matrix(interaction_model) %>% as_tibble()

altered_data$predicted_class <- ifelse(predict(interaction_model, type = "response") > 0.5, 1, 0)
tree_model <- rpart(predicted_class ~ ., data = altered_data)

rpart.plot(tree_model)       

# need to adjust, nfl always pass more (+ 7%)        
modeling_data %>%
  mutate(is_pass = if_else(play_type == "pass", 1, 0)) %>%
  summarize(pass_prop = mean(is_pass), .by = id)

# situational adjusted frequencies ----
modeling_data %>%
  count(id, play_type, down, distance) %>%
  summarize(pass_prop = if_else(play_type == "pass", n / sum(n), 0), .by = c(id, down, distance)) %>%
  filter(pass_prop > 0) %>%
  mutate(adj_pass_prop = if_else(id == "ncaa", pass_prop / 0.507, pass_prop / 0.572)) %>%
  ggplot(aes(x = distance, y = adj_pass_prop, fill = id)) +
  geom_col(position = "dodge") +
  facet_wrap(~down) +
  scale_fill_manual(values = c("darkblue", "darkgreen"))

modeling_data %>%
  mutate(q_def_pass = ntile(def_pass_strength, 4),
         is_pass = if_else(play_type == "pass", 1, 0)) %>%
  summarize(pass_prop = mean(is_pass), .by = c(id, down, distance, q_def_pass)) %>%
  mutate(adj_pass_prop = if_else(id == "ncaa", pass_prop / 0.507, pass_prop / 0.572)) %>%
  ggplot(aes(x = distance, y = adj_pass_prop, fill = id)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(down), cols = vars(q_def_pass)) +
  scale_fill_manual(values = c("darkblue", "darkgreen")) +
  ggtitle("rows are down, columns are pass def strength")
