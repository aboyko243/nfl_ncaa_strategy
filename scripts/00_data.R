# load packages ----
library(tidyverse)
library(nflfastR)
library(cfbfastR)

# load data ----
nfl_data <- load_pbp(seasons = 2024)
ncaa_data <- load_cfb_pbp(seasons = 2024)

## write out data ----
write_csv(nfl_data, file = "data/raw/nfl_data.csv")
write_csv(ncaa_data, file = "data/raw/ncaa_data.csv")

# nfl data cleaning ----
nfl_data %>%
  naniar::miss_var_summary() %>%
  print(n = 372)

## check if week 18 is similar data
chi_data <- nfl_data %>%
  mutate(week_18 = if_else(week == 18, "yes", "no")) %>%
  filter(play_type %in% c("run", "pass"), !is.na(down)) %>%
  summarize(count = sum(play), .by = c(play_type, down, week_18)) %>%
  mutate(prop = if_else(week_18 == "no", count / 33050 * 100, count / 2000 * 100),
         type = str_c(play_type, down, sep = "_")) %>%
  group_by(type, week_18) %>%
  summarize(total = sum(count), .groups = 'drop') %>%
  pivot_wider(names_from = week_18, values_from = total, values_fill = 0)

chi_data <- chi_data[, -1]
chisq.test(as.matrix(chi_data))

# test yields p-value of 0.23
# conclusion: include week 18 in data

## epa data ----
nfl_off_strengths <- nfl_data %>%
  filter(season_type == "REG", play_type %in% c("run", "pass"), !is.na(posteam)) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE), .by = c(posteam, play_type)) %>%
  mutate(epa_plus = (mean_epa - 0.00638) / 0.112) %>%
  select(-mean_epa) %>%
  pivot_wider(names_from = play_type, values_from = c(epa_plus)) %>%
  rename(off_run_strength = run, off_pass_strength = pass) %>%
  arrange(posteam)

nfl_def_strengths <- nfl_data %>%
  filter(season_type == "REG", play_type %in% c("run", "pass"), !is.na(defteam)) %>%
  summarize(mean_epa = -1 * mean(epa, na.rm = TRUE), .by = c(defteam, play_type)) %>%
  mutate(epa_plus = (mean_epa + 0.00488) / 0.0755) %>%
  select(-mean_epa) %>%
  pivot_wider(names_from = play_type, values_from = c(epa_plus)) %>%
  rename(def_run_strength = run, def_pass_strength = pass) %>%
  arrange(defteam)

### write out data ----
write_csv(nfl_off_strengths, file = "data/cleaned/nfl_off_strengths.csv")
write_csv(nfl_def_strengths, file = "data/cleaned/nfl_def_strengths.csv")

## nfl regression data ----
cleaned_nfl_data <- nfl_data %>%
  filter(season_type == "REG", play_type %in% c("run", "pass"), !is.na(posteam), !is.na(down)) %>%
  select(posteam, defteam, yardline_100, half_seconds_remaining, game_half, down,
         goal_to_go, ydstogo, play_type, score_differential) %>%
  mutate(half = if_else(game_half == "Half1", 1, 0),
         distance = case_when(ydstogo < 5 ~ "short",
                              ydstogo > 7 ~ "long", .default = "medium")) %>%
  left_join(nfl_off_strengths, by = join_by(posteam == posteam)) %>%
  left_join(nfl_def_strengths, by = join_by(defteam == defteam)) %>%
  rename(off_team = posteam, def_team = defteam, yardline = yardline_100, time = half_seconds_remaining) %>%
  select(play_type, off_team, off_pass_strength, off_run_strength, def_team, def_pass_strength, def_run_strength,
         down, distance, yardline, goal_to_go, score_differential, time, half)

### write out data ----
write_csv(cleaned_nfl_data, file = "data/cleaned/cleaned_nfl_data.csv")

# ncaa data cleaning ----
ncaa_data %>%
  naniar::miss_var_summary() %>%
  print(n = 330)

power_conferences <- c("ACC", "Big Ten", "Big 12", "SEC")

## epa data ----
ncaa_off_strengths <- ncaa_data %>%
  filter(offense_conference %in% power_conferences, defense_conference %in% power_conferences,
         rush == 1 | pass == 1, !is.na(EPA)) %>%
  summarize(mean_epa = mean(EPA), .by = c(pos_team, pass)) %>%
  mutate(epa_plus = (mean_epa - 0.0095) / 0.124) %>%
  select(-mean_epa) %>%
  pivot_wider(names_from = pass, values_from = c(epa_plus)) %>%
  rename(off_run_strength = `0`, off_pass_strength = `1`) %>%
  arrange(pos_team)

ncaa_def_strengths <- ncaa_data %>%
  filter(offense_conference %in% power_conferences, defense_conference %in% power_conferences,
         rush == 1 | pass == 1, !is.na(EPA)) %>%
  summarize(mean_epa = -1 * mean(EPA), .by = c(def_pos_team, pass)) %>%
  mutate(epa_plus = (mean_epa + 0.019) / 0.112) %>%
  select(-mean_epa) %>%
  pivot_wider(names_from = pass, values_from = c(epa_plus)) %>%
  rename(def_run_strength = `0`, def_pass_strength = `1`) %>%
  arrange(def_pos_team)

### write out data ----
write_csv(ncaa_off_strengths, file = "data/cleaned/ncaa_off_strengths.csv")
write_csv(ncaa_def_strengths, file = "data/cleaned/ncaa_def_strengths.csv")

## ncaa regression data ----
cleaned_ncaa_data <- ncaa_data %>%
  janitor::clean_names() %>%
  filter(offense_conference %in% power_conferences, defense_conference %in% power_conferences,
         rush == 1 | pass == 1, !is.na(epa)) %>%
  select(pos_team, def_pos_team, yard_line, half, time_secs_rem, down, goal_to_go,
         distance, score_diff, rush, pass) %>%
  mutate(goal_to_go = as.numeric(goal_to_go),
         play_type = if_else(rush == 1, "run", "pass"),
         distance = case_when(distance < 5 ~ "short",
                              distance > 7 ~ "long", .default = "medium")) %>%
  left_join(ncaa_off_strengths, by = join_by(pos_team == pos_team)) %>%
  left_join(ncaa_def_strengths, by = join_by(def_pos_team == def_pos_team)) %>%
  rename(off_team = pos_team, def_team = def_pos_team, yardline = yard_line, score_differential = score_diff,
         time = time_secs_rem) %>%
  select(play_type, off_team, off_pass_strength, off_run_strength, def_team, def_pass_strength, def_run_strength,
         down, distance, yardline, goal_to_go, score_differential, time, half)

### write out data ----
write_csv(cleaned_ncaa_data, file = "data/cleaned/cleaned_ncaa_data.csv")

# combine data for regression ----
combined_data <- cleaned_nfl_data %>%
  mutate(id = "nfl") %>%
  bind_rows(cleaned_ncaa_data %>% mutate(id = "ncaa")) %>%
  relocate(id)

combined_data$id <- factor(combined_data$id, labels = c("ncaa", "nfl"), levels = c("ncaa", "nfl"))
combined_data$down <- factor(combined_data$down, labels = c("1", "2", "3", "4"), levels = c("1", "2", "3", "4"))
combined_data$play_type <- factor(combined_data$play_type, labels = c("run", "pass"), levels = c("run", "pass"))
combined_data$distance <-  factor(combined_data$distance, labels = c("short", "medium", "long"), levels = c("short", "medium", "long"))

## write out data ----
save(combined_data, file = "data/cleaned/combined_data.rda")
