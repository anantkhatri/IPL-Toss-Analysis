library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

# 1
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
match_out_path <- file.path(base_dir, "01_match_data_2008_2010_to_2019.csv")

##
match_data <- read_csv(match_out_path, show_col_types = FALSE)

##
match_data_toss <- match_data %>%
  mutate(
    toss_match_result = ifelse(!is.na(toss_winner) & !is.na(match_winner) & toss_winner == match_winner, 1, 0),
    toss_winner_at_home = ifelse(!is.na(toss_winner) & !is.na(home_team) & toss_winner == home_team, 1, 0),
    toss_winner_away = ifelse(!is.na(toss_winner_at_home), ifelse(toss_winner_at_home == 1, 0, 1), NA),
    toss_home_win = ifelse(toss_winner_at_home == 1 & toss_match_result == 1, 1, 0),
    toss_away_win = ifelse(toss_winner_at_home == 0 & toss_match_result == 1, 1, 0)
  )

# 2. Toss summary table
## These are conditional percentages, not percentages of all matches
overall_toss_win_pct <- match_data_toss %>%
  summarise(
    count_matches = n(),
    toss_winner_match_win_pct = round(mean(toss_match_result, na.rm = TRUE) * 100, 2)
  ) %>%
  mutate(
    category = "Overall"
  )

home_toss_win_pct <- match_data_toss %>%
  filter(toss_winner_at_home == 1) %>%
  summarise(
    count_matches = n(),
    toss_winner_match_win_pct = round(mean(toss_match_result, na.rm = TRUE) * 100, 2)
  ) %>%
  mutate(
    category = "Toss winner was home team"
  )

away_toss_win_pct <- match_data_toss %>%
  filter(toss_winner_at_home == 0) %>%
  summarise(
    count_matches = n(),
    toss_winner_match_win_pct = round(mean(toss_match_result, na.rm = TRUE) * 100, 2)
  ) %>%
  mutate(
    category = "Toss winner was away team"
  )

summary_table <- bind_rows(
  overall_toss_win_pct,
  home_toss_win_pct,
  away_toss_win_pct
) %>%
  select(category, count_matches, toss_winner_match_win_pct)

print(summary_table)

# 3. Toss decision summary
toss_decision_summary <- match_data_toss %>%
  group_by(toss_decision) %>%
  summarise(
    matches = n(),
    toss_winner_match_win_pct = round(mean(toss_match_result, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(matches))

print(toss_decision_summary)

# 4. Toss winner at home + decision summary
toss_home_decision_summary <- match_data_toss %>%
  mutate(
    toss_winner_location = case_when(
      toss_winner_at_home == 1 ~ "Home",
      toss_winner_at_home == 0 ~ "Away",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(toss_winner_location, toss_decision) %>%
  summarise(
    matches = n(),
    toss_winner_match_win_pct = round(mean(toss_match_result, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(toss_winner_location, toss_decision)

print(toss_home_decision_summary)

# 5. Toss-wins by team summary
toss_summary <- match_data_toss %>%
  group_by(toss_winner) %>%
  summarise(
    toss_wins = n(),
    toss_to_match_win_pct = round(mean(toss_match_result, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(toss_wins))

print(toss_summary)

# 6. Toss-wins by team: graph
ggplot(toss_summary, aes(x = reorder(toss_winner, toss_wins), y = toss_wins, fill = toss_wins)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = toss_wins), hjust = -0.2, size = 3.5) +
  labs(
    title = "Number of Toss Wins by Team",
    x = "Team",
    y = "Number of tosses won"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  ) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  ylim(0, max(toss_summary$toss_wins) + 5)

# 7. Toss match result distribution
toss_match_percentage <- match_data_toss %>%
  group_by(toss_match_result) %>%
  summarise(
    count = n(),
    percentage = round((n() / nrow(match_data_toss)) * 100, 2),
    .groups = "drop"
  ) %>%
  mutate(
    toss_match_result = ifelse(toss_match_result == 1,
                               "Toss winner also won the match",
                               "Toss winner did not win the match")
  )

print(toss_match_percentage)

##
match_out_path_2 <- file.path(base_dir, "02_match_data_toss_2008_2010_to_2019.csv")
write_csv(match_data_toss, match_out_path_2)