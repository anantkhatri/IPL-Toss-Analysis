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

# Graph: Win percentage by toss winner location and decision
toss_home_decision_summary <- toss_home_decision_summary %>%
  mutate(
    group_label = paste(toss_winner_location, "+", str_to_title(toss_decision)),
    group_label = factor(group_label, levels = c("Away + Bat", "Away + Field", "Home + Bat", "Home + Field"))
  )

ggplot(toss_home_decision_summary, aes(x = group_label, y = toss_winner_match_win_pct, fill = toss_winner_match_win_pct)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(toss_winner_match_win_pct, "%\n(n=", matches, ")")),
            vjust = -0.4, size = 3.8, fontface = "bold", color = "#1a1a1a") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", guide = "none") +
  labs(
    title = "Match Win Percentage by Toss Winner Location and Decision",
    x = "Toss Winner Status and Decision",
    y = "Match Win Percentage (%)"
  ) +
  ylim(0, 75) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    panel.grid.major.x = element_blank()
  )

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

# Graph: Toss to match win conversion rate by team
ggplot(toss_summary, aes(x = reorder(toss_winner, toss_to_match_win_pct),
                         y = toss_to_match_win_pct,
                         fill = toss_to_match_win_pct)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(toss_to_match_win_pct, "%")),
            hjust = -0.2, size = 3.5, fontface = "bold",
            color = "#1a1a1a") +
  coord_flip() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", guide = "none") +
  scale_y_continuous(limits = c(0, 85),
                     breaks = seq(0, 80, by = 20),
                     labels = function(x) paste0(x, "%")) +
  geom_vline(xintercept = 0, color = "#aaaaaa", linewidth = 0.5) +
  labs(
    title = "Toss to Match Win Conversion Rate by Team",
    x = "Team",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    panel.grid.major.y = element_blank()
  )

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

# Graph: Toss winner match win percentage by season
season_toss_summary <- match_data_toss %>%
  group_by(season) %>%
  summarise(
    total_matches = n(),
    toss_win_pct = round(mean(toss_match_result, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(season)

ggplot(season_toss_summary, aes(x = factor(season), y = toss_win_pct, group = 1)) +
  geom_line(color = "#238b45", linewidth = 1.8) +
  geom_point(color = "#00441b", size = 4, shape = 21,
             fill = "#00441b", stroke = 1.5) +
  geom_ribbon(aes(ymin = 50, ymax = toss_win_pct),
              fill = "#74c476", alpha = 0.15) +
  geom_text(aes(label = paste0(toss_win_pct, "%")),
            vjust = -1.2, size = 3.5, fontface = "bold",
            color = "#00441b") +
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "#aaaaaa", linewidth = 0.9) +
  annotate("text", x = 10.4, y = 51.2,
           label = "50% (chance)", size = 3.2,
           color = "#888888", fontface = "italic") +
  scale_y_continuous(limits = c(35, 70),
                     breaks = seq(35, 70, by = 5),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Toss Winner Match Win Percentage by Season",
    x = "IPL Season",
    y = "Toss Winner Match Win Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    panel.grid.major.x = element_blank()
  )

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