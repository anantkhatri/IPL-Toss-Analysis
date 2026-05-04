library(dplyr)
library(readr)
library(zoo)
library(tidyr)

# 1. File path
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
team_match_path <- file.path(base_dir, "03_team_match_data_2008_2010.csv")

# 2. Read team-level match data
team_match_data <- read_csv(team_match_path, show_col_types = FALSE)

# 3. Remove matches with missing winner
team_match_clean <- team_match_data %>%
  filter(match_winner_missing == 0)

# 4. Sort data
team_match_clean <- team_match_clean %>%
  arrange(team, season, date, match_id)

# 5. Previous available season features
# Since 2009 is excluded, 2010 uses 2008 as previous season
season_summary_for_history <- team_match_clean %>%
  group_by(team, season) %>%
  summarise(
    season_matches = n(),
    season_wins = sum(won_match, na.rm = TRUE),
    season_win_pct = season_wins / season_matches,
    season_home_win_pct = mean(won_match[is_home == 1], na.rm = TRUE),
    season_away_win_pct = mean(won_match[is_home == 0], na.rm = TRUE),
    season_bat_first_win_pct = mean(won_match[batting_first == 1], na.rm = TRUE),
    .groups = "drop"
  )

# 6. Map 2008 season stats as previous season stats for 2010
prev_season_stats <- season_summary_for_history %>%
  filter(season == 2008) %>%
  transmute(
    team = team,
    season = 2010,
    prev_season_matches = season_matches,
    prev_season_wins = season_wins,
    prev_season_win_pct = season_win_pct,
    prev_home_win_pct = season_home_win_pct,
    prev_away_win_pct = season_away_win_pct,
    prev_bat_first_win_pct = season_bat_first_win_pct
  )

# 7. Join previous season stats
team_match_features <- team_match_clean %>%
  left_join(prev_season_stats, by = c("team", "season"))

# 8. Current season before-match features
team_match_features <- team_match_features %>%
  group_by(team, season) %>%
  mutate(
    matches_before = row_number() - 1,
    wins_before = lag(cumsum(won_match), default = 0),
    
    win_pct_before = ifelse(
      matches_before > 0,
      wins_before / matches_before,
      NA_real_
    ),
    
    home_wins_before = lag(cumsum(won_match * is_home), default = 0),
    home_matches_before = lag(cumsum(is_home), default = 0),
    home_win_pct_before = ifelse(
      home_matches_before > 0,
      home_wins_before / home_matches_before,
      NA_real_
    ),
    
    away_wins_before = lag(cumsum(won_match * (1 - is_home)), default = 0),
    away_matches_before = lag(cumsum(1 - is_home), default = 0),
    away_win_pct_before = ifelse(
      away_matches_before > 0,
      away_wins_before / away_matches_before,
      NA_real_
    )
  ) %>%
  ungroup()

# 9. Last 3 matches form
team_match_features <- team_match_features %>%
  group_by(team, season) %>%
  mutate(
    last_3_wins = lag(
      rollapply(
        won_match,
        width = 3,
        FUN = sum,
        fill = NA,
        align = "right"
      ),
      default = NA
    )
  ) %>%
  ungroup()

# 10. Final model dataset
model_data <- team_match_features %>%
  select(
    match_id,
    date,
    season,
    team,
    opponent,
    won_match,
    matches_before,
    wins_before,
    win_pct_before,
    home_win_pct_before,
    away_win_pct_before,
    prev_season_win_pct,
    prev_home_win_pct,
    prev_away_win_pct,
    prev_bat_first_win_pct,
    last_3_wins,
    won_toss,
    toss_winner_at_home,
    batting_first,
    is_home
  ) %>%
  arrange(season, date, match_id, team)

# 11. Replace all NA values in history/performance columns with 0
model_data <- model_data %>%
  mutate(across(
    c(
      win_pct_before,
      home_win_pct_before,
      away_win_pct_before,
      prev_season_win_pct,
      prev_home_win_pct,
      prev_away_win_pct,
      prev_bat_first_win_pct,
      last_3_wins
    ),
    ~ replace_na(., 0)
  ))

# 12. Export final modelling dataset
model_out_path <- file.path(base_dir, "05_model_dataset.csv")
write_csv(model_data, model_out_path)

cat("Final modelling dataset written to: ", model_out_path, "\n")

# 13. Final checks
cat("Total rows: ", nrow(model_data), "\n")
cat("Columns: ", ncol(model_data), "\n")
cat("NA count by column:\n")
print(colSums(is.na(model_data)))

cat("\nPreview of model dataset:\n")
print(head(model_data, 12))