library(dplyr)
library(readr)
library(tidyr)

# 1
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
team_match_path <- file.path(base_dir, "03_team_match_data_2008_2010.csv")

##
team_match_data <- read_csv(team_match_path, show_col_types = FALSE)

# 2. Removing rows where match winner is missing 
## These matches do not contribute to win/loss summaries
team_match_clean <- team_match_data %>%
  filter(match_winner_missing == 0)

# 4. Team-per-season summary
team_season_summary <- team_match_clean %>%
  group_by(season, team) %>%
  summarise(
    matches_played = n(),
    wins = sum(won_match, na.rm = TRUE),
    losses = matches_played - wins,
    win_pct = round((wins / matches_played) * 100, 2),
    
    tosses_won = sum(won_toss, na.rm = TRUE),
    toss_win_pct = round((tosses_won / matches_played) * 100, 2),
    
    matches_won_after_winning_toss = sum(won_toss == 1 & won_match == 1, na.rm = TRUE),
    matches_lost_after_winning_toss = sum(won_toss == 1 & won_match == 0, na.rm = TRUE),
    
    home_matches = sum(is_home == 1, na.rm = TRUE),
    home_wins = sum(is_home == 1 & won_match == 1, na.rm = TRUE),
    home_losses = sum(is_home == 1 & won_match == 0, na.rm = TRUE),
    home_win_pct = ifelse(home_matches > 0,
                          round((home_wins / home_matches) * 100, 2),
                          NA_real_),
    
    away_matches = sum(is_home == 0, na.rm = TRUE),
    away_wins = sum(is_home == 0 & won_match == 1, na.rm = TRUE),
    away_losses = sum(is_home == 0 & won_match == 0, na.rm = TRUE),
    away_win_pct = ifelse(away_matches > 0,
                          round((away_wins / away_matches) * 100, 2),
                          NA_real_),
    
    bat_first_matches = sum(batting_first == 1, na.rm = TRUE),
    bat_first_wins = sum(batting_first == 1 & won_match == 1, na.rm = TRUE),
    bat_first_losses = sum(batting_first == 1 & won_match == 0, na.rm = TRUE),
    bat_first_win_pct = ifelse(bat_first_matches > 0,
                               round((bat_first_wins / bat_first_matches) * 100, 2),
                               NA_real_),
    
    bowl_first_matches = sum(batting_first == 0, na.rm = TRUE),
    bowl_first_wins = sum(batting_first == 0 & won_match == 1, na.rm = TRUE),
    bowl_first_losses = sum(batting_first == 0 & won_match == 0, na.rm = TRUE),
    bowl_first_win_pct = ifelse(bowl_first_matches > 0,
                                round((bowl_first_wins / bowl_first_matches) * 100, 2),
                                NA_real_),
    
    .groups = "drop"
  ) %>%
  arrange(season, team)

##
cat("Number of team-season rows: ", nrow(team_season_summary), "\n")
cat("Seasons included: ", paste(sort(unique(team_season_summary$season)), collapse = ", "), "\n")
cat("Teams included: ", length(unique(team_season_summary$team)), "\n")

## Wins + losses = matches_played
validation_check <- team_season_summary %>%
  mutate(check_total = wins + losses)

cat("Minimum difference between matches_played and wins + losses: ",
    min(team_season_summary$matches_played - validation_check$check_total), "\n")
cat("Maximum difference between matches_played and wins + losses: ",
    max(team_season_summary$matches_played - validation_check$check_total), "\n")

##
print(team_season_summary)

##
team_season_out_path <- file.path(base_dir, "04_team_season_summary.csv")
write_csv(team_season_summary, team_season_out_path)
