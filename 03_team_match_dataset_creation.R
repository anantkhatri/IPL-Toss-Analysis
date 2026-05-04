library(dplyr)
library(readr)
library(stringr)

# 1
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
match_toss_path <- file.path(base_dir, "02_match_data_toss_2008_2010_to_2019.csv")

##
match_data_toss <- read_csv(match_toss_path, show_col_types = FALSE)

# 3. Batting-first team
match_data_toss <- match_data_toss %>%
  mutate(
    batting_first_team = case_when(
      toss_decision == "bat" & toss_winner == team1 ~ team1,
      toss_decision == "bat" & toss_winner == team2 ~ team2,
      toss_decision == "field" & toss_winner == team1 ~ team2,
      toss_decision == "field" & toss_winner == team2 ~ team1,
      TRUE ~ NA_character_
    )
  )

# 4. Two team-level rows
team_match_data <- bind_rows(
  
  match_data_toss %>%
    transmute(
      match_id = match_id,
      date = date,
      season = season,
      city = city,
      venue = venue,
      team = team1,
      opponent = team2,
      home_team = home_team,
      is_home = ifelse(!is.na(home_team) & team1 == home_team, 1, 0),
      won_toss = ifelse(team1 == toss_winner, 1, 0),
      toss_winner_at_home = toss_winner_at_home,
      team_innings_order = ifelse(team1 == batting_first_team, "bat_first", "bowl_first"),
      batting_first = ifelse(team1 == batting_first_team, 1, 0),
      won_match = ifelse(!is.na(match_winner) & team1 == match_winner, 1, 0),
      match_winner_missing = ifelse(is.na(match_winner), 1, 0)
    ),
  
  match_data_toss %>%
    transmute(
      match_id = match_id,
      date = date,
      season = season,
      city = city,
      venue = venue,
      team = team2,
      opponent = team1,
      home_team = home_team,
      is_home = ifelse(!is.na(home_team) & team2 == home_team, 1, 0),
      won_toss = ifelse(team2 == toss_winner, 1, 0),
      toss_winner_at_home = toss_winner_at_home,
      team_innings_order = ifelse(team2 == batting_first_team, "bat_first", "bowl_first"),
      batting_first = ifelse(team2 == batting_first_team, 1, 0),
      won_match = ifelse(!is.na(match_winner) & team2 == match_winner, 1, 0),
      match_winner_missing = ifelse(is.na(match_winner), 1, 0)
    )
)

# 5. Clean ordering
team_match_data <- team_match_data %>%
  arrange(season, date, match_id, team)

# 6. 
cat("Number of original matches: ", nrow(match_data_toss), "\n")
cat("Number of team-level rows: ", nrow(team_match_data), "\n")
cat("Expected team-level rows: ", nrow(match_data_toss) * 2, "\n")

match_row_check <- team_match_data %>%
  count(match_id)

cat("Minimum rows per match_id: ", min(match_row_check$n), "\n")
cat("Maximum rows per match_id: ", max(match_row_check$n), "\n")

win_check <- team_match_data %>%
  group_by(match_id) %>%
  summarise(total_match_wins = sum(won_match, na.rm = TRUE), .groups = "drop")

cat("Minimum won_match total per match: ", min(win_check$total_match_wins), "\n")
cat("Maximum won_match total per match: ", max(win_check$total_match_wins), "\n")

missing_winner_matches <- match_data_toss %>%
  filter(is.na(match_winner)) %>%
  select(match_id, date, team1, team2, venue)

print(missing_winner_matches)

##
print(head(team_match_data, 12))

##
team_match_out_path <- file.path(base_dir, "03_team_match_data_2008_2010.csv")
write_csv(team_match_data, team_match_out_path)
