library(yaml)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(tibble)
library(readr)

# 1. Directory setup
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
data_dir <- file.path(base_dir, "data")

yaml_files <- list.files(
  path = data_dir,
  pattern = "\\.yaml$",
  full.names = TRUE
)

if (length(yaml_files) == 0) {
  stop("No .yaml files found in ", data_dir)
}

# 2. Helper function
get_or_na <- function(x, ...) {
  out <- tryCatch(
    purrr::pluck(x, ...),
    error = function(e) NA
  )
  if (is.null(out)) NA else out
}

# 3. Name normalisation map
name_fixes <- c(
  "Rising Pune Supergiant" = "Rising Pune Supergiants",
  "Delhi Daredevils" = "Delhi Capitals"
)

# 4. Home team mapping
home_team_map <- c(
  "M Chinnaswamy Stadium" = "Royal Challengers Bangalore",
  "M.Chinnaswamy Stadium" = "Royal Challengers Bangalore",
  "Punjab Cricket Association Stadium, Mohali" = "Kings XI Punjab",
  "Feroz Shah Kotla" = "Delhi Daredevils",
  "Wankhede Stadium" = "Mumbai Indians",
  "Eden Gardens" = "Kolkata Knight Riders",
  "Sawai Mansingh Stadium" = "Rajasthan Royals",
  "Rajiv Gandhi International Stadium, Uppal" = "Deccan Chargers",
  "MA Chidambaram Stadium, Chepauk" = "Chennai Super Kings",
  "MA Chidambaram Stadium" = "Chennai Super Kings",
  "Dr DY Patil Sports Academy" = "Rising Pune Supergiants",
  "Brabourne Stadium" = "Mumbai Indians",
  "Brabourne Stadium, Mumbai" = "Mumbai Indians",
  "Sardar Patel Stadium, Motera" = "Rajasthan Royals",
  "Barabati Stadium" = "Deccan Chargers",
  "Vidarbha Cricket Association Stadium, Jamtha" = "Mumbai Indians",
  "Himachal Pradesh Cricket Association Stadium" = "Kings XI Punjab",
  "Kalool Stadium" = "Kochi Tuskers Kerela",
  "Jawaharlal Nehru Stadium, Kochi" = "Kochi Tuskers Kerela",
  "Barabati Stadium" = "Sunrisers Hyderabad",
  "Rajiv Gandhi International Stadium, Uppal" = "Sunrisers Hyderabad",
  "Saurashtra Cricket Association Stadium" = "Gujarat Lions",
  "Green Park" = "Gujarat Lions",
  "Niranjan Shah Stadium." = "Gujarat Lions",
  "Subrata Roy Sahara Stadium" = "Rising Pune Supergiants",
  "Nehru Stadium, Kochi" = "Kochi Tuskers Kerela",
  "Nehru Stadium" = "Kochi Tuskers Kerela",
  "Dr. Y.S. Rajasekhara Reddy ACA-VDCA Cricket Stadium" = "Sunrisers Hyderabad",
  "Holkar Cricket Stadium" = "Kings XI Punjab",
  "Maharashtra Cricket Association Stadium" = "Rising Pune Supergiants",
  "Shaheed Veer Narayan Singh International Stadium" = "Delhi Daredevils",
  "Arun Jaitley Stadium" = "Delhi Capitals",
  "Rajiv Gandhi International Stadium" = "Sunrisers Hyderabad",
  "Punjab Cricket Association IS Bindra Stadium" = "Kings XI Punjab",
  "Punjab Cricket Association IS Bindra Stadium, Mohali" = "Kings XI Punjab",
  "JSCA International Stadium Complex" = "Chennai Super Kings"
)

# 5. Match level data extraction
match_data_list <- map(yaml_files, function(fpath) {
  match_raw <- yaml::read_yaml(fpath)
  info <- match_raw$info
  
  match_id <- basename(fpath) %>%
    str_replace("\\.yaml$", "") %>%
    as.integer()
  
  team1 <- get_or_na(info, "teams", 1)
  team2 <- get_or_na(info, "teams", 2)
  
  city <- get_or_na(info, "city")
  venue <- get_or_na(info, "venue")
  match_date <- get_or_na(info, "dates", 1)
  
  toss_winner <- get_or_na(info, "toss", "winner")
  toss_decision <- get_or_na(info, "toss", "decision")
  match_winner <- get_or_na(info, "outcome", "winner")
  
  outcome_by_list <- get_or_na(info, "outcome", "by")
  
  result_type <- NA_character_
  result_margin <- NA_real_
  
  if (!all(is.na(outcome_by_list))) {
    by_names <- names(outcome_by_list)
    if (length(by_names) > 0) {
      result_type <- by_names[1]
      result_margin <- suppressWarnings(as.numeric(outcome_by_list[[1]]))
    }
  }
  
  home_team <- ifelse(
    venue %in% names(home_team_map),
    home_team_map[venue],
    NA_character_
  )
  
  season <- if (!is.na(match_date)) {
    as.integer(format(as.Date(match_date), "%Y"))
  } else {
    NA_integer_
  }
  
  tibble(
    match_id = match_id,
    date = as.Date(match_date),
    season = season,
    city = city,
    venue = venue,
    team1 = team1,
    team2 = team2,
    toss_winner = toss_winner,
    toss_decision = toss_decision,
    match_winner = match_winner,
    result_type = result_type,
    result_margin = result_margin,
    home_team = home_team
  )
})

match_data <- bind_rows(match_data_list) %>%
  arrange(season, date, match_id)

# Apply name normalisation to match_data
match_data <- match_data %>%
  mutate(across(c(team1, team2, toss_winner, match_winner, home_team),
                ~ifelse(. %in% names(name_fixes), name_fixes[.], .)))

# 6. Ball-by-ball data extraction
ball_by_ball_list <- map(yaml_files, function(fpath) {
  match_raw <- yaml::read_yaml(fpath)
  info <- match_raw$info
  
  match_id <- basename(fpath) %>%
    str_replace("\\.yaml$", "") %>%
    as.integer()
  
  match_date <- get_or_na(info, "dates", 1)
  
  season <- if (!is.na(match_date)) {
    as.integer(format(as.Date(match_date), "%Y"))
  } else {
    NA_integer_
  }
  
  innings_list <- match_raw$innings
  
  map2_dfr(
    innings_list,
    seq_along(innings_list),
    function(inn_wrapper, inn_number) {
      innings_label <- names(inn_wrapper)[1]
      innings_obj <- inn_wrapper[[1]]
      
      batting_team <- get_or_na(innings_obj, "team")
      deliveries <- innings_obj$deliveries
      
      map_dfr(deliveries, function(deliv) {
        ball_name <- names(deliv)[1]
        ball_info <- deliv[[1]]
        
        over_ball <- str_split(ball_name, "\\.", n = 2)[[1]]
        over_num <- as.integer(over_ball[1])
        ball_in_over <- as.integer(over_ball[2])
        
        striker <- get_or_na(ball_info, "batsman")
        non_striker <- get_or_na(ball_info, "non_striker")
        bowler <- get_or_na(ball_info, "bowler")
        
        runs_batsman <- get_or_na(ball_info, "runs", "batsman")
        runs_extras <- get_or_na(ball_info, "runs", "extras")
        runs_total <- get_or_na(ball_info, "runs", "total")
        
        extras_list <- get_or_na(ball_info, "extras")
        extras_type <- "0"
        extras_runs <- 0L
        
        if (!all(is.na(extras_list))) {
          extras_type <- names(extras_list)[1]
          extras_runs <- suppressWarnings(as.integer(extras_list[[1]]))
        }
        
        wicket_player_out <- "0"
        wicket_kind <- "0"
        
        if (!is.null(ball_info$wicket)) {
          wicket_player_out <- get_or_na(ball_info, "wicket", "player_out")
          wicket_kind <- get_or_na(ball_info, "wicket", "kind")
        }
        
        tibble(
          match_id = match_id,
          season = season,
          innings = inn_number,
          innings_label = innings_label,
          batting_team = batting_team,
          over = over_num,
          ball_in_over = ball_in_over,
          striker = striker,
          non_striker = non_striker,
          bowler = bowler,
          runs_batsman = as.integer(runs_batsman),
          runs_extras = as.integer(runs_extras),
          runs_total = as.integer(runs_total),
          extras_type = extras_type,
          extras_runs = as.integer(extras_runs),
          wicket_player_out = wicket_player_out,
          wicket_kind = wicket_kind
        )
      })
    }
  )
})

ball_by_ball_data <- bind_rows(ball_by_ball_list) %>%
  arrange(match_id, innings, over, ball_in_over)

# Apply name normalisation to ball_by_ball_data
ball_by_ball_data <- ball_by_ball_data %>%
  mutate(across(c(batting_team),
                ~ifelse(. %in% names(name_fixes), name_fixes[.], .)))

# 7. Output files
match_out_path <- file.path(base_dir, "01_match_data_2008_2010_to_2019.csv")
ball_by_ball_path <- file.path(base_dir, "01_ball_by_ball_data_2008_2010_to_2019.csv")

write_csv(match_data, match_out_path)
write_csv(ball_by_ball_data, ball_by_ball_path)

cat("Match-level dataset written to: ", match_out_path, "\n")
cat("Ball-by-ball dataset written to: ", ball_by_ball_path, "\n")

cat("Number of matches extracted: ", nrow(match_data), "\n")
cat("Number of ball-by-ball rows extracted: ", nrow(ball_by_ball_data), "\n")
