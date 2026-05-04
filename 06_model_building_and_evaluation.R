library(dplyr)
library(readr)
library(broom)
library(car)

# 1. File path
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
model_data_path <- file.path(base_dir, "05_model_dataset.csv")

# 2. Read modelling dataset
model_data <- read_csv(model_data_path, show_col_types = FALSE)

# 3. Keep 2010 rows only and require at least 3 previous matches
model_data_clean <- model_data %>%
  filter(season == 2010) %>%
  filter(matches_before >= 3)

# 4. Keep only columns required for modelling
model_data_clean <- model_data_clean %>%
  select(
    match_id, date, season, team, opponent, won_match,
    matches_before, wins_before, win_pct_before,
    home_win_pct_before, away_win_pct_before,
    prev_season_win_pct, prev_home_win_pct, prev_away_win_pct,
    prev_bat_first_win_pct, last_3_wins,
    won_toss, toss_winner_at_home, batting_first, is_home
  )

# 5. Replace any remaining NA values in predictors with column means
predictor_cols <- c(
  "matches_before", "wins_before", "win_pct_before",
  "home_win_pct_before", "away_win_pct_before",
  "prev_season_win_pct", "prev_home_win_pct", "prev_away_win_pct",
  "prev_bat_first_win_pct", "last_3_wins",
  "won_toss", "toss_winner_at_home", "batting_first", "is_home"
)

for (col in predictor_cols) {
  model_data_clean[[col]][is.na(model_data_clean[[col]])] <-
    mean(model_data_clean[[col]], na.rm = TRUE)
}

# 6. Final NA check
cat("Rows available for modelling: ", nrow(model_data_clean), "\n")
cat("Matches available for modelling: ", n_distinct(model_data_clean$match_id), "\n")
cat("NA count after cleaning:\n")
print(colSums(is.na(model_data_clean)))

# 7. Correlation matrix for predictors
correlation_data <- model_data_clean %>%
  select(all_of(predictor_cols))

cor_matrix <- cor(correlation_data)

cat("\nCorrelation matrix:\n")
print(round(cor_matrix, 3))

# 8. Fit Model 1: Pre-match only
model1 <- lm(
  won_match ~ matches_before + wins_before + win_pct_before +
    home_win_pct_before + away_win_pct_before +
    prev_season_win_pct + prev_home_win_pct + prev_away_win_pct +
    prev_bat_first_win_pct + last_3_wins + is_home,
  data = model_data_clean
)

cat("\nModel 1 summary:\n")
print(summary(model1))

# 9. Fit Model 2: Pre-match + toss
model2 <- lm(
  won_match ~ matches_before + wins_before + win_pct_before +
    home_win_pct_before + away_win_pct_before +
    prev_season_win_pct + prev_home_win_pct + prev_away_win_pct +
    prev_bat_first_win_pct + last_3_wins + is_home +
    won_toss + toss_winner_at_home + batting_first,
  data = model_data_clean
)

cat("\nModel 2 summary:\n")
print(summary(model2))

# 10. Model comparison
model_comparison <- anova(model1, model2)

cat("\nComparison of nested models:\n")
print(model_comparison)

# 11. VIF for Model 1
vif_model1_values <- vif(model1)
vif_model1 <- tibble(
  term = names(vif_model1_values),
  vif = as.numeric(vif_model1_values)
)

cat("\nVIF - Model 1:\n")
print(vif_model1)

# 12. VIF for Model 2
vif_model2_values <- vif(model2)
vif_model2 <- tibble(
  term = names(vif_model2_values),
  vif = as.numeric(vif_model2_values)
)

cat("\nVIF - Model 2:\n")
print(vif_model2)

# 13. Convert correlation matrix to CSV-ready format
correlation_output <- as.data.frame(cor_matrix)
correlation_output <- tibble::rownames_to_column(correlation_output, var = "variable")

# 14. Extract coefficients
coef_model1 <- tidy(model1)
coef_model2 <- tidy(model2)

# 15. Output file paths
coef1_out_path <- file.path(base_dir, "06_model1_pre_match_coefficients.csv")
coef2_out_path <- file.path(base_dir, "06_model2_pre_match_toss_coefficients.csv")
comparison_out_path <- file.path(base_dir, "06_model_comparison.csv")
clean_model_out_path <- file.path(base_dir, "06_model_dataset_for_fitting.csv")
correlation_out_path <- file.path(base_dir, "06_predictor_correlation_matrix.csv")
vif1_out_path <- file.path(base_dir, "06_model1_vif.csv")
vif2_out_path <- file.path(base_dir, "06_model2_vif.csv")

# 16. Write outputs
write_csv(coef_model1, coef1_out_path)
write_csv(coef_model2, coef2_out_path)
write_csv(as.data.frame(model_comparison), comparison_out_path)
write_csv(model_data_clean, clean_model_out_path)
write_csv(correlation_output, correlation_out_path)
write_csv(vif_model1, vif1_out_path)
write_csv(vif_model2, vif2_out_path)

# 17. Final messages
cat("\nModel 1 coefficients written to: ", coef1_out_path, "\n", sep = "")
cat("Model 2 coefficients written to: ", coef2_out_path, "\n", sep = "")
cat("Model comparison written to: ", comparison_out_path, "\n", sep = "")
cat("Clean modelling dataset written to: ", clean_model_out_path, "\n", sep = "")
cat("Predictor correlation matrix written to: ", correlation_out_path, "\n", sep = "")
cat("Model 1 VIF written to: ", vif1_out_path, "\n", sep = "")
cat("Model 2 VIF written to: ", vif2_out_path, "\n", sep = "")

# 18. Simple interpretation help
cat("Correlation:\n")
cat("- Values close to 1 or -1 indicate strong relationship between predictors.\n")
cat("- Values close to 0 indicate weak relationship.\n\n")

cat("VIF:\n")
cat("- VIF = 1 means no multicollinearity.\n")
cat("- VIF between 1 and 5 is usually acceptable.\n")
cat("- VIF above 5 suggests moderate concern.\n")
cat("- VIF above 10 suggests serious multicollinearity.\n")