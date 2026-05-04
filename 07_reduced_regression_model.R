library(dplyr)
library(readr)
library(broom)
library(car)
library(tibble)

# 1. File path
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
model_data_path <- file.path(base_dir, "05_model_dataset.csv")

# 2. Read modelling dataset
model_data <- read_csv(model_data_path, show_col_types = FALSE)

# 3. Keep 2010 rows only and require at least 3 previous matches
model_data_clean <- model_data %>%
  filter(season == 2010) %>%
  filter(matches_before >= 3)

# 4. Keep only reduced set of variables
model_data_clean <- model_data_clean %>%
  select(
    match_id,
    date,
    season,
    team,
    opponent,
    won_match,
    win_pct_before,
    prev_season_win_pct,
    won_toss,
    toss_winner_at_home,
    batting_first,
    is_home
  )

# 5. Replace any remaining NA values in predictors with column means
predictor_cols_reduced <- c(
  "win_pct_before",
  "prev_season_win_pct",
  "won_toss",
  "toss_winner_at_home",
  "batting_first",
  "is_home"
)

for (col in predictor_cols_reduced) {
  model_data_clean[[col]][is.na(model_data_clean[[col]])] <-
    mean(model_data_clean[[col]], na.rm = TRUE)
}

# 6. Final NA check
cat("Rows available for reduced modelling: ", nrow(model_data_clean), "\n")
cat("Matches available for reduced modelling: ", n_distinct(model_data_clean$match_id), "\n")
cat("NA count after cleaning:\n")
print(colSums(is.na(model_data_clean)))

# 7. Correlation matrix for reduced predictors
correlation_data_reduced <- model_data_clean %>%
  select(all_of(predictor_cols_reduced))

cor_matrix_reduced <- cor(correlation_data_reduced)

cat("\nReduced predictor correlation matrix:\n")
print(round(cor_matrix_reduced, 3))

# 8. Fit Reduced Model 1: Pre-match only
reduced_model1 <- lm(
  won_match ~ win_pct_before +
    prev_season_win_pct +
    is_home,
  data = model_data_clean
)

cat("\nReduced Model 1 summary:\n")
print(summary(reduced_model1))

# 9. Fit Reduced Model 2: Pre-match + toss
reduced_model2 <- lm(
  won_match ~ win_pct_before +
    prev_season_win_pct +
    is_home +
    won_toss +
    toss_winner_at_home +
    batting_first,
  data = model_data_clean
)

cat("\nReduced Model 2 summary:\n")
print(summary(reduced_model2))

# 10. Compare reduced models
reduced_model_comparison <- anova(reduced_model1, reduced_model2)

cat("\nComparison of reduced nested models:\n")
print(reduced_model_comparison)

# 11. VIF for Reduced Model 1
vif_reduced_model1_values <- vif(reduced_model1)
vif_reduced_model1 <- tibble(
  term = names(vif_reduced_model1_values),
  vif = as.numeric(vif_reduced_model1_values)
)

cat("\nVIF - Reduced Model 1:\n")
print(vif_reduced_model1)

# 12. VIF for Reduced Model 2
vif_reduced_model2_values <- vif(reduced_model2)
vif_reduced_model2 <- tibble(
  term = names(vif_reduced_model2_values),
  vif = as.numeric(vif_reduced_model2_values)
)

cat("\nVIF - Reduced Model 2:\n")
print(vif_reduced_model2)

# 13. Convert reduced correlation matrix to CSV-ready format
correlation_output_reduced <- as.data.frame(cor_matrix_reduced)
correlation_output_reduced <- rownames_to_column(correlation_output_reduced, var = "variable")

# 14. Extract coefficients
coef_reduced_model1 <- tidy(reduced_model1)
coef_reduced_model2 <- tidy(reduced_model2)

# 15. Output file paths
coef_reduced1_out_path <- file.path(base_dir, "07_reduced_model1_coefficients.csv")
coef_reduced2_out_path <- file.path(base_dir, "07_reduced_model2_coefficients.csv")
comparison_reduced_out_path <- file.path(base_dir, "07_reduced_model_comparison.csv")
clean_reduced_model_out_path <- file.path(base_dir, "07_reduced_model_dataset_for_fitting.csv")
correlation_reduced_out_path <- file.path(base_dir, "07_reduced_predictor_correlation_matrix.csv")
vif_reduced1_out_path <- file.path(base_dir, "07_reduced_model1_vif.csv")
vif_reduced2_out_path <- file.path(base_dir, "07_reduced_model2_vif.csv")

# 16. Write outputs
write_csv(coef_reduced_model1, coef_reduced1_out_path)
write_csv(coef_reduced_model2, coef_reduced2_out_path)
write_csv(as.data.frame(reduced_model_comparison), comparison_reduced_out_path)
write_csv(model_data_clean, clean_reduced_model_out_path)
write_csv(correlation_output_reduced, correlation_reduced_out_path)
write_csv(vif_reduced_model1, vif_reduced1_out_path)
write_csv(vif_reduced_model2, vif_reduced2_out_path)

# 17. Final messages
cat("\nReduced Model 1 coefficients written to: ", coef_reduced1_out_path, "\n", sep = "")
cat("Reduced Model 2 coefficients written to: ", coef_reduced2_out_path, "\n", sep = "")
cat("Reduced model comparison written to: ", comparison_reduced_out_path, "\n", sep = "")
cat("Clean reduced modelling dataset written to: ", clean_reduced_model_out_path, "\n", sep = "")
cat("Reduced predictor correlation matrix written to: ", correlation_reduced_out_path, "\n", sep = "")
cat("Reduced Model 1 VIF written to: ", vif_reduced1_out_path, "\n", sep = "")
cat("Reduced Model 2 VIF written to: ", vif_reduced2_out_path, "\n", sep = "")

# 18. Interpretation guide
cat("This reduced model keeps one main current-form variable, one previous-season variable,\n")
cat("home advantage, and toss-related variables.\n\n")

cat("VIF interpretation:\n")
cat("- VIF = 1 means no multicollinearity.\n")
cat("- VIF between 1 and 5 is usually acceptable.\n")
cat("- VIF above 5 suggests moderate concern.\n")
cat("- VIF above 10 suggests serious multicollinearity.\n\n")

cat("Model comparison interpretation:\n")
cat("- If reduced Model 2 is not significantly better than reduced Model 1,\n")
cat("  toss variables still do not add meaningful explanatory value.\n")