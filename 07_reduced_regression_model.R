library(dplyr)
library(readr)
library(broom)
library(car)
library(tibble)
library(ggplot2)
library(tidyr)

# 1. File path
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"
model_data_path <- file.path(base_dir, "05_model_dataset.csv")

# 2. Read modelling dataset
model_data <- read_csv(model_data_path, show_col_types = FALSE)

# 3
model_data_clean <- model_data %>%
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

# Graph: Correlation heatmap for reduced predictors
cor_df <- as.data.frame(cor_matrix_reduced) %>%
  tibble::rownames_to_column(var = "Var1") %>%
  tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

var_labels <- c(
  "win_pct_before"      = "Win % Before",
  "prev_season_win_pct" = "Prev Season Win %",
  "won_toss"            = "Won Toss",
  "toss_winner_at_home" = "Toss Winner at Home",
  "batting_first"       = "Batting First",
  "is_home"             = "Home"
)

cor_df <- cor_df %>%
  mutate(
    Var1 = var_labels[Var1],
    Var2 = var_labels[Var2],
    Var1 = factor(Var1, levels = unname(var_labels)),
    Var2 = factor(Var2, levels = rev(unname(var_labels)))
  )

ggplot(cor_df, aes(x = Var1, y = Var2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(correlation, 2)),
            size = 3.8, fontface = "bold",
            color = ifelse(abs(cor_df$correlation) > 0.4, "white", "#1a1a1a")) +
  scale_fill_gradient2(
    low = "white",
    mid = "#74c476",
    high = "#00441b",
    midpoint = 0.25,
    limits = c(-0.5, 1),
    name = "Correlation"
  ) +
  labs(
    title = "Correlation Matrix of Reduced Model Predictors",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid = element_blank()
  )

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

# Graph: Coefficient plot with confidence intervals for both reduced models
coef_plot_data <- bind_rows(
  coef_reduced_model1 %>% mutate(model = "Model 1: Pre-match Only"),
  coef_reduced_model2 %>% mutate(model = "Model 2: Pre-match + Toss")
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    ci_lower = estimate - 1.96 * std.error,
    ci_upper = estimate + 1.96 * std.error,
    term = case_when(
      term == "win_pct_before"      ~ "Win % Before",
      term == "prev_season_win_pct" ~ "Prev Season Win %",
      term == "is_home"             ~ "Home Advantage",
      term == "won_toss"            ~ "Won Toss",
      term == "toss_winner_at_home" ~ "Toss Winner at Home",
      term == "batting_first"       ~ "Batting First",
      TRUE ~ term
    ),
    term = factor(term, levels = c(
      "Batting First",
      "Toss Winner at Home",
      "Won Toss",
      "Home Advantage",
      "Prev Season Win %",
      "Win % Before"
    )),
    model = factor(model, levels = c(
      "Model 1: Pre-match Only",
      "Model 2: Pre-match + Toss"
    ))
  )

ggplot(coef_plot_data, aes(x = estimate, y = term, color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "#aaaaaa", linewidth = 0.9) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 height = 0.25, linewidth = 0.9,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.5)) +
  scale_color_manual(
    values = c(
      "Model 1: Pre-match Only"    = "#238b45",
      "Model 2: Pre-match + Toss"  = "#00441b"
    ),
    name = ""
  ) +
  scale_shape_manual(
    values = c(
      "Model 1: Pre-match Only"    = 16,
      "Model 2: Pre-match + Toss"  = 17
    ),
    name = ""
  ) +
  labs(
    title = "Coefficient Estimates with 95% Confidence Intervals",
    subtitle = "Reduced Model 1 vs Reduced Model 2",
    x = "Coefficient Estimate (Change in Win Probability)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#555555"),
    axis.text.y   = element_text(size = 10),
    axis.text.x   = element_text(size = 10),
    axis.title.x  = element_text(size = 11),
    legend.position = "bottom",
    legend.text   = element_text(size = 10),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    panel.grid.major.x = element_line(color = "#eeeeee", linetype = "dotted")
  )

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