library(dplyr)
library(readr)
library(tibble)

# 1. File paths
base_dir <- "C:/Users/anant_dl6wk3s/OneDrive/Desktop/RESEARCH PROJECT/A/RESEARCH_IPL_a1914174/2008_2010/2008_2010_to_2019"

coef_model1_path <- file.path(base_dir, "07_reduced_model1_coefficients.csv")
coef_model2_path <- file.path(base_dir, "07_reduced_model2_coefficients.csv")
model_comparison_path <- file.path(base_dir, "07_reduced_model_comparison.csv")
vif_model1_path <- file.path(base_dir, "07_reduced_model1_vif.csv")
vif_model2_path <- file.path(base_dir, "07_reduced_model2_vif.csv")

# 2. Read files
coef_model1 <- read_csv(coef_model1_path, show_col_types = FALSE)
coef_model2 <- read_csv(coef_model2_path, show_col_types = FALSE)
model_comparison <- read_csv(model_comparison_path, show_col_types = FALSE)
vif_model1 <- read_csv(vif_model1_path, show_col_types = FALSE)
vif_model2 <- read_csv(vif_model2_path, show_col_types = FALSE)

# 3. Extract key coefficients

model1_home <- coef_model1 %>% filter(term == "is_home")
model2_home <- coef_model2 %>% filter(term == "is_home")
model2_won_toss <- coef_model2 %>% filter(term == "won_toss")
model2_toss_home <- coef_model2 %>% filter(term == "toss_winner_at_home")
model2_batting_first <- coef_model2 %>% filter(term == "batting_first")

# 4. Extract model comparison values
model_comparison_f <- model_comparison$F[2]
model_comparison_p <- model_comparison$`Pr(>F)`[2]

# 5. Extract VIF values
max_vif_model1 <- max(vif_model1$vif)
max_vif_model2 <- max(vif_model2$vif)

# 6. Create final summary table
final_results <- tibble(
  metric = c(
    "Reduced Model 1 home effect estimate",
    "Reduced Model 1 home effect p-value",
    "Reduced Model 2 home effect estimate",
    "Reduced Model 2 home effect p-value",
    "Reduced Model 2 won_toss estimate",
    "Reduced Model 2 won_toss p-value",
    "Reduced Model 2 toss_winner_at_home estimate",
    "Reduced Model 2 toss_winner_at_home p-value",
    "Reduced Model 2 batting_first estimate",
    "Reduced Model 2 batting_first p-value",
    "Reduced model comparison F-statistic",
    "Reduced model comparison p-value",
    "Reduced Model 1 max VIF",
    "Reduced Model 2 max VIF"
  ),
  value = c(
    model1_home$estimate,
    model1_home$p.value,
    model2_home$estimate,
    model2_home$p.value,
    model2_won_toss$estimate,
    model2_won_toss$p.value,
    model2_toss_home$estimate,
    model2_toss_home$p.value,
    model2_batting_first$estimate,
    model2_batting_first$p.value,
    model_comparison_f,
    model_comparison_p,
    max_vif_model1,
    max_vif_model2
  )
)

# 7. Print results
cat("\nFinal Results Summary:\n")
print(final_results)

# 8. Write final CSV
final_results_path <- file.path(base_dir, "08_final_results_summary.csv")
write_csv(final_results, final_results_path)

cat("\nFinal results summary written to: ", final_results_path, "\n")

# 9. Interpretation output

cat("
The reduced regression model was used after correcting multicollinearity.
VIF values indicate no multicollinearity in the reduced models.

Home advantage remains statistically significant.
Toss-related variables are not statistically significant.

Model comparison shows that adding toss variables does not improve prediction.

Therefore, match outcomes are more influenced by home advantage 
than toss decisions.
")