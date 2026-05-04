R-based analysis of IPL toss decisions and match outcomes (2008–2019). 
Investigates whether winning the toss, batting or fielding first, and 
home ground advantage significantly affect match results. Includes data 
extraction, EDA, feature engineering, and regression modelling scripts.

This project was completed as part of the Data Science Research Project 
at the School of Mathematical Sciences, University of Adelaide.

**Contents:**

1. `01_data_extraction_yaml_to_csv.R` — Extracts raw IPL YAML files into CSV format
2. `02_toss_analysis_eda.R` — Exploratory analysis of toss outcomes
3. `03_team_match_dataset_creation.R` — Builds team level match dataset
4. `04_team_season_summary_creation.R` — Creates season summary per team
5. `05_pre_match_feature_engineering.R` — Builds pre match features
6. `06_model_building_and_evaluation.R` — Full regression models
7. `07_reduced_regression_model.R` — Reduced clean models
8. `08_final_results_summary.R` — Final results summary

**Data source:** Cricsheet (https://cricsheet.org)
