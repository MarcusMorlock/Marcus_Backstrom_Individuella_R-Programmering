load_data <- function(csv_path = "insurance_costs.csv"){
  library(tidyverse)
  df <- read_csv(csv_path)
  glimpse(df)
}

clean_data <- function(df) {
  df_clean <- df %>% 
    mutate(
      customer_id = str_replace(customer_id, "C", ""),
      customer_id = as.integer(customer_id),
      age = as.integer(age),
      children = as.integer(children),
      sex = str_to_lower(sex),
      region = str_to_lower(region),
      smoker = str_to_lower(smoker),
      chronic_condition = str_to_lower(chronic_condition),
      exercise_level = str_to_lower(exercise_level),
      plan_type = str_to_lower(plan_type),
      prior_accidents = as.integer(prior_accidents, na.rm = TRUE),
      prior_claims = as.integer(prior_claims, na.rm = TRUE),
      annual_checkups = as.integer(annual_checkups, na.rm = TRUE)
    )
 glimpse(df_clean)
}

print_unique_chars_only <- function(df, skip_col = "customer_id") {
  
  for (col in setdiff(names(df), skip_col) ) {
    
    if (is.character(df[[col]])) {
      print(col)
      print(unique(df[[col]]))
    }
  }
  colSums(is.na(df))
}
