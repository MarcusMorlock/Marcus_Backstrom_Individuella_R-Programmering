
#Clean the data and stream line it. Also creates age_group category putting them into 4 groups.
clean_data <- function(df) {
  df_clean <- df %>% 
    mutate(
      customer_id = str_replace(customer_id, "C", ""),
      customer_id = as.integer(customer_id),
      age = as.integer(age),
      children = as.integer(children),
      sex = str_trim(sex),
      sex = str_to_lower(sex),
      sex = as.factor(sex),
      region = str_trim(region),
      region = str_to_lower(region),
      region = as.factor(region),
      smoker = str_trim(smoker),
      smoker = str_to_lower(smoker),
      smoker = if_else(smoker == "yes", TRUE, FALSE, missing = NA),
      chronic_condition = str_trim(chronic_condition), 
      chronic_condition = str_to_lower(chronic_condition),
      chronic_condition = if_else(chronic_condition == "yes", TRUE, FALSE, missing = NA),
      exercise_level = str_trim(exercise_level),
      exercise_level = str_to_lower(exercise_level),
      exercise_level = if_else(is.na(exercise_level), "medium", exercise_level),
      exercise_level = as.factor(exercise_level),
      plan_type = str_trim(plan_type),
      plan_type = str_to_lower(plan_type),
      plan_type = as.factor(plan_type),
      prior_accidents = as.integer(prior_accidents, na.rm = TRUE),
      prior_claims = as.integer(prior_claims, na.rm = TRUE),
      annual_checkups = if_else(is.na(annual_checkups), median(annual_checkups, na.rm = TRUE), annual_checkups),
      annual_checkups = as.integer(annual_checkups, na.rm = TRUE),
       age_group = case_when(
        age < 30 ~ "under 30",
        age < 45 ~ "30-44",
        age < 60 ~ "45-59",
        TRUE ~ "60 and above"
      ),
      age_group = factor(age_group,levels = c("under 30", "30-44", "45-59", "60 and above")),
      
    ) %>% 
    group_by(sex, age_group,exercise_level) %>% 
    mutate(bmi = if_else(is.na(bmi), median(bmi, na.rm = TRUE), bmi)) %>% 
    ungroup()
  
  glimpse(df_clean)
  print(colSums(is.na(df_clean)))
  return(df_clean)
}























# 
# clean_data <- function(df) {
#   df_clean <- df %>% 
#     mutate(
#       customer_id = str_replace(customer_id, "C", ""),
#       customer_id = as.integer(customer_id),
#       age = as.integer(age),
#       children = as.integer(children),
#       sex = str_to_lower(sex),
#       sex = as.factor(sex),
#       region = str_to_lower(region),
#       region = as.factor(region),
#       smoker = str_to_lower(smoker),
#       smoker = if_else(smoker == "yes", TRUE, FALSE, missing = NA),
#       chronic_condition = str_to_lower(chronic_condition),
#       chronic_condition = if_else(chronic_condition == "yes", TRUE, FALSE, missing = NA),
#       exercise_level = if_else(is.na(exercise_level), "medium", exercise_level),
#       exercise_level = str_to_lower(exercise_level),
#       exercise_level = as.factor(exercise_level),
#       plan_type = str_to_lower(plan_type),
#       plan_type = as.factor(plan_type),
#       prior_accidents = as.integer(prior_accidents, na.rm = TRUE),
#       prior_claims = as.integer(prior_claims, na.rm = TRUE),
#       annual_checkups = if_else(is.na(annual_checkups), median(annual_checkups, na.rm = TRUE), annual_checkups),
#       annual_checkups = as.integer(annual_checkups, na.rm = TRUE),
#       age_group = case_when(
#         age < 30 ~ "under 30",
#         age < 45 ~ "30-44",
#         age < 60 ~ "45-59",
#         TRUE ~ "60 and above"
#       ),
#       age_group = factor(age_group,levels = c("under 30", "30-44", "45-59", "60 and above")),
#       
#     ) %>% 
#     group_by(sex, age_group,exercise_level) %>% 
#     mutate(bmi = if_else(is.na(bmi), median(bmi, na.rm = TRUE), bmi)) %>% 
#     ungroup()
#   
#   glimpse(df_clean)
#   print(colSums(is.na(df_clean)))
#   return(df_clean)
# }