bmi_category <- function(df) {
  df_add_bmi_category<- df %>% 
    mutate(
      bmi_category = case_when(
        bmi < 18.5 ~ "under weight",
        bmi < 25 ~ "normal weight",
        bmi < 30 ~ "over weight",
        bmi < 35 ~ "obese",
        TRUE ~ "severely obese"
      ),
      bmi_category = factor(bmi_category, levels = c("under weight","normal weight", "over weight", "obese", "severely obese" ))
    )
  return(df_add_bmi_category)
}

health_risk_level <- function(df) {
  df_add_health_risk_level <- df %>%
    mutate(
      health_risk_level = case_when(
        
        #High Risk
        smoker == TRUE | 
          chronic_condition == TRUE |
          bmi_category == "severely obese"|
          bmi_category == "under weight" |
          (bmi_category == "obese" & exercise_level == "low")  ~ "high risk",
        
        #Medium Risk
        bmi_category == "obese" |
          (bmi_category == "over weight" & exercise_level == "low") |
          (age_group == "45-59" & exercise_level == "low") ~ "medium risk",
        
        #Low Risk
        TRUE ~ "low risk"
      ),
      health_risk_level = factor(health_risk_level, levels = c("low risk","medium risk", "high risk" ))
    )
  return(df_add_health_risk_level)
}

# add_age_group <- function(age) {
#  case_when(
#       age < 30 ~ "under 30",
#       age < 45 ~ "30-44",
#       age < 60 ~ "45-59",
#       TRUE ~ "60 and above"
#     )
# }