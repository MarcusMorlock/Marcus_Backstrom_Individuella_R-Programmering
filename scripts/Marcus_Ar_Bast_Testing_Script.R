source("scripts/load_data.R")
df_raw <- load_data("data/insurance_costs.csv")

source("scripts/utility.R")

source("scripts/clean_data.R")
df_clean <- clean_data(df_raw)

look_at_data(df = df_clean)





df_add_column <- df_clean %>% 
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
look_at_data(df = df_add_column)

df_add_column <- df_add_column %>%
  mutate(
    health_risk_level = case_when(
      
      #High Risk
      smoker == TRUE | 
      chronic_condition == TRUE |
      bmi_category == "severely obese"|
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

summary(df_add_column)
look_at_data(df_add_column)
