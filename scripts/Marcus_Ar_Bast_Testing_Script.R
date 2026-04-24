
df_fixed <- load_data("data/insurance_costs_cleaned_columns_added.csv")
source("scripts/load_data.R")
df_raw <- load_data("data/insurance_costs.csv")

# source("scripts/utility.R")

source("scripts/clean_data.R")
df_clean <- clean_data(df_raw)

look_at_data(df = df_clean)

source("scripts/add_column.R")

df_add_column <- bmi_category(df_clean)
df_add_column <- health_risk_level(df_add_column)

look_at_data(df_add_column)

library(scales)

ggplot(df_add_column, aes(x = age_group, y = charges)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Charge by Age Group",
    x = "Age Groups",
    y = "Charged"
  )
source("scripts/figure.R")
source("scripts/create_images.R")

col_age_group_by_charge(df_add_column)

# df_add_column %>% 
#   group_by(health_risk_level) %>% 
#   summarise(
#     avg_charges = mean(charges, na.rm = TRUE),
#     median_charges = median(charges, na.rm = TRUE),
#     min_charges = min(charges, na.rm = TRUE),
#     max_charges = max(charges, na.rm = TRUE),
#     n = n()
#   )
#   
  
source("scripts/tables.R")
  
health_risk_level_charges_summarize(df_add_column)  
  

  
look_at_data(df_add_column)
  
  
df_add_column %>% 
  mutate(
    smoker = if_else(smoker == TRUE, "Smoker", "None Smoker")
  ) %>% 
ggplot( aes(x = smoker , y = charges)) +
  geom_boxplot() +
  labs(
    title = "Smokers Charged",
    x = "Smoker",
    y = "Amount Charged"
  )


ggplot(df_add_column, aes(x = exercise_level , y = charges)) +
  geom_boxplot() 



smoker_boxplot(df_add_column)


df_add_column %>% 
  group_by(plan_type) %>% 
  summarise(

    avg_charges = mean(charges, na.rm = TRUE),
    median_charges = median(charges, na.rm = TRUE),
    avg_prior_claims = mean(prior_claims, na.rm = TRUE),
    avg_prior_accidents = mean(prior_accidents, na.rm = TRUE),
    avg_annual_checkups = mean(annual_checkups, na.rm = TRUE),
    n = n()
    
  )

df_add_column %>% 
  select(plan_type, charges)

charged_prior_checkup_table(df_add_column)


# Regression

glimpse(df_add_column)

model_1 <- lm(charges ~ bmi ,data = df_add_column)
model_2 <- lm(charges ~ health_risk_level ,data = df_add_column)
model_3 <- lm(
  charges ~ bmi + smoker + chronic_condition 
  ,data = df_add_column)
model_4 <- lm(
  charges ~ bmi + smoker + chronic_condition + age_group 
  ,data = df_add_column)
model_5 <- lm(
  charges ~ bmi + smoker + chronic_condition + health_risk_level 
  ,data = df_add_column)

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)


#V1
for (col in setdiff(names(df_add_column), c("charges", "age_group", "bmi_category", "health_risk_level") ) ) {
    model_test <- lm(reformulate(col, "charges"), data = df_add_column)
    cat(col, " : ",
        "R Squared :",
        round(summary(model_test)$r.squared, 4), 
        "Adjusted R Squared : ",      
        round(summary(model_test)$adj.r.squared, 4), "\n")
    
}

#V2
for (col in setdiff(names(df_add_column), c("charges", "age_group", "bmi_category", "health_risk_level") ) ) {
  model_test <- lm(reformulate(col, "charges"), data = df_add_column)

  r2 <- round(summary(model_test)$r.squared, 4)

  adj_r2 <- round(summary(model_test)$adj.r.squared, 4)
  
  cat(sprintf("%-18s : R2 = %.4f    Adj.R2 = %.4f\n" , col, r2, adj_r2))
  
}


#V3
source("scripts/regression.R")

regression_tibble <- r2_adjr2_per_variable(df_add_column)

regression_tibble




model_1 <- lm(charges ~ smoker ,data = df_add_column)
model_1_2 <- lm(charges ~ chronic_condition ,data = df_add_column)
model_2 <- lm(charges ~ smoker + chronic_condition ,data = df_add_column)
model_3 <- lm(charges ~ smoker + chronic_condition + age  ,data = df_add_column)
model_4 <- lm(charges ~ smoker + chronic_condition + age + prior_accidents  ,data = df_add_column)
model_5 <- lm(charges ~ smoker + chronic_condition + age + prior_accidents + exercise_level  ,data = df_add_column)

model_6 <- lm(charges ~ smoker + sex ,data = df_add_column)

summary(model_1)
summary(model_1_2)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)
summary(model_6)

glimpse(df_add_column)

# 
# model_and_summary <- function(df, target = "charges", choice = "") {
#   model_test_sum <- lm(reformulate(choice, target), data = df)
#   summary(model_test_sum)
# }

# Top 5:
model_and_summary(df_add_column, choice = c("smoker", "exercise_level", "chronic_condition", "prior_accidents", "age"))

# Swap exercise_level for bmi
model_and_summary(df_add_column, choice = c("smoker", "bmi", "chronic_condition", "prior_accidents", "age" ))

# Add bmi
model_and_summary(df_add_column, choice = c("smoker", "exercise_level", "chronic_condition", "prior_accidents", "age", "bmi" ))

model_and_summary(df_add_column, choice = c("smoker", "chronic_condition", "bmi" ))
model_and_summary(df_add_column, choice = c("smoker", "chronic_condition" ))
model_and_summary(df_add_column, choice = c("smoker", "chronic_condition", "age" ))
model_and_summary(df_add_column, choice = c("smoker", "chronic_condition", "age", "bmi" ))


#Swap age for age_group
model_and_summary(df_add_column, choice = c("smoker", "bmi"))
model_and_summary(df_add_column, choice = c("smoker"))
model_and_summary(df_add_column, choice = c("smoker", "plan_type"))
model_and_summary(df_add_column, choice = c("smoker", "age"))
model_and_summary(df_add_column, choice = c("bmi", "age", "plan_type"))






# All numeric + factor variables except charges and id
all_predictors <- setdiff(names(df_add_column), c("customer_id","charges", "age_group", "bmi_category", "health_risk_level"))

model_all <- lm(reformulate(all_predictors, "charges"), data = df_add_column)

summary(model_all)$r.squared      # This will be high but probably < 1
summary(model_all)$adj.r.squared  # This will likely be lower than regular R²
























