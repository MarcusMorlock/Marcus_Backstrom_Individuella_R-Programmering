setwd("path/to/Marcus_Individuella_R-Programmering")

source("scripts/load_data.R")
df_raw <- load_data("data/insurance_costs.csv")

source("scripts/utility.R")

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
  
  
  
  
  
  
  
  
  
  
  
  