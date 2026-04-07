library(tidyverse)
df <- read_csv("data/insurance_costs.csv")

glimpse(df)
df_clean <- df %>% 
  mutate(
    customer_id = str_replace(customer_id, "C", ""),
    customer_id = as.integer(customer_id),
    age = as.integer(age),
    children = as.integer(children)
  )

glimpse(df_clean)

for (col in names(df_clean)) {
  if (is.character(df_clean[[col]])) {
    print(unique(df_clean[[col]]))
  }
}
