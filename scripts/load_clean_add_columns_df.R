source("../scripts/load_data.R")

source("../scripts/clean_data.R")

source("../scripts/add_column.R")

df_raw <- load_data("../data/insurance_costs.csv")

df_clean <- clean_data(df_raw)

df_columns_added <- bmi_category(df_clean)
df_columns_added <- health_risk_level(df_columns_added)

return(df_columns_added)
