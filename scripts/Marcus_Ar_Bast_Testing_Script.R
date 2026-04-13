source("scripts/load_data.R")
df_raw <- load_data("data/insurance_costs.csv")

source("scripts/utility.R")

source("scripts/clean_data.R")
df_clean <- clean_data(df_raw)

look_at_data(df = df_clean)

source("scripts/add_column.R")

