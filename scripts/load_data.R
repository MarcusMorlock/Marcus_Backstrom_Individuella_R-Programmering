# Load the data from a csv and take a quick look at the same time.
load_data <- function(csv_path = ""){

  library(tidyverse)
  df <- read_csv(csv_path)
  
  glimpse(df)
  
  print(colSums(is.na(df)))

  return(df)
}
