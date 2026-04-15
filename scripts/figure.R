library(scales)
library(tidyverse)

col_age_group_by_charge <- function(df) {
  col_ageGroup_by_charge <- ggplot(df, aes(x = age_group, y = charges)) +
    geom_col() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Charge by Age Group",
      x = "Age Groups",
      y = "Charged"
    )
  
}
