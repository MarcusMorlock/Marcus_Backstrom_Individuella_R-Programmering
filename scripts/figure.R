library(scales)
library(tidyverse)

# #dont work for me.
# save_images <- function(plot, file_name) {
#   if (!grepl("\\.png$", file_name)){
#     file_name <- paste0(file_name, ".png")
#   }
#   
#   path <- file.path("..", "save", "images", file_name)
#   
#   ggsave(plot = plot, filename = path)
#   
# }

col_age_group_by_charge <- function(df) {
  col_ageGroup_by_charge <- ggplot(df, aes(x = age_group, y = charges)) +
    geom_col() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Charge by Age Group",
      x = "Age Groups",
      y = "Charged"
    )
    return(col_ageGroup_by_charge)

}

smoker_boxplot <- function(df) {
  df %>% 
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
    
}

