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

col_age_group_by_charge <- function(df, save = FALSE) {
  col_ageGroup_by_charge <- ggplot(df, aes(x = age_group, y = charges)) +
    geom_col() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Charge by Age Group",
      x = "Age Groups",
      y = "Charged"
    )
  if (save) {
    ggsave("col_age_group_by_charge.png")
  }
  
    return(col_ageGroup_by_charge)

}


