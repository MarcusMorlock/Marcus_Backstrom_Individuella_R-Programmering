source("scripts/figure.R")
# did not work when i tried as i wanted.
create_and_save_images <- function(df) {
  figure_1 <- col_age_group_by_charge(df)
  save_images(figure_1, "col_by_age_group_of_charge")
}
