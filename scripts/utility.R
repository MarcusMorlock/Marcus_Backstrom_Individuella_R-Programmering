# only look at unique chars with the ability to skip columns
print_unique_chars_only <- function(df, skip_col = "") {
  
  for (col in setdiff(names(df), skip_col) ) {
    
    if (is.character(df[[col]])) {
      print(col)
      print(unique(df[[col]]))
    }
    
  }

}

# Made with help of AI. I wanted to remove the if from the for loop.
print_unique_chars_only_v2 <- function(df, skip_col = "") {
  
  # Behåll endast character eller factor
  df <- df[, sapply(df, function(x) is.character(x) || is.factor(x)), drop = FALSE]
  
  #  Loop över de kvarvarande kolumnerna
  for (col in setdiff(names(df), skip_col) ) {
    print(col)
    print(unique(df[[col]]))
  }
}

# i found myself wanting to use 3 or 4 of these often so i made an funktion to run all 4 at once. 
look_at_data <- function(df, skip_col = "", glimpse = TRUE, summary = TRUE, colSums = TRUE, unique = TRUE) {
  if (glimpse) {
    glimpse(df)
  }
  if (summary) {
    print(summary(df))
  }
  if (colSums) {
    print(colSums(is.na(df)))
  }
  if (unique) {
    print_unique_chars_only_v2(df = df, skip_col = "customer_id")
  }
  
}