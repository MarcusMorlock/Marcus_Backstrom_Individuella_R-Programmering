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