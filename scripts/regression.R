
# removes the self made categories and customer id and return the r squared and adj r squared for each variable in data frame.

r2_adjr2_per_variable <- function(df) {
  predicts <- setdiff(names(df), c("customer_id","charges", "age_group", "bmi_category", "health_risk_level"))
  
  results <- tibble(
    Variabel = predicts,
    R_Squared = NA_real_,
    Adj_R_Squared = NA_real_
  )
  
  for (i in 1:length(predicts)) {
    col <- predicts[i]
    model_test <- lm(reformulate(col, "charges"), data = df)
    
    results$R_Squared[i] <- summary(model_test)$r.squared
    results$Adj_R_Squared[i] <- summary(model_test)$adj.r.squared
    
    
  }
  
  
  results <- results[order(-results$R_Squared), ]
  
  for (i in 1:nrow(results)) {
    cat(sprintf("%-18s : R2 = %.4f    Adj.R2 = %.4f\n",
                results$Variabel[i], results$R_Squared[i], results$Adj_R_Squared[i]))
  }
  
  invisible(results)
  
}

# make a quick summery with multiple variables with charges as default target.
model_and_summary <- function(df, target = "charges", choice = "") {
  choice <- str_trim(choice, side = "both")
  model_test_summary <- lm(reformulate(choice, target), data = df)
  summary(model_test_summary)
}



