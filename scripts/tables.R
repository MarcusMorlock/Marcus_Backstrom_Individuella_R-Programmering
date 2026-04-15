
health_risk_level_charges_summarize <- function(df) {
  df %>% 
    group_by(health_risk_level) %>% 
    summarise(
      avg_charges = mean(charges, na.rm = TRUE),
      median_charges = median(charges, na.rm = TRUE),
      min_charges = min(charges, na.rm = TRUE),
      max_charges = max(charges, na.rm = TRUE),
      n = n()
    )
}