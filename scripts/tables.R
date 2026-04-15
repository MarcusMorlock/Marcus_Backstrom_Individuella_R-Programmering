
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

charged_prior_checkup_table <- function(df) {
  df %>% 
    mutate(plan_type = factor(plan_type, levels = c("basic", "standard", "premium"))) %>% 
    group_by(plan_type) %>% 
    summarise(
      
      avg_charges = mean(charges, na.rm = TRUE),
      min_charges = min(charges, na.rm = TRUE),
      max_charges = max(charges, na.rm = TRUE),
      median_charges = median(charges, na.rm = TRUE),
      avg_prior_claims = mean(prior_claims, na.rm = TRUE),
      avg_prior_accidents = mean(prior_accidents, na.rm = TRUE),
      avg_annual_checkups = mean(annual_checkups, na.rm = TRUE),
      n = n()
      
    )
}