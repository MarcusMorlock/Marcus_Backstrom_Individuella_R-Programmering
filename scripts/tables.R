# Create a quick summery table of health risk using charges mean, median, min , max.
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

# create a quick summery of plan_type based on charges, prior_claims, prior_accidents_ annual_checkups.
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