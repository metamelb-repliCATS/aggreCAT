library(aggreCAT)
library(tidyverse)

expert_judgements <- data_ratings %>%
  mutate(value = value / 100)
known_outcomes <- data_outcomes %>%
  rowwise() %>%
  mutate(outcome = if_else(outcome == 1,
                           runif(1, 0.5, 0.99),
                           runif(1, 0.01, 0.49)))

weight_classical_calibration <- function(expert_judgements,
                                         known_outcomes,
                                         quantiles = c(0, 0.05, 0.5, 0.95, 1),
                                         question_name = "direct_replication") {
  
  ## Establish bins
  
  ### Expected distribution
  
  expected_bin_proportions <- diff(quantiles)
  
  ### Real distributions
  
  real_bin_proportions <- expert_judgements %>%
    filter(question == !!question_name) %>%
    pivot_wider(names_from = element,
                values_from = value) %>%
    left_join(known_outcomes,
              by = "paper_id") %>%
    mutate(bin_1 = outcome <= three_point_lower,
           bin_2 = three_point_lower <= outcome & three_point_best > outcome,
           bin_3 = three_point_best <= outcome & three_point_upper > outcome,
           bin_4 = three_point_upper <= outcome) %>%
    select(user_name,
           starts_with("bin_")) %>%
    group_by(user_name) %>%
    summarise(real_bin_proportion_1 = mean(bin_1),
              real_bin_proportion_2 = mean(bin_2),
              real_bin_proportion_3 = mean(bin_3),
              real_bin_proportion_4 = mean(bin_4),
              n = n())
  
  ## Accuracy calculation
  
  accuracy <- real_bin_proportions %>%
    rowwise() %>%
    mutate(kullback_leibler = sum(real_bin_proportion_1 * 
                                    log(real_bin_proportion_1 / expected_bin_proportions[1]),
                                  real_bin_proportion_2 * 
                                    log(real_bin_proportion_2 / expected_bin_proportions[2]),
                                  real_bin_proportion_3 * 
                                    log(real_bin_proportion_3 / expected_bin_proportions[3]),
                                  real_bin_proportion_4 * 
                                    log(real_bin_proportion_4 / expected_bin_proportions[4])),
           calibration = 1 - pchisq(2 * n * kullback_leibler,
                                    df = 3))
  
  ## Weight
  
  weight <- accuracy %>%
    select(user_name,
           calibration)
  
  return(weight)
  
}
