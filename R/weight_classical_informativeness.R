expert_judgements <- data_ratings %>%
  mutate(value = value / 100)



weight_classical_informativeness <- function(expert_judgements,
                                             quantiles = c(0, 0.05, 0.5, 0.95, 1),
                                             question_name = "direct_replication",
                                             buffer = 0.1){
  
  ## Expected distribution
  
  expected_bin_proportions <- diff(quantiles)
  
  ## Expert informativeness
  
  ### Overall minimum/maximum bounds to establish the uniform distribution
  
  unif_dist <- expert_judgements %>%
    filter(question == !!question_name) %>%
    pivot_wider(names_from = element,
                values_from = value) %>%
    select(paper_id,
           three_point_lower,
           three_point_upper) %>%
    group_by(paper_id) %>%
    summarise(L_star = min(three_point_lower),
              U_star = max(three_point_upper)) %>%
    mutate(L_star = L_star - (L_star * buffer),
           U_star = U_star + (U_star * buffer))
  
  ### Informativeness calculation
  
  informativeness <- expert_judgements %>%
    filter(question == !!question_name) %>%
    pivot_wider(names_from = element,
                values_from = value) %>%
    left_join(unif_dist,
              by = "paper_id") %>%
    rowwise() %>%
    mutate(three_point_lower = ifelse(three_point_lower == three_point_best,
                                      three_point_lower - .Machine$double.eps,
                                      three_point_lower),
           three_point_upper = ifelse(three_point_upper == three_point_best,
                                      three_point_upper + .Machine$double.eps,
                                      three_point_upper),
           informativeness = sum(expected_bin_proportions[1] *
                                   log(expected_bin_proportions[1] / 
                                         (three_point_lower - L_star)),
                                 expected_bin_proportions[2] *
                                   log(expected_bin_proportions[2] / 
                                         (three_point_best - three_point_lower)),
                                 expected_bin_proportions[3] *
                                   log(expected_bin_proportions[3] / 
                                         (three_point_upper - three_point_best)),
                                 expected_bin_proportions[4] *
                                   log(expected_bin_proportions[4] / 
                                         (U_star - three_point_upper)),
                                 log(U_star - L_star))) %>%
    select(user_name,
           informativeness) %>%
    group_by(user_name) %>%
    summarise(informativeness = mean(informativeness))
  
  return(informativeness)
  
}