weight_classical_method <- function(expert_judgements,
                                    known_outcomes,
                                    quantiles = c(0, 0.05, 0.5, 0.95, 1),
                                    question_name = "direct_replication",
                                    buffer = 0.1,
                                    alpha = 0){
  
  ## Calibration
  
  weight_calibration <- weight_classical_calibration(expert_judgements,
                                                     known_outcomes,
                                                     quantiles,
                                                     question_name)
  
  ## Informativeness
  
  weight_informativeness <- weight_classical_informativeness(expert_judgements,
                                                             quantiles,
                                                             question_name,
                                                             buffer)
  
  ## Combined weight
  
  weight <- weight_calibration %>%
    left_join(weight_informativeness,
              by = "user_name") %>%
    rowwise() %>%
    mutate(combined_score = calibration * informativeness * as.numeric(calibration > alpha)) %>%
    select(user_name,
           combined_score) %>%
    rename(weight = combined_score)
    
  return(weight)
  
}