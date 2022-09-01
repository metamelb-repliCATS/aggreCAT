# experience weight calculation 1
# log transformed number of claims assessed by participant

weight_experience <- function(expert_judgements,
                              prior_expert_judgements = NULL,
                              round_for_agg) {

  if(is.null(prior_expert_judgements)){
    expert_judgements_pp <- expert_judgements %>%
      dplyr::filter(element == "three_point_best")
  } else {
    expert_judgements_pp <- expert_judgements %>%
      dplyr::bind_rows(prior_expert_judgements) %>%
      dplyr::filter(element == "three_point_best")
  }

  judgement_times <- expert_judgements_pp %>%
    dplyr::select(paper_id, user_name, round, timestamp)

  weights <- judgement_times %>%
    dplyr::group_by(user_name, round) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(claim_count = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-timestamp) %>%
    dplyr::filter(round == round_for_agg) %>%
    dplyr::mutate(weight = log(claim_count) + 1) %>%
    dplyr::select(paper_id, user_name, weight)

  return(weights)

}



