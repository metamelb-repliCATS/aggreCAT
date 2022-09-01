#' @title Weighting method: Total number of judgement reasons
#'
#' @description
#' This function is used by [ReasoningWAgg] to calculate weights for the aggregation
#' type `"ReasonWAgg"`. Calculates weights based on the number of judgement reasoning
#' methods used by an individual
#'
#' @details
#' Individuals' weight is equal to the maximum number of judgement reasons given
#'
#' @param expert_reasons A dataframe in the form of [data_supp_ReasonWAgg]
#'
#'
#' @export

weight_reason <- function(expert_reasons) {

  ## Calculate reason counts

  expert_reasons %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("RW"),
                                .fns = ~dplyr::if_else(.x > 1, 1, .x))) %>%
    dplyr::mutate(reason_count = rowSums(dplyr::select(., -paper_id, -user_name)),
                  reason_count = dplyr::na_if(reason_count,
                                              0)) %>%
    dplyr::select(paper_id,
                  user_name,
                  reason_count)

}
