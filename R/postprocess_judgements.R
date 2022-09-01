#' @title
#' Post-processing.
#'
#' @description
#' Standardise the output from aggregation method's.
#' This function is called by every aggregation method as a final step.
#'
#' @param method_output tibble created from one of the aggregation methods after
#' pre-processing' with columns for the aggregation `method`, `paper_id`, `aggregated_judgement` and
#' `n_experts`
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`, corresponding
#' with an aggregation `method` (character).
#'
#' @export

postprocess_judgements <- function(method_output) {
  method_output %>%
    dplyr::select(method,
                  paper_id,
                  aggregated_judgement,
                  n_experts) %>%
    dplyr::rename(
      cs = aggregated_judgement
    )

}

