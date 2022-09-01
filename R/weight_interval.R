#' @title
#' Weighting method: Width of intervals
#'
#' @description
#' Calculates weights by interval width
#'
#' @details
#' This function is used inside [IntervalWAgg] for aggregation type `"IntWAgg"`. It
#' calculates the width of each three-point judgement (upper - lower), then returns
#' the weight as the inverse of this interval.
#'
#' @param expert_judgements A dataframe in the form of [data_ratings]
#'
#' @export

weight_interval <- function(expert_judgements) {
        # calculate obs-level weight
  expert_judgements %>%
  tidyr::pivot_wider(names_from = element, values_from = value) %>%
        dplyr::mutate(
            ub = three_point_upper - three_point_lower,
            ub = dplyr::if_else(ub == 0,
                                .Machine$double.eps,
                                ub),
            agg_weight = 1 / ub,
        ) %>%
        dplyr::rename(value = three_point_best)
}
