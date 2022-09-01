#' @title
#' Weighting method: Down weighting outliers
#'
#' @description
#' This method down-weights outliers.
#'
#' @details
#' This function is used by [LinearWAgg] to calculate weights for the aggregation type
#' `"OutWAgg"`. Outliers are given less weight by using the squared difference between the
#' median of an individual's best estimates across all claims and their best estimate
#' for the claim being assessed:
#' \loadmathjax
#' \mjdeqn{d_{i,c} = \left(median\{{B_{i,c}}_{_{i=1,...,N}}\} - B_{i,c}\right)^2}{ascii}
#'
#' Weights are given by 1 minus the proportion of the individual's squared difference
#' relative to the maximum squared difference for the claim across all individuals:
#'
#' \mjdeqn{w\_out_{i} = 1 - \frac{d_{i,c}}{\max({d_c})})}{ascii}
#'
#' @param expert_judgements A dataframe in the form of [data_ratings]
#'
#' @importFrom stats median
#'
#' @export

weight_outlier <- function(expert_judgements) {
    expert_judgements_pp <- expert_judgements %>%
        dplyr::filter(element == "three_point_best")

    expert_judgements_pp_d <- expert_judgements_pp %>%
        # get central tendency of best estimates for claim
        dplyr::group_by(paper_id) %>%
        dplyr::summarise(median_best = median(value,
                                              na.rm = TRUE)) %>%
        dplyr::left_join(expert_judgements_pp, by = "paper_id") %>%
        # calculate difference for individual best estimates
        dplyr::mutate(d = (median_best - value)^2)

    expert_judgements_pp_d %>%
        dplyr::group_by(paper_id) %>%
        # get max difference
        dplyr::summarise(d_max = max(d,
                                     na.rm = TRUE)) %>%

        # adjust max value if 0
        dplyr::mutate(d_max = dplyr::if_else(
            d_max == 0,
            .Machine$double.eps,
            d_max
        )) %>%
        dplyr::left_join(expert_judgements_pp_d, by = "paper_id") %>%
        # calculate weight
        dplyr::mutate(agg_weight = 1 - (d / d_max)) %>%
        dplyr::select(-d,-d_max,-median_best)

}
