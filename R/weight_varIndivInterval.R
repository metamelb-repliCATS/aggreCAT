#' @title
#' Weighting method: Variation in individuals’ interval widths
#'
#' @description
#' Calculates weights based on the variability of interval widths
#' within individuals.
#'
#' @details
#' This function is used inside [IntervalWAgg] for aggregation types `"VarIndIntWAgg"`
#' and `"KitchSinkWAgg"`. It calculates the difference between individual's upper and
#' lower estimates, then calculates the variance in this interval across each individual's
#' claim assessments.
#' \loadmathjax
#' \mjdeqn{w\_varIndivInterval_{i} = var\{(U_{i,d}-L_{i,d}):c=1,...,C\}}{ascii}
#'
#' @param expert_judgements A dataframe in the form of [data_ratings]
#'
#' @importFrom stats var
#'
#' @return A tibble in the form of the input `expert_judgements` argument with additional columns 
#' supplying the calculated weight for each row's observation.
#'
#' @export

weight_varIndivInterval <- function(expert_judgements) {
    diff <- expert_judgements %>%
        tidyr::pivot_wider(names_from = element, values_from = value) %>%
        dplyr::mutate(
            diff = abs(three_point_upper - three_point_lower),
            diff = dplyr::if_else(diff == 0,
                                  .Machine$double.eps,
                                  diff))

    diff %>%
        dplyr::group_by(user_name) %>%
        dplyr::summarise(agg_var = var(diff, na.rm = TRUE)) %>%
        dplyr::left_join(expert_judgements, by = "user_name") %>%
        dplyr::mutate(
            agg_weight = dplyr::if_else(
                !is.na(agg_var),
                agg_var,
                0)
        )

    }
