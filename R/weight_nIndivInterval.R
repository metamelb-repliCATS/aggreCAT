#' @title
#' Weighting method: Individually scaled interval widths
#'
#' @description
#' Weighted by the rescaled interval width within individuals across claims.
#'
#' @details
#' \loadmathjax
#' This function is used inside [IntervalWAgg] for aggregation types `"IndIntWAgg"`,
#' `"IndIntAsymWAgg"` and `"KitchSinkWAgg"`. Interval width weights are rescaled
#' relative to an individuals interval widths across all claims.
#'
#' \mjdeqn{w\_nIndivInterval_{i,c} = \frac{1}{\frac{U_{i,c}-L_{i,c}}{\max\left(\{(U_{i,d}-L_{i,d}):d=1,...,C\}\right)}}}{ascii}
#'
#' @param expert_judgements A dataframe in the form of [data_ratings]
#'
#' @export

weight_nIndivInterval <- function(expert_judgements) {

        expert_judgements %>%
        tidyr::pivot_wider(names_from = element, values_from = value) %>%
        dplyr::group_by(user_name) %>%

        # calculate weight weight
        dplyr::summarise(max_agg =
                             abs(max(three_point_upper - three_point_lower,
                                     na.rm = TRUE))) %>%
        dplyr::full_join(expert_judgements, by = "user_name") %>%
        tidyr::pivot_wider(names_from = element, values_from = value) %>%
        dplyr::mutate(int_agg = abs(three_point_upper - three_point_lower),
                      # check we're not dividing by zero
                      max_agg = dplyr::if_else(max_agg == 0,
                                               .Machine$double.eps,
                                               max_agg),
                      int_agg = dplyr::if_else(int_agg == 0,
                                               .Machine$double.eps,
                                               int_agg),
                      agg_weight = 1 / (int_agg / max_agg)) %>%
                # Inverse of re-scaled interval width)
            tidyr::pivot_longer(
                    c(three_point_upper,
                      three_point_lower,
                      three_point_best),
                    names_to = "element",
                    values_to = "value"
        ) %>%
        dplyr::select(-max_agg, -int_agg) %>%
                dplyr::filter(element == "three_point_best")

}

