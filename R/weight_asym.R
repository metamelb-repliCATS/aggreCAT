#' @title
#' Weighting method: Asymmetry of intervals
#'
#' @description
#' Calculates weights by asymmetry of intervals
#'
#' @details
#' This function is used inside [IntervalWAgg] to calculate the weights for the
#' aggregation type `"AsymWAgg"`, `"IndIntAsymWAgg"` and `"KitchSinkWAgg"`. Pre-processed
#' expert judgements (long format) are first converted to wide format then weighted by:
#' \loadmathjax
#' \mjdeqn{w\_asym_{i,c}= \begin{cases}
#' 1 - 2 \cdot \frac{U_{i,c}-B_{i,c}}{U_{i,c}-L_{i,c}}, \text{for}\ B_{i,c} \geq
#' \frac{U_{i,c}-L_{i,c}}{2}+L_{i,c}\cr
#' 1 - 2 \cdot \frac{B_{i,c}-L_{i,c}}{U_{i,c}-L_{i,c}}, \text{otherwise}
#' \end{cases}}{ascii}
#'
#' Data is converted back to long format, with only the weighted best estimates
#' retained.
#'
#' @param expert_judgements the long tibble exported from the `preprocess_judgements` function.
#'
#' @return A tibble in the form of the input `expert_judgements` argument with additional columns 
#' supplying the calculated weight for each row's observation.
#'
#' @export
#'
#' @examples
#' weight_asym(preprocess_judgements(data_ratings))

weight_asym <- function(expert_judgements) {

    expert_judgements %>%
        tidyr::pivot_wider(names_from = element, values_from = value) %>%
        dplyr::mutate(
            ul = three_point_upper - three_point_lower,
            ul = dplyr::if_else(ul == 0,
                                .Machine$double.eps,
                                ul),
            weight_obs =
                dplyr::if_else(
                    three_point_best >= ((ul / 2) + three_point_lower),
                    1 - 2 * (three_point_upper - three_point_best) / ul,
                    1 - 2 * (three_point_best - three_point_lower) / ul
                ),


            agg_weight = dplyr::if_else(
                weight_obs < 0,
                0,
                weight_obs
            )

        )  %>%
        tidyr::pivot_longer(
            c(three_point_lower,
              three_point_best,
              three_point_upper),
            names_to = "element",
            values_to = "value"
        ) %>%
        dplyr::filter(element == "three_point_best")

}
