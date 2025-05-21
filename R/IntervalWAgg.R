#' @title
#' Aggregation Method: IntervalWAgg
#'
#' @description
#' Calculate one of several types of linear-weighted best estimates where the weights
#' are dependent on the lower and upper bounds of three-point elicitation (interval widths).
#'
#' @details
#' The width of the interval provided by individuals may be an indicator of certainty,
#' and arguably of accuracy of the best estimate contained between the bounds of the interval.
#'
#' `type` may be one of the following:
#'
#' **IntWAgg**: Weighted according to the interval width across individuals for that claim, rewarding narrow interval widths.
#' \loadmathjax
#' \mjdeqn{w\_Interval_{i,c}= \frac{1}{U_{i,c} - L_{i,c}}}{ascii}
#' \mjdeqn{\hat{p}_c( IntWAgg) = \sum_{i=1}^N \tilde{w}\_Interval_{i,c}B_{i,c}}{ascii}
#'
#' where \mjeqn{U_{i,d}  - L_{i,d}}{ascii} are individual \mjeqn{i}{ascii}'s judgements for claim \mjeqn{d}{ascii}. Then
#'
#' **IndIntWAgg**: Weighted by the rescaled interval width (interval width relative to largest
#' interval width provided by that individual)
#'
#' Because of the variability in interval widths between individuals across claims,
#' it may be beneficial to account for this individual variability by rescaling interval
#' widths across all claims per individual. This results in a re-scaled interval width weight,
#' for individual \mjeqn{i}{ascii} for claim \mjeqn{c}{ascii}, relative to the widest interval provided by
#' that individual across all claims \mjeqn{C}{ascii}:
#'
#' \mjdeqn{w\_nIndivInterval_{i,c}= \frac{1}{\frac{U_{i,c} - L_{i,c}}{max({ (U_{i,d}
#' - L_{i,d}): d = 1,\dots, C})}}}{ascii}
#'
#' \mjdeqn{\hat{p}_c\left( IndIntWAgg \right) = \sum_{i=1}^N \tilde{w}\_nIndivInterval_{i,c}B_{i,c}}{ascii}
#'
#' **AsymWAgg**: Weighted by the asymmetry of individuals' intervals, rewarding increasing asymmetry.
#'
#' We use the asymmetry of an interval relative to the corresponding best estimate
#' to define the following weights:
#'
#' \mjdeqn{w\_asym_{i,c}= \begin{cases}
#' 1 - 2 \cdot \frac{U_{i,c}-B_{i,c}}{U_{i,c}-L_{i,c}}, \text{for}\ B_{i,c} \geq
#' \frac{U_{i,c}-L_{i,c}}{2}+L_{i,c}\cr
#' 1 - 2 \cdot \frac{B_{i,c}-L_{i,c}}{U_{i,c}-L_{i,c}}, \text{otherwise}
#' \end{cases}}{ascii}
#'
#' then,
#'
#' \mjdeqn{\hat{p}_c(AsymWAgg) = \sum_{i=1}^N \tilde{w}\_asym_{i,c}B_{i,c}.}{ascii}
#'
#' **IndIntAsymWAgg**: Weighted by individuals’ interval widths and asymmetry
#'
#' This rewards both asymmetric and narrow intervals. We simply multiply the weights calculated
#' in the `"AsymWAgg"` and `"IndIntWAgg"` methods.
#'
#' \mjdeqn{w\_nIndivInterval\_asym_{i,c} = \tilde{w}\_nIndivInterval_{i,c} \cdot \tilde{w}\_asym_{i,c}}{ascii}
#'
#' \mjdeqn{\hat{p}_c( IndIntAsymWAgg) = \sum_{i=1}^N \tilde{w}\_nIndivInterval\_asym_{i,c}B_{i,c}}{ascii}
#'
#' **VarIndIntWAgg**: Weighted by the variation in individuals’ interval widths
#'
#' A higher variance in individuals' interval width across claims may indicate a higher responsiveness
#' to the supporting evidence of different claims. Such responsiveness might be predictive of more
#' accurate assessors. We define:
#'
#' \mjdeqn{w\_varIndivInterval_{i}= var{(U_{i,c} - L_{i,c}): c = 1,\dots, C},}{ascii}
#'
#' where the variance (\mjeqn{var}{ascii}) is calculated across all claims for individual \mjeqn{i}{ascii}.
#' Then,
#'
#' \mjdeqn{\hat{p}_c(VarIndIntWAgg) = \sum_{i=1}^N \tilde{w}\_varIndivInterval_{i}B_{i,c}}{ascii}
#'
#' **KitchSinkWAgg**: Weighted by everything but the kitchen sink
#'
#' This method is informed by the intuition that we want to reward narrow and asymmetric intervals,
#' as well as the variability of individuals' interval widths (across their estimates). Again, we multiply
#' the weights calculated in the `"AsymWAgg"`, `"IndIntWAgg"` and `"VarIndIntWAgg"` methods above.
#'
#' \mjdeqn{w\_kitchSink_{i,c} = \tilde{w}\_nIndivInterval_{i,c} \cdot \tilde{w}\_asym_{i,c} \cdot \tilde{w}\_varIndivInterval_{i}}{ascii}
#'
#' \mjdeqn{\hat{p}_c(KitchSinkWAgg) = \sum_{i=1}^N \tilde{w}\_kitchSink_{i,c}B_{i,c}}{ascii}
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param type One of `"IntWAgg"`, `"IndIntWAgg"`, `"AsymWAgg"`, `"IndIntAsymWAgg"`, `"VarIndIntWAgg"`,
#' `"KitchSinkWAgg"`.
#' @param name Name for aggregation method. Defaults to `type` unless specified.
#' @param placeholder Toggle the output of the aggregation method to impute placeholder data.
#' @param percent_toggle Change the values to probabilities. Default is `FALSE`.
#' @param round_2_filter Note that the IDEA protocol results in both a Round 1
#' and Round 2 set of probabilities for each claim. Unless otherwise specified,
#' we will assume that the final Round 2 responses (after discussion) are being
#' referred to.
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`.
#'
#' @examples
#' \donttest{IntervalWAgg(data_ratings)}
#'
#' @export
#' @md

IntervalWAgg <- function(expert_judgements,
                         type = "IntWAgg",
                         name = NULL,
                         placeholder = FALSE,
                         percent_toggle = FALSE,
                         round_2_filter = TRUE) {

  if(!(type %in% c("IntWAgg",
                   "IndIntWAgg",
                   "AsymWAgg",
                   "IndIntAsymWAgg",
                   "VarIndIntWAgg",
                   "KitchSinkWAgg"))){

    stop('`type` must be one of "IntWAgg", "IndIntWAgg", "AsymWAgg", "IndIntAsymWAgg", "VarIndIntWAgg" or "KitchSinkWAgg"')

  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("IntervalWAgg: %s",
                      name))

  if(isTRUE(placeholder)){

    method_placeholder(expert_judgements,
                       name)

  } else {

    df <- expert_judgements %>%
      preprocess_judgements(percent_toggle = {{percent_toggle}},
                            round_2_filter = {{round_2_filter}}) %>%
      dplyr::group_by(paper_id)

    switch(type,
           "IntWAgg" = {

             weights <- df %>%
               weight_interval() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::select(-ub) %>%
               dplyr::ungroup()  %>%
               dplyr::select(paper_id,
                      user_name,
                      agg_weight)

             # standardise weight
             weights_sum <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE))

             df <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               # calculate aggregated judgement by claim
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           },
           "IndIntWAgg" = {

             weights <- df %>%
               weight_nIndivInterval() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup()  %>%
               dplyr::select(paper_id,
                      user_name,
                      agg_weight)

             # standardise weight
             weights_sum <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE))

             df <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               # calculate aggregated judgement by claim
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           },
           "AsymWAgg" = {

             weights <- df %>%
               weight_asym() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup()  %>%
               dplyr::select(paper_id,
                      user_name,
                      agg_weight)

             # standardise weight
             weights_sum <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE))

             df <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               # calculate aggregated judgement by claim
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           },
           "VarIndIntWAgg" = {

             weights <- df %>%
               weight_varIndivInterval() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup()  %>%
               dplyr::select(paper_id,
                      user_name,
                      agg_weight) %>%
               dplyr::distinct()

             # standardise weight
             weights_sum <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE))

             df <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               # calculate aggregated judgement by claim
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           },
           "IndIntAsymWAgg" = {

             nindiv_weight <- df %>%
               weight_nIndivInterval() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup() %>%
               dplyr::select(paper_id, user_name, agg_weight) %>%
               dplyr::rename(indiv = agg_weight)

             asym_weight <- df %>%
               weight_asym() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup() %>%
               dplyr::select(paper_id, user_name, agg_weight) %>%
               dplyr::rename(asym = agg_weight)

             df <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::left_join(nindiv_weight,
                         by = c("paper_id",
                                "user_name")) %>%
               dplyr::left_join(asym_weight,
                         by = c("paper_id",
                                "user_name")) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(indiv_weight = indiv / sum(indiv,
                                                        na.rm = TRUE),
                             asym_weight = asym / sum(asym,
                                                      na.rm = TRUE),
                             agg_weight = indiv_weight * asym_weight,
                             agg_weight = agg_weight / sum(agg_weight,
                                                           na.rm = TRUE)) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           },
           "KitchSinkWAgg" = {

             nindiv_weight <- df %>%
               weight_nIndivInterval() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup() %>%
               dplyr::select(paper_id,
                             user_name,
                             agg_weight) %>%
               dplyr::rename(indiv = agg_weight)

             asym_weight <- df %>%
               weight_asym() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup() %>%
               dplyr::select(paper_id, user_name, agg_weight) %>%
               dplyr::rename(asym = agg_weight)

             var_weight <- df %>%
               weight_varIndivInterval() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup() %>%
               dplyr::select(paper_id,
                             user_name,
                             agg_weight) %>%
               dplyr::distinct() %>%
               dplyr::rename(var = agg_weight)

             df <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::left_join(nindiv_weight,
                         by = c("paper_id",
                                "user_name")) %>%
               dplyr::left_join(asym_weight,
                         by = c("paper_id",
                                "user_name")) %>%
               dplyr::left_join(var_weight,
                         by = c("paper_id",
                                "user_name")) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(indiv_weight = indiv / sum(indiv,
                                                        na.rm = TRUE),
                             asym_weight = asym / sum(asym,
                                                      na.rm = TRUE),
                             var_weight = var / sum(var,
                                                    na.rm = TRUE),
                             agg_weight = indiv_weight * asym_weight * var_weight,
                             agg_weight = agg_weight / sum(agg_weight,
                                                           na.rm = TRUE)) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           })

    df %>%
      dplyr::mutate(method = name) %>%
      postprocess_judgements()

  }
}
