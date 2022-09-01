#' @title
#' Aggregation Method: ShiftingWAgg
#'
#' @description
#' Weighted by judgements that shift the most after discussion
#'
#' @details
#' When judgements are elicited using the IDEA protocol (or any other protocol that allows
#' experts to revisit their original estimates), the second round of estimates may differ
#' from the original first set of estimates an expert provides. Greater changes between rounds
#' will be given greater weight.
#'
#' `type` may be one of the following:
#'
#' \loadmathjax
#' **ShiftWAgg**: Takes into account the shift in all three estimates
#'
#' Considers shifts across lower, \mjeqn{L_{i,c}}{ascii}, and upper,
#' \mjeqn{U_{i,c}}{ascii}, confidence limits, and the best estimate, \mjeqn{B_{i,c}}{ascii}.
#' More emphasis is placed on changes in the best estimate such that:
#'
#' \mjdeqn{w\_Shift_{i,c} = |B1_{i,c} - B_{i,c}| + \frac{|L1_{i,c} - L_{i,c}|+|U1_{i,c} - U_{i,c}|}{2},}{ascii}
#'
#' where \mjeqn{L1_{i,c}, B1_{i,c},U1_{i,c}}{ascii} are the first round lower, best and upper estimates (prior
#' to discussion) and \mjeqn{L_{i,c}, B_{i,c},U1_{i,c}}{ascii} are the individual’s revised second round estimates
#' (after discussion).
#'
#' \mjdeqn{\hat{p}_c(ShiftWAgg) = \sum_{i=1}^N \tilde{w}\_Shift_{i,c}B_{i,c}}{ascii}
#'
#' **BestShiftWAgg**: Weighted according to shifts in best estimates alone
#'
#' Taking into account the fact that the scales best estimates are measured on are bounded,
#' we can calculate shifts relative to the largest possible shift.
#'
#' \mjdeqn{w\_BestShift_{i,c}=
#' \begin{cases}
#' \frac{|B1_{i,c} - B_{i,c}|}{B1_{i,c}},
#' \begin{aligned}
#' \displaystyle &\ for\ (B1_{i,c} > 0.5\ and\ B_{i,c} \leq 0.5) \cr
#' \displaystyle &\ or\ B_{i,c} < B1_{i,c} \leq 0.5\ or\ B1_{i,c} > B_{i,c} > 0.5
#' \end{aligned} \cr
#' \frac{|B1_{i,c} - B_{i,c}|}{1- B1_{i,c}},
#' \begin{aligned}
#' \displaystyle &\ for\ (B1_{i,c} < 0.5\ and\ B_{i,c} \geq 0.5) \cr
#' \displaystyle &\ or\ B1_{i,c} < B_{i,c} < 0.5\ or\ B_{i,c} > B1_{i,c} > 0.5.
#' \end{aligned}
#' \end{cases}}{ascii}
#'
#' \mjdeqn{\hat{p}_c(BestShiftWAgg) = \sum_{i=1}^N \tilde{w}\_BestShift_{i,c}B_{i,c}}{ascii}
#'
#' **IntShiftWAgg**: Weighted by shifts in interval widths alone.
#'
#' Individuals whose interval widths narrow between rounds are given more weight.
#'
#' \mjdeqn{w\_IntShift_{i,c} = \frac{1}{(U_{i,c}-L_{i,c})-(U1_{i,c}-L1_{i,c})+1}}{ascii}
#'
#' \mjdeqn{\hat{p}_c(IntShiftWAgg) = \sum_{i=1}^N \tilde{w}\_IntShift_{i,c}B_{i,c}}{ascii}
#'
#' **DistShiftWAgg**: Weighted by whether best estimates become more extreme (closer to 0 or 1) between rounds.
#'
#' \mjdeqn{w\_DistShift_{i,c} = 1 - (\min (B_{i,c}, 1-B_{i,c}) - \min (B1_{i,c}, 1-B1_{i,c}))}{ascii}
#'
#' \mjdeqn{\hat{p}_c(DistShiftWAgg) = \sum_{i=1}^N \tilde{w}\_DistShift_{i,c}B_{i,c}}{ascii}
#'
#' **DistIntShiftWAgg**: Rewards both narrowing of intervals and shifting towards the certainty limits between rounds.
#'
#' We simply multiply the weights calculated in the "DistShiftWAgg" and "IntShiftWAgg" methods.
#'
#' \mjdeqn{w\_DistIntShift_{i,c} = \tilde{w}\_IntShift_{i,c} \cdot \tilde{w}\_DistShift_{i,c}}{ascii}
#'
#' \mjdeqn{\hat{p}_c(DistIntShiftWAgg) = \sum_{i=1}^N \tilde{w}\_DistIntShift_{i,c}B_{i,c}}{ascii}
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param type One of `"ShiftWAgg"`, `"BestShiftWAgg"`, `"IntShiftWAgg"`, `"DistShiftWAgg"`, or `"DistIntShiftWAgg"`.
#' @param name Name for aggregation method. Defaults to `type` unless specified.
#' @param placeholder Toggle the output of the aggregation method to impute placeholder data.
#' @param percent_toggle Change the values to probabilities. Default is `FALSE`.
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`.
#'
#' @examples
#' \dontrun{ShiftingWAgg(data_ratings)}
#'
#' @export
#' @md

ShiftingWAgg <- function(expert_judgements,
                         type = "ShiftWAgg",
                         name = NULL,
                         placeholder = FALSE,
                         percent_toggle = FALSE) {

  if(!(type %in% c("ShiftWAgg",
                   "BestShiftWAgg",
                   "IntShiftWAgg",
                   "DistShiftWAgg",
                   "DistIntShiftWAgg"))){

    stop('`type` must be one of "ShiftWAgg", "BestShiftWAgg", "IntShiftWAgg", "DistShiftWAgg", or "DistIntShiftWAgg"')

  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("ShiftingWAgg: %s",
                      name))

  if(isTRUE(placeholder)){

    method_placeholder(expert_judgements,
                       name)

  } else {

    df <- expert_judgements %>%
      preprocess_judgements(percent_toggle = {{percent_toggle}},
                            round_2_filter = FALSE) %>%
      dplyr::group_by(paper_id)

    switch(type,
           "ShiftWAgg" = {

             n_experts <- df %>%
               dplyr::group_by(paper_id, user_name) %>%
               dplyr::summarise(n = dplyr::n()) %>%
               dplyr::count() %>%
               dplyr::rename(n_experts = n)

             ## Calculate absolute value of change between rounds

             weights_best <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               dplyr::mutate(row = dplyr::row_number()) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::select(-row) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(diff_best = abs(round_1 - round_2))

             weights_lower <- df %>%
               dplyr::filter(element == "three_point_lower") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               dplyr::mutate(row = dplyr::row_number()) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::select(-row) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(diff_lower = abs(round_1 - round_2)) %>%
               dplyr::select(paper_id,
                             user_name,
                             diff_lower)

             weights_upper <- df %>%
               dplyr::filter(element == "three_point_upper") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               dplyr::mutate(row = dplyr::row_number()) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::select(-row) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(diff_upper = abs(round_1 - round_2)) %>%
               dplyr::select(paper_id,
                             user_name,
                             diff_upper)

             weights <- weights_best %>%
               dplyr::left_join(weights_lower, by = c("paper_id",
                                                      "user_name")) %>%
               dplyr::left_join(weights_upper, by = c("paper_id",
                                                      "user_name")) %>%
               dplyr::mutate(aggregate_diff = diff_best + (diff_lower / 2) + (diff_upper / 2)) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(aggregate_diff = dplyr::case_when(all(aggregate_diff == 0) ~ 1,
                                                               TRUE ~ aggregate_diff)) %>%
               dplyr::ungroup()

             ## Weight calculation

             agg_weights <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(aggregate_diff,
                                              na.rm = TRUE),
                                # set to arbitrarily small number if 0
                                agg_sum = dplyr::if_else(agg_sum == 0,
                                                         .Machine$double.eps,
                                                         agg_sum)
               )

             df <- weights %>%
               dplyr::left_join(agg_weights, by = "paper_id") %>%
               dplyr::mutate(
                 agg_weight = aggregate_diff /  agg_sum
               ) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(
                 aggregated_judgement = sum(agg_weight * round_2,
                                            na.rm = TRUE),
               ) %>%
               dplyr::left_join(n_experts, by = "paper_id")

           },
           "BestShiftWAgg" = {

             n_experts <- df %>%
               dplyr::group_by(paper_id, user_name) %>%
               dplyr::summarise(n = dplyr::n()) %>%
               dplyr::count() %>%
               dplyr::rename(n_experts = n)


             ## Calculate absolute value of change between rounds

             weights <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               dplyr::mutate(row = dplyr::row_number()) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::select(-row) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(diff_best = dplyr::if_else(
                 round_2 < round_1 & round_1 <= 0.5 |
                   round_1 > round_2 & round_2 > 0.5 |
                   round_1 > 0.5 & round_2 <= 0.5,
                 abs(round_1 - round_2)/round_1,
                 abs(round_1 - round_2)/(1 - round_1)
               )
               ) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(aggregate_diff = dplyr::case_when(all(diff_best == 0) ~ 1,
                                                               TRUE ~ diff_best)) %>%
               dplyr::ungroup()

             ## Weight calculation

             agg_weights <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(aggregate_diff,
                                              na.rm = TRUE),
                                # set to arbitrarily small number if 0
                                agg_sum = dplyr::if_else(agg_sum == 0,
                                                         .Machine$double.eps,
                                                         agg_sum)
               )

             df <- weights %>%
               dplyr::left_join(agg_weights, by = "paper_id") %>%
               dplyr::mutate(agg_weight = aggregate_diff /  agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * round_2,
                                            na.rm = TRUE)) %>%
               dplyr::left_join(n_experts,
                                by = "paper_id")

           },
           "IntShiftWAgg" = {

             n_experts <- df %>%
               dplyr::group_by(paper_id, user_name) %>%
               dplyr::summarise(n = dplyr::n()) %>%
               dplyr::count() %>%
               dplyr::rename(n_experts = n)

             ## Calculate interval width change between rounds

             weights <- df %>%
               tidyr::pivot_wider(names_from = "element",
                                  values_from = "value") %>%
               dplyr::mutate(width = three_point_upper - three_point_lower) %>%
               dplyr::select(user_name,
                             paper_id,
                             round,
                             width) %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               dplyr::mutate(row = dplyr::row_number()) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "width",
                                  values_fill = NA) %>%
               dplyr::select(-row) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(diff_width = round_2 - round_1) %>%
               dplyr::select(-round_2,
                             -round_1)

             agg_weights <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::mutate(agg_weight = 1 / (diff_width + 1)) %>%
               dplyr::ungroup()

             # standardise weight
             df <- agg_weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE)) %>%
               dplyr::full_join(agg_weights,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               # calculate aggregated judgement by claim
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * round_2,
                                                           na.rm = TRUE)) %>%
               dplyr::left_join(n_experts,
                                by = "paper_id")

           },
           "DistShiftWAgg" = {

             n_experts <- df %>%
               dplyr::group_by(paper_id, user_name) %>%
               dplyr::summarise(n = dplyr::n()) %>%
               dplyr::count() %>%
               dplyr::rename(n_experts = n)


             ## Calculate absolute value of change between rounds

             weights <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(dist_1 = min(round_1, 1 - round_1,
                                          na.rm = TRUE),
                             dist_2 = min(round_2, 1 - round_2,
                                          na.rm = TRUE),
                             diff_dist = dist_2 - dist_1,
                             aggregate_diff = 1 - diff_dist)

             ## Weight calculation

             agg_weights <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(aggregate_diff,
                                              na.rm = TRUE))

             df <- weights %>%
               dplyr::left_join(agg_weights,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = aggregate_diff /  agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * round_2,
                                                           na.rm = TRUE)) %>%
               dplyr::left_join(n_experts,
                                by = "paper_id")

           },
           "DistIntShiftWAgg" = {

             n_experts <- df %>%
               dplyr::group_by(paper_id, user_name) %>%
               dplyr::summarise(n = dplyr::n()) %>%
               dplyr::count() %>%
               dplyr::rename(n_experts = n)

             ## Calculate interval width change between rounds

             weights <- df %>%
               tidyr::pivot_wider(names_from = "element",
                                  values_from = "value") %>%
               dplyr::mutate(width = three_point_upper - three_point_lower) %>%
               dplyr::select(user_name,
                             paper_id,
                             round,
                             width) %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               dplyr::mutate(row = dplyr::row_number()) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "width",
                                  values_fill = NA) %>%
               dplyr::select(-row) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(diff_width = round_2 - round_1) %>%
               dplyr::select(-round_2,
                             -round_1)

             int_shift <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::left_join(weights, by = c("paper_id",
                                                "user_name")) %>%
               dplyr::mutate(agg_weight = 1/(diff_width+1)) %>%
               dplyr::rename(int_shift = agg_weight) %>%
               dplyr::ungroup()

             dist_shift <- df %>%
               dplyr::filter(element == "three_point_best") %>%
               dplyr::group_by(user_name,
                               paper_id,
                               round) %>%
               tidyr::pivot_wider(names_from = "round",
                                  values_from = "value",
                                  values_fill = NA) %>%
               dplyr::filter(!is.na(round_2)) %>%
               dplyr::mutate(dist_1 = min(round_1, 1 - round_1,
                                          na.rm = TRUE),
                             dist_2 = min(round_2, 1 - round_2,
                                          na.rm = TRUE),
                             diff_dist = dist_2 - dist_1,
                             dist_shift = 1 - diff_dist) %>%
               dplyr::select(paper_id,
                             user_name,
                      dist_shift)

             ## Weight calculation

             agg_weights <- int_shift %>%
               dplyr::left_join(dist_shift,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(int_weight = int_shift /
                               sum(int_shift, na.rm = TRUE),
                             dist_shift = dist_shift /
                               sum(dist_shift, na.rm = TRUE),
                             agg_weight = int_weight * dist_shift)

             # standardise weight
             df <- agg_weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE)) %>%
               dplyr::full_join(agg_weights,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               # calculate aggregated judgement by claim
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * round_2,
                                                           na.rm = TRUE)) %>%
               dplyr::left_join(n_experts,
                                by = "paper_id")


           })

    df %>%
      dplyr::mutate(method = name) %>%
      postprocess_judgements()

  }
}
