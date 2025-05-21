#' @title
#' Aggregation Method: LinearWAgg
#'
#' @description
#' Calculate one of several types of linear-weighted best estimates.
#'
#' @details
#' This function returns weighted linear combinations of the best-estimate judgements for each claim.
#'
#' `type` may be one of the following:
#' \loadmathjax
#'
#' **Judgement**: Weighted by user-supplied weights at the judgement level
#' \mjdeqn{\hat{p}_c\left( JudgementWeights \right) = \sum_{i=1}^N
#' \tilde{w}\_judgement_{i,c}B_{i,c}}{ascii}
#'
#' **Participant**: Weighted by user-supplied weights at the participant level
#' \mjdeqn{\hat{p}_c\left( ParticipantWeights \right) = \sum_{i=1}^N
#' \tilde{w}\_participant_{i}B_{i,c}}{ascii}
#'
#' **DistLimitWAgg**: Weighted by the distance of the best estimate from the
#' closest certainty limit. Giving greater weight to best estimates that are closer to certainty
#' limits may be beneficial.
#' \mjdeqn{w\_distLimit_{i,c} = \max \left(B_{i,c}, 1-B_{i,c}\right)}{ascii}
#' \mjdeqn{\hat{p}_c\left( DistLimitWAgg \right) = \sum_{i=1}^N
#' \tilde{w}\_distLimit_{i,c}B_{i,c}}{ascii}
#'
#' **GranWAgg**: Weighted by the granularity of best estimates
#'
#' Individuals are weighted by whether or not their best estimates are more
#' granular than a level of 0.05 (i.e., not a multiple of 0.05).
#' \mjdeqn{w\_gran_{i} = \frac{1}{C} \sum_{d=1}^C \left\lceil{\frac{B_{i,d}}
#' {0.05}-\left\lfloor{\frac{B_{i,d}}{0.05}}\right\rfloor}\right\rceil,}{ascii}
#'
#' where \mjeqn{\lfloor{\ }\rfloor}{ascii} and \mjeqn{\lceil{\ }\rceil}{ascii}
#' are the mathematical floor and ceiling functions respectively.
#' \mjdeqn{\hat{p}_c\left( GranWAgg \right) = \sum_{i=1}^N \tilde{w}\_gran_{i}
#' B_{i,c}}{ascii}
#'
#' **OutWAgg**: Down weighting outliers
#'
#' This method down-weights outliers by using the differences from the central
#' tendency (median) of an individual's best estimates.
#' \mjdeqn{d_{i,c} = \left(median\{{B_{i,c}}_{_{i=1,...,N}}\} - B_{i,c}\right)^2}{ascii}
#' \mjdeqn{w\_out_{i} = 1 - \frac{d_{i,c}}{\max({d_c})})}{ascii}
#' \mjdeqn{\hat{p}_c\left( OutWAgg \right) = \sum_{i=1}^N \tilde{w}\_out_{i}B_{i,c}}{ascii}
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param type One of `"Judgement"`, `"Participant"`, `"DistLimitWAgg"`, `"GranWAgg"`, or `"OutWAgg"`.
#' @param weights (Optional) A two column dataframe (`user_name` and `weight`) for `type = "Participant"`
#' or a three two column dataframe (`paper_id', 'user_name` and `weight`) for `type = "Judgement"`
#' @param name Name for aggregation method. Defaults to `type` unless specified.
#' @param placeholder Toggle the output of the aggregation method to impute placeholder data.
#' @param percent_toggle Change the values to probabilities. Default is `FALSE`.
#' @param flag_loarmean A toggle to impute log mean (defaults `FALSE`).
#' @param round_2_filter Note that the IDEA protocol results in both a Round 1
#' and Round 2 set of probabilities for each claim. Unless otherwise specified,
#' we will assume that the final Round 2 responses (after discussion) are being
#' referred to.
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`.
#'
#' @examples
#' \donttest{LinearWAgg(data_ratings)}
#'
#' @export
#' @md

LinearWAgg <- function(expert_judgements,
                       type = "DistLimitWAgg",
                       weights = NULL,
                       name = NULL,
                       placeholder = FALSE,
                       percent_toggle = FALSE,
                       flag_loarmean = FALSE,
                       round_2_filter = TRUE) {

  if(!(type %in% c("Judgement",
                   "Judgement_LO",
                   "Participant",
                   "Participant_LO",
                   "DistLimitWAgg",
                   "GranWAgg",
                   "OutWAgg"))){

    stop('`type` must be one of "Judgement", "Judgement_LO", "Participant", "Participant_LO", "DistLimitWAgg", "GranWAgg", or "OutWAgg"')

  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("LinearWAgg: %s",
                      name))

  if(isTRUE(placeholder)){

    method_placeholder(expert_judgements,
                       name)

  } else {

    df <- expert_judgements %>%
      preprocess_judgements(percent_toggle = {{percent_toggle}},
                            round_2_filter = {{round_2_filter}}) %>%
      dplyr::filter(element == "three_point_best") %>%
      dplyr::group_by(paper_id)

    switch(type,
           "Judgement" = {

             ## User-supplied weights at the judgement-level

             weights_sum <- df %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(weight,
                                              na.rm = TRUE),
                                # set to arbitrarily small number if 0
                                agg_sum = dplyr::if_else(agg_sum == 0,
                                                         .Machine$double.eps,
                                                         agg_sum))

             df <- df %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = weight /  agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(
                 aggregated_judgement = sum(agg_weight * value,
                                            na.rm = TRUE),
                 n_experts = dplyr::n()
               )

           },
           "Judgement_LO" = {

             ## User-supplied weights at the judgement-level with log-odds default for no weights

             weights_df <- df %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(no_weights_for_claim = all(is.na(weight))) %>%
               dplyr::ungroup()

             weights_sum <- weights_df %>%
               dplyr::filter(no_weights_for_claim == FALSE) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(weight,
                                              na.rm = TRUE),
                                # set to arbitrarily small number if 0
                                agg_sum = dplyr::if_else(agg_sum == 0,
                                                         .Machine$double.eps,
                                                         agg_sum))

             df_judgement <- weights_df %>%
               dplyr::filter(no_weights_for_claim == FALSE) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = weight /  agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(
                 aggregated_judgement = sum(agg_weight * value,
                                            na.rm = TRUE),
                 n_experts = dplyr::n()
               )

             df_LO_data <- weights_df %>%
               dplyr::filter(no_weights_for_claim == TRUE)

             df_LO <- if (nrow(df_LO_data) > 0) {

               df_LO_data %>%
                 dplyr::group_by(paper_id) %>%
                 dplyr::mutate(value = dplyr::if_else(
                   value == 1 | value == 0,
                   value + .Machine$double.eps,
                   value
                 )) %>%
                 dplyr::mutate(log_odds = log(abs(value / (1 - value)))) %>%
                 dplyr::summarise(aggregated_judgement = mean(log_odds,
                                                              na.rm = TRUE),
                                  n_experts = dplyr::n()
                 ) %>%
                 dplyr::mutate(aggregated_judgement =
                                 exp(aggregated_judgement) /
                                 (1 + exp(aggregated_judgement)))

             } else {NULL}

             df <- dplyr::bind_rows(df_judgement,
                                    df_LO)

             LO_papers <- df_LO_data %>%
               dplyr::pull(paper_id) %>%
               unique()

           },
           "Participant" = {

             ## User-supplied weights at the participant-level

             weights_sum <- df %>%
               dplyr::left_join(weights,
                                by = c("user_name")) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(weight,
                                              na.rm = TRUE),
                                # set to arbitrarily small number if 0
                                agg_sum = dplyr::if_else(agg_sum == 0,
                                                         .Machine$double.eps,
                                                         agg_sum))

             df <- df %>%
               dplyr::left_join(weights,
                                by = c("user_name")) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = weight /  agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(
                 aggregated_judgement = sum(agg_weight * value,
                                            na.rm = TRUE),
                 n_experts = dplyr::n()
               )
           },
           "Participant_LO" = {

             ## User-supplied weights at the participant-level with log odds default for no weights

             weights_df <- df %>%
               dplyr::left_join(weights,
                                by = c("user_name")) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(no_weights_for_claim = all(is.na(weight))) %>%
               dplyr::ungroup()

             weights_sum <- weights_df %>%
               dplyr::filter(no_weights_for_claim == FALSE) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(weight,
                                              na.rm = TRUE),
                                # set to arbitrarily small number if 0
                                agg_sum = dplyr::if_else(agg_sum == 0,
                                                         .Machine$double.eps,
                                                         agg_sum))

             df_participant <- weights_df %>%
               dplyr::filter(no_weights_for_claim == FALSE) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = weight /  agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(
                 aggregated_judgement = sum(agg_weight * value,
                                            na.rm = TRUE),
                 n_experts = dplyr::n()
               )

             df_LO_data <- weights_df %>%
               dplyr::filter(no_weights_for_claim == TRUE)

             df_LO <- if (nrow(df_LO_data) > 0) {

               df_LO_data %>%
                 dplyr::group_by(paper_id) %>%
                 dplyr::mutate(value = dplyr::if_else(
                   value == 1 | value == 0,
                   value + .Machine$double.eps,
                   value
                 )) %>%
                 dplyr::mutate(log_odds = log(abs(value / (1 - value)))) %>%
                 dplyr::summarise(aggregated_judgement = mean(log_odds,
                                                              na.rm = TRUE),
                                  n_experts = dplyr::n()
                 ) %>%
                 dplyr::mutate(aggregated_judgement =
                                 exp(aggregated_judgement) /
                                 (1 + exp(aggregated_judgement)))

             } else {NULL}

             df <- dplyr::bind_rows(df_participant,
                                    df_LO)

             LO_papers <- df_LO_data %>%
               dplyr::pull(paper_id) %>%
               unique()

           },
           "DistLimitWAgg" = {

             if(any(df$value < 0) | any(df$value > 1)){

               stop("DistLimitWAgg requires judgements bounded 0-1. Check your data compatability or `percent_toggle` argument.")

             }

             weights <- df %>%
               dplyr::group_by(paper_id,
                               user_name) %>%
               dplyr::mutate(agg_weight = max(value, 1 - value,
                                              na.rm = TRUE)) %>%
               dplyr::ungroup()

             weights_sum <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE))

             df <- weights %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           },
           "GranWAgg" = {

             if(any(df$value < 0) | any(df$value > 1)){

               stop("GranWAgg requires probabilistic judgements. Check your data compatability or `percent_toggle` argument.")

             }

             weights <- df %>%
               ## Calculate observation-level value
               ## .Machine$double.eps correction is to handle nonsense floating point issue: e.g. floor(0.15/0.05)
               dplyr::mutate(gran_binary = ceiling(value / 0.05 - floor(value / 0.05 + .Machine$double.eps*4))) %>%
               ## Caclulate weight by individual
               dplyr::group_by(user_name) %>%
               dplyr::summarise(agg_weight = mean(gran_binary,
                                                  na.rm = TRUE),
                                # to avoid dividing by 0
                                agg_weight = dplyr::if_else(agg_weight == 0,
                                                            .Machine$double.eps,
                                                            agg_weight))

             weights_sum  <- df %>%
               dplyr::left_join(weights,
                                by = "user_name") %>%
               ## Calculate scaled weight by claim
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(agg_sum = sum(agg_weight,
                                              na.rm = TRUE))

             df <- df %>%
               dplyr::left_join(weights,
                                by = "user_name") %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(
                 aggregated_judgement = sum(agg_weight * value,
                                            na.rm = TRUE),
                 n_experts = dplyr::n())

           },
           "OutWAgg" = {

             # get weights
             weights <- df %>%
               weight_outlier() %>%
               dplyr::group_by(paper_id) %>%
               dplyr::mutate(agg_weight = dplyr::case_when(all(agg_weight == 0) ~ 1,
                                                           TRUE ~ agg_weight)) %>%
               dplyr::ungroup() %>%
               dplyr::select(paper_id,
                             user_name,
                             agg_weight)

             # standardise weights
             weights_sum <- weights %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(
                 agg_sum = sum(agg_weight,
                               na.rm = TRUE)
               )

             df <- df %>%
               dplyr::left_join(weights,
                                by = c("paper_id",
                                       "user_name")) %>%
               dplyr::left_join(weights_sum,
                                by = "paper_id") %>%
               dplyr::mutate(agg_weight = agg_weight / agg_sum) %>%
               dplyr::group_by(paper_id) %>%
               dplyr::summarise(aggregated_judgement = sum(agg_weight * value,
                                                           na.rm = TRUE),
                                n_experts = dplyr::n())

           })

    df <- df %>%
      dplyr::mutate(method = name)

    if(flag_loarmean){

      df <- df %>%
        dplyr::mutate(method = dplyr::if_else(paper_id %in% LO_papers,
                                       "LOArMean",
                                       method))

    }

    df %>%
      postprocess_judgements()

  }
}

