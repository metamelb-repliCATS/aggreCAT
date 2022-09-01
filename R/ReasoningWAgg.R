#' @title
#' Aggregation Method: ReasoningWAgg
#'
#' @description
#' Calculate one of several types of linear-weighted best estimates using supplementary
#' participant reasoning data to create weights.
#'
#' @details
#' Weighted by the breadth of reasoning provided to support the individuals’ estimate.
#'
#' `type` may be one of the following:
#'
#' \loadmathjax
#' **ReasonWAgg**: Weighted by the number of supporting reasons
#'
#' Giving greater weight to best estimates that are accompanied by a greater number of
#' supporting reasons may be beneficial. We will consider \mjeqn{w\_{reason}_{i,c}}{ascii}
#' to be the number of unique reasons provided by that individual \mjeqn{i}{ascii} in
#' support of their estimate for claim \mjeqn{c}{ascii}.
#'
#' \mjdeqn{\hat{p}_c(ReasonWAgg) = \sum_{i=1}^N \tilde{w}\_reason_{i,c}B_{i,c}}{ascii}
#'
#' See Hanea et al. (2021) for an example of reason coding.
#'
#' **ReasonWAgg2**: Incorporates both the number of reasons and their diversity across claims.
#'
#' The claim diversity component of this score is calculated per individual from all claims
#' they assessed. We assume each individual answers at least two claims. If an individual has
#' assessed only one claim, there weighting for that claim is equivalent to "ReasonWAgg".
#'
#' We will consider \mjeqn{w\_{varReason}_{i,c}}{ascii} to be the weighted "number of unique
#' reasons" provided by participant \mjeqn{i}{ascii} in support of their estimate for claim
#' \mjeqn{c}{ascii}. Assume there are \mjeqn{R}{ascii} total unique reasons any participant
#' can use to justify their numerical answers. Then, for each participant \mjeqn{i}{ascii} we
#' can construct a matrix \mjeqn{\mathbf{CR_i}}{ascii} with \mjeqn{R}{ascii} columns, each
#' corresponding to a unique reason, \mjeqn{r}{ascii}, and \mjeqn{C}{ascii} rows, where
#' \mjeqn{C}{ascii} is the number of claims assessed by that participant. Each element of
#' this matrix \mjeqn{\mathbf{CR_i}(r,c)}{ascii} can be either 1 or 0.
#' \mjeqn{\mathbf{CR_i}(r,c) = 1}{ascii} if reason \mjeqn{R_r}{ascii} was used to justify
#' the estimates assessed for \mjeqn{c}{ascii}, and \mjeqn{\mathbf{CR_i}(r,c) = 0}{ascii} if
#' reason \mjeqn{R_r}{ascii} was not mentioned when assessing claim \mjeqn{c}{ascii}. The
#' more frequently that a participant uses a given reason reduces the amount it contributes
#' to the weight assigned to that participant.
#'
#' \mjdeqn{w\_{varReason}_{i,c} =\sum_{r=1}^{R} \mathbf{CR_i}(c,r) \cdot (1 - \frac{\sum_{c=1}^C
#' \mathbf{CR_i}(c,r)}{C})}{ascii}
#'
#' \mjdeqn{\hat{p}_c(ReasonWAgg2) = \sum_{i=1}^N \tilde{w}\_varReason_{i,c}B_{i,c}}{ascii}
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param reasons A dataframe in the form of [data_supp_ReasonWAgg]
#' @param type One of `"ReasonWAgg"`, `"ReasonWAgg2"`.
#' @param name Name for aggregation method. Defaults to `type` unless specified.
#' @param beta_transform Toggle switch to extremise confidence scores with the beta distribution. Defaults to `FALSE`.
#' @param beta_param Length two vector of alpha and beta parameters of the beta distribution. Defaults to `c(6,6)`.
#' @param placeholder Toggle the output of the aggregation method to impute placeholder data.
#' @param percent_toggle Change the values to probabilities. Default is `FALSE`.
#' @param flag_loarmean A toggle to impute LOArMean instead of ArMean when no participants have a reasoning weight for a specific claim (defaults `FALSE`).
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`.
#'
#' @examples
#' \dontrun{ReasoningWAgg(data_ratings)}
#'
#' @export
#' @md

ReasoningWAgg <- function(expert_judgements,
                          reasons = NULL,
                          type = "ReasonWAgg",
                          name = NULL,
                          beta_transform = FALSE,
                          beta_param = c(6, 6),
                          placeholder = FALSE,
                          percent_toggle = FALSE,
                          flag_loarmean = FALSE) {

  if(!(type %in% c("ReasonWAgg",
                   "ReasonWAgg2"))){

    stop('`type` must be one of "ReasonWAgg" or "ReasonWAgg2"')

  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("ReasoningWAgg: %s",
                      name))

  if(isTRUE(placeholder)){

    method_placeholder(expert_judgements,
                       name)

  } else {

    df <- expert_judgements %>%
      preprocess_judgements(percent_toggle = {{percent_toggle}}) %>%
      dplyr::filter(element == "three_point_best") %>%
      dplyr::group_by(paper_id)

    switch(type,
           "ReasonWAgg" = {

             reason_weights <- weight_reason(reasons)

           },
           "ReasonWAgg2" = {

             reason_weights <- weight_reason2(reasons)

           })

    # work out which claims have no participants with reasoning scores

    with_rs <-
      reason_weights %>%
      dplyr::right_join(df, by = c("user_name", "paper_id"))

    # output summary data
    # identify claims that don't have quiz scores associated
    claim_summary_data <- with_rs %>%
      dplyr::group_by(paper_id) %>%
      dplyr::summarise(
        n_experts = dplyr::n(),
        no_reason_scores_for_claim = all(is.na(reason_count))
      )

    # loarmean for when there aren't quiz scores
    loarmean_data <- df %>%
      dplyr::full_join(claim_summary_data, by = "paper_id") %>%
      # filter to no quiz scores
      dplyr::filter(no_reason_scores_for_claim)

    loarmean_results <- if (nrow(loarmean_data) > 0) {
      loarmean_data %>%
        # Taking the core function of LOArMean
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
                         # first = min(timestamp,
                         #             na.rm = TRUE),
                         # last = max(timestamp,
                         #            na.rm = TRUE)
        ) %>%
        dplyr::mutate(aggregated_judgement =
                        exp(aggregated_judgement) /
                        (1 + exp(aggregated_judgement))) %>%
        dplyr::mutate(method = name)
    } else {NULL}

    # Process the no quiz scores dataframe to formatted output
    if(!is.null(loarmean_results)){
      loarmean_results <- postprocess_judgements(loarmean_results)
    }

    # calculate reason weight
    reasonwagg_data <-
      claim_summary_data %>%
      dplyr::select(paper_id, no_reason_scores_for_claim) %>%
      dplyr::full_join(with_rs) %>%
      # filter to paper_ids with quiz scores associated
      dplyr::filter(!no_reason_scores_for_claim)

    ReasonWAgg_results <- reasonwagg_data %>%
      # calculate scaled weight by claim
      dplyr::group_by(paper_id) %>%
      dplyr::summarise(agg_sum = sum(reason_count,
                                     na.rm = TRUE)) %>%
      dplyr::full_join(reasonwagg_data, by = "paper_id") %>%
      dplyr::mutate(agg_weight = reason_count / agg_sum) %>%
      dplyr::group_by(paper_id) %>%
      dplyr::summarise(
        aggregated_judgement = sum(agg_weight * value,
                                   na.rm = TRUE),
        n_experts = dplyr::n()) %>%
      dplyr::left_join(claim_summary_data) %>%
      # output
      dplyr::mutate(method = name) %>%
      postprocess_judgements()

    if(isTRUE(beta_transform)){

      ReasonWAgg_results <- ReasonWAgg_results %>%
        dplyr::mutate(cs = stats::pbeta(q = cs,
                                        shape1 = beta_param[1],
                                        shape2 = beta_param[2]))

    }

    output_data <-
      dplyr::bind_rows(loarmean_results, ReasonWAgg_results)

    if(flag_loarmean) {
      claim_summary_data %>%
        dplyr::select(paper_id, no_reason_scores_for_claim) %>%
        dplyr::full_join(output_data) %>%
        dplyr::mutate(method_applied =
                        dplyr::if_else(no_reason_scores_for_claim,
                                       "LOArMean", name))
    } else {

      output_data

    }
  }
}

