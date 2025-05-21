#' @title
#' Aggregation Method: AverageWAgg
#'
#' @description
#' Calculate one of several types of averaged best estimates.
#'
#' @details
#' This function returns the average, median and transformed averages of
#' best-estimate judgements for each claim.
#'
#' `type` may be one of the following:
#' \loadmathjax
#'
#' **ArMean**: Arithmetic mean of the best estimates
#' \mjdeqn{\hat{p}_c\left(ArMean \right ) = \frac{1}{N}\sum_{i=1}^N B_{i,c}}{ascii}
#' **Median**: Median of the best estimates
#' \mjdeqn{\hat{p}_c \left(\text{median} \right) = \text{median} \{ B^i_c\}_{i=1,...,N}}{ascii}
#' **GeoMean**: Geometric mean of the best estimates
#' \mjdeqn{GeoMean_{c}=  \left(\prod_{i=1}^N  B_{i,c}\right)^{\frac{1}{N}}}{ascii}
#' **LOArMean**: Arithmetic mean of the log odds transformed best estimates
#' \mjdeqn{LogOdds_{i,c}= \frac{1}{N} \sum_{i=1}^N log\left( \frac{B_{i,c}}{1-B_{i,c}}\right)}{ascii}
#' The average log odds estimate is then back transformed to give a final group estimate:
#' \mjdeqn{\hat{p}_c\left( LOArMean \right) = \frac{e^{LogOdds_{i,c}}}{1+e^{LogOdds_{i,c}}}}{ascii}
#' **ProbitArMean**: Arithmetic mean of the probit transformed best estimates
#' \mjdeqn{Probit_{c}= \frac{1}{N} \sum_{i=1}^N \Phi^{-1}\left( B_{i,c}\right)}{ascii}
#' The average probit estimate is then back transformed to give a final group estimate:
#' \mjdeqn{\hat{p}_c\left(ProbitArMean \right) = \Phi\left({Probit_{c}}\right)}{ascii}
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param type One of `"ArMean"`, `"Median"`, `"GeoMean"`, `"LOArMean"`, or `"ProbitArMean"`.
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
#' \donttest{AverageWAgg(data_ratings)}
#'
#' @export
#' @md

AverageWAgg <- function(expert_judgements,
                        type = "ArMean",
                        name = NULL,
                        placeholder = FALSE,
                        percent_toggle = FALSE,
                        round_2_filter = TRUE) {

  if(!(type %in% c("ArMean",
                   "GeoMean",
                   "Median",
                   "LOArMean",
                   "LOGeoMean",
                   "ProbitArMean"))){

    stop('`type` must be one of "ArMean", "GeoMean", "Median", "LOArMean", "LOGeoMean", or "ProbitArMean"')

  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("AverageWAgg: %s",
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
           "ArMean" = {

             df <- df %>%
               dplyr::summarise(
                 aggregated_judgement = mean(value,
                                             na.rm = TRUE),
                 n_experts = dplyr::n()
               )

           },
           "GeoMean" = {

             df <- df %>%
               dplyr::summarise(n_experts = dplyr::n(),
                                aggregated_judgement = (prod(value, na.rm = TRUE)) ^ (1/n_experts))

           },
           "Median" = {

             df <- df %>%
               dplyr::summarise(
                 aggregated_judgement = median(value,
                                               na.rm = TRUE),
                 n_experts = dplyr::n()
               )

           },
           "LOArMean" = {

             if(any(df$value < 0) | any(df$value > 1)){

               stop("LOArMean requires probabilistic judgements. Check your data compatability or `percent_toggle` argument.")

             }

             df <- df %>%
               dplyr::mutate(value = dplyr::case_when(value == 1 ~ value - .Machine$double.eps,
                                                      value == 0 ~ value + .Machine$double.eps,
                                                      TRUE ~ value),
                             log_odds = log(abs(value / (1 - value)))) %>%
               dplyr::summarise(
                 aggregated_judgement = mean(log_odds,
                                             na.rm = TRUE),
                 n_experts = dplyr::n()
               )  %>%
               dplyr::mutate(
                 aggregated_judgement = exp(aggregated_judgement) / (1 + exp(aggregated_judgement))
               )

           },
           "LOGeoMean" = {

             if(any(df$value < 0) | any(df$value > 1)){

               stop("LOGeoMean requires probabilistic judgements. Check your data compatability or `percent_toggle` argument.")

             }

             df <- df %>%
               dplyr::mutate(value = dplyr::case_when(value == 1 ~ value - .Machine$double.eps,
                                                      value == 0 ~ value + .Machine$double.eps,
                                                      value == 0.5 ~ value + .Machine$double.eps,
                                                      TRUE ~ value),
                             log_odds = log(abs(value / (1 - value)))) %>%
               # dplyr::summarise(n_experts = dplyr::n(),
               #                  aggregated_judgement = (prod(log_odds, na.rm = TRUE))) %>%
               dplyr::summarise(n_experts = dplyr::n(),
                                aggregated_judgement = (prod(log_odds, na.rm = TRUE)) ^ (1/n_experts)) %>%
               dplyr::mutate(
                 aggregated_judgement = exp(aggregated_judgement) / (1 + exp(aggregated_judgement))
               )

           },
           "ProbitArMean" = {


             df <- df %>%
               dplyr::mutate(probit = VGAM::probitlink(value,
                                                       bvalue = .Machine$double.eps)) %>%
               dplyr::summarise(aggregated_judgement = mean(probit,
                                                            na.rm = TRUE),
                                n_experts = dplyr::n()) %>%
               dplyr::mutate(aggregated_judgement = VGAM::probitlink(aggregated_judgement,
                                                                     inverse = TRUE))

           })

    df %>%
      dplyr::mutate(method = name) %>%
      postprocess_judgements()

  }
}
