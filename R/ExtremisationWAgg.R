#' @title
#' Aggregation Method: ExtremisationWAgg
#'
#' @description
#' Calculate beta-transformed arithmetic means of best estimates.
#'
#' @details
#' This method takes the average of best estimates and transforms it using the cumulative
#' distribution function of a beta distribution.
#'
#' `type` may be one of the following:
#'
#' **BetaArMean**: Beta transformation applied across the entire range of calculated confidence scores.
#' \loadmathjax
#'
#' \mjdeqn{\hat{p}_c\left( \text{BetaArMean} \right) = H_{\alpha \beta}\left(\frac{1}{N} \sum_{i=1}^N B_{i,c} \right),}{ascii}
#'
#' where \mjeqn{H_{\alpha \beta}}{ascii} is the cumulative distribution function of the beta distribution
#' with parameters \mjeqn{\alpha}{ascii} and \mjeqn{\beta}{ascii}, which default to 6 in the function.
#'
#' The justification for equal parameters (the 'shape1' and 'shape2' arguments in the `stats::pbeta` function)
#' are outlined in Satopää et al (2014) and the references therein (note that the method outlined in that paper
#' is called a beta-transformed linear opinion pool).
#' To decide on the default shape value of `6`, we explored the `data_ratings` dataset with random subsets of 5 assessments per claim,
#' which we expect to have for most of the claims assessed by repliCATS.
#'
#' **BetaArMean2**: Beta transformation applied only to calculated confidence scores that are outside a specified middle range. The premise being that we don't extremise "fence-sitter" confidence scores.
#'
#' \mjdeqn{\hat{p}_c\left( \text{BetaArMean2} \right) = \begin{cases}
#' \displaystyle H_{\alpha \beta}\left(\frac{1}{N} \sum_{i=1}^N B_{i,c} \right), \text{ for } \frac{1}{N} \sum_{i=1}^N B_{i,c} < \textit{cutoff\_lower} \cr
#' \displaystyle \frac{1}{N} \sum_{i=1}^N B_{i,c}, \text{ for } \textit{cutoff\_lower} \leq \frac{1}{N} \sum_{i=1}^N B_{i,c} \leq \textit{cutoff\_upper} \cr
#' \displaystyle H_{\alpha \beta}\left(\frac{1}{N} \sum_{i=1}^N B_{i,c} \right), \text{ for } \frac{1}{N} \sum_{i=1}^N B_{i,c} > \textit{cutoff\_upper} \cr
#' \end{cases}}{ascii}
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param type One of `"BetaArMean"` or `"BetaArMean2"`.
#' @param name Name for aggregation method. Defaults to `type` unless specified.
#' @param alpha parameter for the 'shape1' argument in the `stats::pbeta` function (defaults to 6)
#' @param beta parameter for the 'shape2' argument in the `stats::pbeta` function (defaults to 6)
#' @param cutoff_lower Lower bound of middle region without extremisation in `"BetaArMean2"` aggregation `type`s.
#' @param cutoff_upper Upper bound of middle region without extremisation in `"BetaArMean2"` aggregation `type`s.
#' @param placeholder Toggle the output of the aggregation method to impute placeholder data.
#' @param percent_toggle Change the values to probabilities. Default is `FALSE`.
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`.
#'
#' @examples
#' \dontrun{ExtremisationWAgg(data_ratings)}
#'
#' @export
#' @md

ExtremisationWAgg <- function(expert_judgements,
                              type = "BetaArMean",
                              name = NULL,
                              alpha = 6,
                              beta = 6,
                              cutoff_lower = NULL,
                              cutoff_upper = NULL,
                              placeholder = FALSE,
                              percent_toggle = FALSE) {

  if(!(type %in% c("BetaArMean",
                   "BetaArMean2"))){

    stop('`type` must be one of "BetaArMean" or "BetaArMean2')

  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("ExtremisationWAgg: %s",
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
           "BetaArMean" = {

             df <- df %>%
               dplyr::summarise(mean_judgement = mean(value,
                                                      na.rm = TRUE),
                                n_experts = dplyr::n()) %>%
               dplyr::mutate(aggregated_judgement = stats::pbeta(q = mean_judgement,
                                                                 shape1 = alpha,
                                                                 shape2 = beta))

           },
           "BetaArMean2" = {

             df <- df %>%
               dplyr::summarise(mean_judgement = mean(value,
                                                      na.rm = TRUE),
                                n_experts = dplyr::n()) %>%
               dplyr::mutate(aggregated_judgement = dplyr::if_else(
                 mean_judgement < cutoff_lower | mean_judgement > cutoff_upper,
                 stats::pbeta(q = mean_judgement,
                              shape1 = alpha,
                              shape2 = beta),
                 mean_judgement))

           })

    df %>%
      dplyr::mutate(method = name) %>%
      postprocess_judgements()

  }
}
