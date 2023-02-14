#' @title
#' Aggregation Method: DistributionWAgg
#'
#' @description
#' Calculate the arithmetic mean of distributions created with expert judgements.
#' The aggregate is the median of the average distribution fitted on the individual estimates.
#'
#' @details
#'
#' \loadmathjax
#'
#' This method assumes that the elicited probabilities and bounds can be considered
#' to represent participants' subjective distributions associated with relative
#' frequencies (rather than unique events). That is to say that we considered that
#' the lower bound of the individual per claim corresponds to the 5th percentile
#' of their subjective distribution on the probability of replication, denoted
#' \mjeqn{q_{5,i}}{ascii}, the best estimate corresponds to the median, \mjeqn{q_{50,i}}{ascii}, and the upper
#' bound corresponds to the 95th percentile, \mjeqn{q_{95,i}}{ascii}. With these three
#' percentiles, we can fit parametric or non-parametric distributions and aggregate
#' them rather than the (point) best estimates.
#'
#' `type` may be one of the following:
#'
#' **DistribArMean**: Applies a non-parametric distribution evenly across upper, lower and best estimates.
#'
#' Using the three percentiles we can build the minimally
#' informative non-parametric distribution that spreads the mass uniformly between
#' the three percentiles.
#'
#' \mjdeqn{F_{i}(x) = \begin{cases}
#' \displaystyle 0, \text{ for } x<0 \cr
#' \displaystyle \frac{0.05}{q_{5,i}}\cdot x, \text{ for } 0 \leq x< q_{5,i}\cr
#' \displaystyle \frac{0.45}{q_{50,i}-q_{5,i}}\cdot(x-q_{5,i})+0.05, \text{ for } q_{5,i}\leq x< q_{50,i}\cr
#' \displaystyle \frac{0.45}{q_{95,i}-q_{50,i}}\cdot(x-q_{50,i})+0.5, \text{ for } q_{50,i}\leq x< q_{95,i}\cr
#' \displaystyle \frac{0.05}{1 - q_{95,i}}\cdot(x-q_{95,i})+0.95, \text{ for } q_{95,i}\leq x< 1\cr
#' \displaystyle 1,  \text{ for } x\geq 1.
#' \end{cases}}{ascii}
#'
#' Then take the average of all constructed distributions of participants for each claim:
#'
#' \mjdeqn{AvDistribution = \frac{1}{N}\sum_{i=1}^N F_i(x),}{ascii}
#'
#' and the aggregation is the median of the average distribution:
#'
#' \mjdeqn{\hat{p}_c\left( DistribArMean \right) = AvDistribution^{-1}(0.5).}{ascii}
#'
#' **TriDistribArMean**: Applies a triangular distribution to the upper, lower and best estimates.
#'
#' A more restrictive fit with different assumptions about the
#' elicited best estimates, upper and lower bounds. We can assume that the lower and upper
#' bounds form the support of the distribution, and the best estimate corresponds to the mode.
#'
#' \mjdeqn{F_i(x)=
#' \begin{cases}
#' \displaystyle 0, \text{ for } x < L_{i} \cr
#' \displaystyle \frac{\left( x-L_{i}\right)^2}{\left( U_{i}-L_{i}\right)\left( B_{i}-L_{i}
#' \right)}, \text{ for } L_{i} \leq x < B_{i}\cr
#' \displaystyle 1 - \frac{\left( U_{i}-x\right)^2}{\left( U_{i}-L_{i}\right)\left
#' ( U_{i}-B_{i}\right)}, \text{ for } B_{i} < x < U_{i}\cr
#' \displaystyle 1,  \text{ for } x \geq U_{i}.
#' \end{cases}}{ascii}
#'
#' Then take the average of all constructed distributions of participants for each claim:
#'
#' \mjdeqn{    AvDistribution = \frac{1}{N}\sum_{i=1}^N F_i(x),}{ascii}
#'
#' and the aggregation is the median of the average distribution:
#'
#' \mjdeqn{    \hat{p}_c\left(TriDistribArMean\right) = AvDistribution^{-1}(0.5).}{ascii}
#'
#' @param expert_judgements A dataframe in the format of [data_ratings].
#' @param type One of `"DistribArMean"` or `"TriDistribArMean"`.
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
#' \dontrun{DistributionWAgg(data_ratings)}
#'
#' @export
#' @md

DistributionWAgg <- function(expert_judgements,
                             type = "DistribArMean",
                             name = NULL,
                             placeholder = FALSE,
                             percent_toggle = FALSE,
                             round_2_filter = TRUE) {

  if(!(type %in% c("DistribArMean",
                   "TriDistribArMean"))){

    stop('`type` must be one of "DistribArMean" or "TriDistribArMean"')

  }

  ## Set name argument

  name <- ifelse(is.null(name),
                 type,
                 name)

  cli::cli_h1(sprintf("DistributionWAgg: %s",
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
           "DistribArMean" = {

             Fx_fun <- function(x, lower, best, upper) {
               dplyr::case_when(
                 x < 0 ~ 0,
                 x >= 0 & x < lower ~ 0.05 / lower * x,
                 x >= lower &
                   x < best ~ 0.45 / (best - lower) * (x - lower) + 0.05,
                 x >= best &
                   x < upper ~ 0.45 / (upper - best) * (x - best) + 0.5,
                 x >= upper &
                   x < 1 ~ 0.05 / (1 - upper) * (x - upper) + 0.95,
                 x > 1 ~ 1
               )
             }

             avdist_fun <- function(dq, claim_input) {
               claim_input %>%
                 dplyr::mutate(Fx = purrr::pmap(
                   list(three_point_lower,
                        three_point_best,
                        three_point_upper),
                   .f = function(l, b, u) {
                     function(x) {
                       Fx_fun(x,

                              lower = l,
                              best = b,
                              upper = u)
                     }
                   }
                 ))  %>%
                 purrr::pluck("Fx") %>%
                 purrr::map_dbl(
                   .f = function(Fx_fun) {
                     Fx_fun(dq)
                   }
                 ) %>%
                 mean()
             }

           },
           "TriDistribArMean" = {

             Fx_fun <- function(x, lower, best, upper) {
               dplyr::case_when(
                 x < lower ~ 0,
                 x >= lower &
                   x < best ~ ((x-lower) ^ 2) / ((upper - lower) * (best - lower)),
                 x >= best &
                   x < upper ~ 1 - (((upper - x) ^ 2) / ((upper - lower) * (upper - best))),
                 x >= upper ~ 1
               )
             }

             avdist_fun <- function(dq, claim_input) {
               claim_input %>%
                 dplyr::mutate(Fx = purrr::pmap(
                   list(three_point_lower,
                        three_point_best,
                        three_point_upper),
                   .f = function(l, b, u) {
                     function(x) {
                       Fx_fun(x,

                              lower = l,
                              best = b,
                              upper = u)
                     }
                   }
                 ))  %>%
                 purrr::pluck("Fx") %>%
                 purrr::map_dbl(
                   .f = function(Fx_fun) {
                     Fx_fun(dq)
                   }
                 ) %>%
                 mean()
              }

           })

    agg_judge_df <- df  %>%
      tidyr::pivot_wider(names_from = element, values_from = value) %>%
      dplyr::mutate(
        three_point_upper = dplyr::if_else(
          three_point_upper == 1,
          three_point_upper - .Machine$double.eps,
          three_point_upper
        ),
        three_point_lower = dplyr::if_else(
          three_point_lower == 0,
          three_point_lower + .Machine$double.eps,
          three_point_lower
        ),
        # ensure the values are in order
        three_point_upper = dplyr::if_else(
          three_point_upper < three_point_best,
          three_point_best,
          three_point_upper
        ),
        three_point_lower = dplyr::if_else(
          three_point_lower > three_point_best,
          three_point_best,
          three_point_lower
        ),
      ) %>%
      dplyr::group_by(paper_id) %>%
      tidyr::nest() %>%
      dplyr::mutate(avdist = purrr::map(
        data,
        .f = function(data) {
          function(x) {
            avdist_fun(x, claim_input = data)
          }
        }
      )) %>% dplyr::ungroup()

    quantiles <- agg_judge_df %>%
      dplyr::mutate(avdist_preimage = purrr::map(
        avdist,
        .f = function(f) {
          GoFKernel::inverse(f, lower = 0, upper = 1)
        }
      )) %>%
      dplyr::mutate(aggregated_judgement =
                      purrr::map_dbl(
                        avdist_preimage,
                        .f = function(f) {
                          # suppressing warnings on preimage
                          f(0.5)
                        }
                      ))


    n_experts <- df %>%
      dplyr::group_by(paper_id, user_name) %>%
      dplyr::summarise(n_experts = dplyr::n(), .groups = "drop_last") %>%
      dplyr::count() %>%
      dplyr::rename("n_experts" = "n") %>%
      dplyr::ungroup()

    x <-  quantiles %>%
      dplyr::left_join(n_experts, by = "paper_id") %>%
      dplyr::select(paper_id, aggregated_judgement, n_experts) %>%
      dplyr::ungroup()

    x %>%
      dplyr::group_by(paper_id) %>%
      dplyr::mutate(method = name) %>%
      dplyr::ungroup() %>%
      postprocess_judgements()

  }
}
