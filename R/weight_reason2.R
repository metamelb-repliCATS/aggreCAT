#' @title
#' Weighting method: Total number and diversity of judgement reasons
#'
#' @description
#' This function is used by [ReasoningWAgg] to calculate weights for the aggregation
#' type `"ReasonWAgg2"`. Weights are based on the number and diversity of reasoning
#' methods used by the participant to support their judgement.
#'
#' @details
#' \loadmathjax
#' An individual's weight is a product of the number of reasons given in support of
#' their judgement and the diversity of these reasons.
#' \mjdeqn{w\_{varReason}_{i,c} =\sum_{r=1}^{R} \mathbf{CR_i}(c,r) \cdot (1 - \frac{\sum_{c=1}^C
#' \mathbf{CR_i}(c,r)}{C})}{ascii}
#'
#' @param expert_reasons A dataframe in the form of [data_supp_reasons]
#'
#' @return A tibble of three columns `paper_id`, `user_name`, and `reason_count` 
#'
#' @export

weight_reason2 <- function(expert_reasons) {

  ## Calculate reason frequency/proportion

  reason_freq <- expert_reasons %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("RW"),
                                .fns = ~dplyr::if_else(.x > 1, 1, .x))) %>%
    dplyr::select(-paper_id) %>%
    dplyr::group_by(user_name) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("RW"),
                                .fns = function(x) {1-x})) %>%
    dplyr::rename_with(.fn = function(x) {paste0(x, "_FREQ")},
                       .cols = dplyr::starts_with("RW"))

  ## Calculate reason counts

  expert_reasons %>%
    dplyr::left_join(reason_freq,
                     by = "user_name") %>%
    tidyr::pivot_longer(data = .,
                        cols = dplyr::starts_with("RW"),
                        names_to = "var.names",
                        values_to = "vals") %>%
    dplyr::mutate(var.names2 = stringr::str_remove(var.names, "_FREQ")) %>%
    dplyr::group_by(paper_id,
                    user_name,
                    var.names2) %>%
    dplyr::summarise(vals = dplyr::first(vals) * dplyr::last(vals)) %>%
    dplyr::group_by(paper_id,
                    user_name) %>%
    dplyr::summarise(reason_count = sum(vals) %>%
                       dplyr::na_if(., 0))

}
