#' Placeholder function with TA2 output
#'
#' This functions stands in for when we haven't completed coding the method.
#'
#' This function expects input from [preprocess_judgements] and outputs
#' for [postprocess_judgements].
#'
#' @param expert_judgements A data frame in the form of ratings
#' @param method_name Aggregation method to place into placeholder mode
#'
#' @examples
#' \dontrun{method_placeholder(data_ratings, method_name = "TestMethod")}
#'
#' @return A tibble of confidence scores `cs` for each `paper_id`.
#'
#' @export

method_placeholder <- function(expert_judgements, method_name) {

  placeholder = TRUE

  cli::cli_alert_info("Placeholder: {.val {placeholder}}")

  expert_judgements %>%
    preprocess_judgements() %>%
    dplyr::filter(element == "three_point_best") %>%
    dplyr::group_by(paper_id) %>%
    dplyr::summarise(cs = 0.65,
                     n_experts = 0) %>%
    dplyr::mutate(method = {{method_name}}) %>%
    dplyr::select(method, paper_id, cs, n_experts)
}

