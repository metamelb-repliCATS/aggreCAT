#' @title
#' Pre-process the data
#'
#' @description
#' Process input data with filters and meaningful variable names.
#'
#' This function is called at the head of every aggregation method function.
#'
#' @details
#' This pre-processing function takes input data in the format of
#' [data_ratings] and outputs a dataframe that:
#'
#' 1. Applies any filters or manipulations required by the aggregation method.
#' 2. Converts the input data into variables with more
#' meaningful names for coding, to avoid errors in the wrangling process.
#'
#' @param expert_judgements A dataframe with the same variables (columns) as
#' [data_ratings].
#' @param round_2_filter Note that the IDEA protocol results in both a Round 1
#' and Round 2 set of probabilities for each claim. Unless otherwise specified,
#' we will assume that the final Round 2 responses (after discussion) are being
#' referred to.
#' @param three_point_filter Defaults `TRUE` to filter three point estimates. `FALSE` will
#' filter the involved_binary question.
#' @param percent_toggle Change the values to probabilities from percentages. Default is `FALSE`
#'
#' @return a long tibble of expert judgements, with six columns:
#'  `round`, `paper_id`, `user_name`, `element` (i.e. question type),
#'  and `value` (i.e. participant response).
#'
#' @examples
#' \dontrun{preprocess_judgements(data_ratings)}
#'
#' @export

preprocess_judgements <- function(expert_judgements,
                                  round_2_filter = TRUE,
                                  three_point_filter = TRUE,
                                  percent_toggle = FALSE){


  # Processing Alerts -------------------------------------------------------


  cli::cli_h2("Pre-Processing Options")
  cli::cli_alert_info("Round Filter: {.val {round_2_filter}}")
  cli::cli_alert_info("Three Point Filter: {.val {three_point_filter}}")
  cli::cli_alert_info("Percent Toggle: {.val {percent_toggle}}")

  if(any(is.na(expert_judgements$value))) cli::cli_abort("NAs Found in Values")

  # Variables of focus
  expert_judgements <- expert_judgements %>%
    dplyr::select(round,
           paper_id,
           user_name,
           element,
           value)


  # Round Filter ------------------------------------------------------------


  filter_round <- function(expert_judgements, round_2_filter){

    output_df <-  if(round_2_filter){
      expert_judgements %>%
        dplyr::filter(round %in% "round_2")
    } else {
      expert_judgements
    }
  }


  # Three Point Filter ------------------------------------------------------


  filter_element <- function(expert_judgements, three_point_filter){
    # Default
    # Filters to three point estimates
    # Otherwise removes the involved binary
    output_df <- if(three_point_filter){
      expert_judgements %>%
        dplyr::group_by(round, paper_id, user_name) %>%
        dplyr::filter(element %in% c("three_point_best",
                                     "three_point_lower",
                                     "three_point_upper"))
    } else {
      expert_judgements %>%
        dplyr::filter(expert_judgements != "involved_binary")
    }

  }


  # Convert Values ----------------------------------------------------------


  change_value <- function(expert_judgements, percent_toggle){
    # Converts values to 0,1
    output_df <- if(percent_toggle){
      expert_judgements %>%
        dplyr::mutate(value =
                        dplyr::case_when(
                          element %in% c("three_point_best",
                                         "three_point_lower",
                                         "three_point_upper") ~ value / 100,
                          TRUE ~ value
                        ))
    } else {
      expert_judgements
    }

  }


  # Processing Data Frame ---------------------------------------------------


  method_out <-  expert_judgements %>%
    filter_round(round_2_filter) %>%
    filter_element(three_point_filter) %>%
    change_value(percent_toggle) %>%
    dplyr::bind_rows() %>%
    dplyr::ungroup()

  return(method_out)

}

