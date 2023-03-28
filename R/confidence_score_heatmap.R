#' Confidence Score Heat Map
#'
#' Confidence scores displayed on a colour spectrum across generated methods and 
#' assessed claims, split into predicted replication outcomes (TRUE/FALSE).
#'
#' @param confidence_scores A data frame of confidence scores generated from the aggregation methods in the form of [data_confidence_scores].
#' Defaults to [data_confidence_scores] if no argument supplied.
#' @param data_outcomes A data frame of unique claims and the associated binary outcome in the form of [data_outcomes]. If no argument supplied
#' then defaults to [data_outcomes] supplied within package.
#' @param x_label Bottom x axis label name or ID. Default is blank.
#'
#' @importFrom insight format_capitalize
#'
#' @return Plot in viewer
#'
#' @examples
#' \dontrun{confidencescore_heatmap(data_confidence_scores, data_outcomes)}
#'
#' @export

confidence_score_heatmap <- function(confidence_scores = NULL,
                                     data_outcomes = NULL){
  
  
  # Defaults to example dataset if none provided
  if(is.null(confidence_scores)){
    cli::cli_alert_info("No Confidence Scores Provided. Using Package Default")
    confidence_scores <- aggreCAT::data_confidence_scores
  } else {
    confidence_scores <- confidence_scores
  }
  
  if(is.null(data_outcomes)){
    cli::cli_alert_info("No Outcome Scores Provided. Using Package Default")
    data_outcomes <- aggreCAT::data_outcomes
  } else {
    data_outcomes <- data_outcomes
  }
  
  # object for checking the number of claims
  number_of_claims <- dplyr::n_distinct(confidence_scores$paper_id)
  
  # object to throw a display warning
  number_of_methods <- dplyr::n_distinct(confidence_scores$method)
  
  if(number_of_claims <= 10) cli::cli_alert_warning("Claims less than ~ 10 might not display effectively.")
  
  data_cs_with_outcomes <- confidence_scores %>%
    dplyr::left_join(data_outcomes, by = "paper_id") %>%
    dplyr::mutate(replicated_outcome = ifelse(outcome == 1, "TRUE", "FALSE")) %>%
    dplyr::select(paper_id, 
                  method, 
                  cs, 
                  outcome, 
                  replicated_outcome)
  
  # Generate accuracy scores
  evaluated_outcomes <- aggreCAT::confidence_score_evaluation(confidence_scores = data_confidencescores_lf,
                                                              outcomes = data_outcomes)
  
  # Plot
  
  evaluated_outcomes %>%
    dplyr::left_join(data_cs_with_outcomes) %>%
    # dplyr::filter(replicated_outcome == "FALSE") %>%
    dplyr::mutate(AUC = round(AUC, 
                              digits = 3),
                  Brier_Score = round(Brier_Score, 
                                      digits = 2),
                  paper_id = forcats::fct_reorder(paper_id, -cs),
                  replicated_outcome = paste0("Outcome: ",
                                              replicated_outcome),
                  replicated_outcome = factor(replicated_outcome,
                                              levels = c("Outcome: TRUE",
                                                         "Outcome: FALSE"))) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = paper_id, 
                 y = factor(method, 
                            levels = rev(levels(factor(method)))),
                 fill = cs,
                 colour = cs) +
    ggplot2::geom_tile(linetype = 1, 
                       linejoin = "mitre") +
    ggplot2::scale_fill_distiller(palette = "RdYlBu", 
                                  direction = 1, 
                                  breaks = seq(0,1,0.2)) +
    ggplot2::scale_color_distiller(palette = "RdYlBu", 
                                   direction = 1, 
                                   breaks = seq(0,1,0.2)) +
    ggplot2::labs(x = "Paper ID",
                  y = "Aggregation Method",
                  title = "",
                  fill = "Confidence \nScore", color = "Confidence \nScore") +
    ggplot2::theme(axis.text.x = element_text(angle = 90,
                                              hjust = 1,
                                              vjust = 0.5)) +
    ggplot2::facet_wrap(~replicated_outcome,
                        scales = "free_x")
  
}
