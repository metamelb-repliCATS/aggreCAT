#' Confidence Score Ridge Plot
#'
#' Display a ridge plot of confidence scores for each aggregation method
#'
#' @param confidence_scores A data frame of confidence scores in long format in the form of [data_confidence_scores]
#'
#' @return A density ridge plot of aggregation methods
#'
#' @examples
#' \dontrun{confidence_scores_ridgeplot(data_confidence_scores)}
#'
#' @export

confidence_score_ridgeplot <- function(confidence_scores = NULL){
  
  if(is.null(confidence_scores)){
    cli::cli_alert_info("No Confidence Scores Provided. Using Package Default")
    confidence_scores <- aggreCAT::data_confidence_scores
  } else {
    confidence_scores <- confidence_scores
  }
  
  number_of_claims <- dplyr::n_distinct(confidence_scores$paper_id)
  
  confidence_scores %>%
    ggplot2::ggplot(ggplot2::aes(x = cs,
                                 y = factor(method,
                                            levels = rev(levels(factor(method)))),
                                 fill = factor(ggplot2::after_stat(quantile))
    )) +
    ggridges::stat_density_ridges(
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantiles = c(0.025, 0.975)) +
    ggplot2::scale_fill_manual(
      name = "Probability", 
      values = c("#D55E00", "#A0A0A0A0", "#0072B2"), # colour friendly
      labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
    ggplot2::scale_x_continuous(breaks = c(0, .25, .5, .75, 1)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::geom_vline(xintercept = .50,
                        linetype = "dashed",
                        color = "black",
                        alpha = 1,
                        show.legend = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Density of Aggregated Confidence Scores",
                  subtitle = paste0("Claims Assessed N = ", number_of_claims),
                  x = "Confidence Scores",
                  y = "Aggregation Method")
}

