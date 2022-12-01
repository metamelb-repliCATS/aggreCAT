#' Confidence Score Ridge Plot
#'
#' Display a ridge plot of confidence scores for each aggregation method faceted by its linear, non-linear
#' and Bayesian characteristic.
#'
#' @param confidence_scores A data frame of confidence scores in long format in the form of [data_confidence_scores]
#'
#' @return A density ridge plot of aggregation methods
#'
#' @examples
#' \dontrun{confidence_scores_ridgeplot(data_confidence_scores)}
#'
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

  confidence_scores <- method_types(confidence_scores)

  confidence_scores %>%
    ggplot2::ggplot(ggplot2::aes(x = cs,
                                 y = factor(method,
                                            levels = rev(levels(factor(method)))),
                                 fill = factor(stat(quantile))
    )) +
    ggridges::stat_density_ridges(
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantiles = c(0.025, 0.975)) +
    ggplot2::scale_fill_manual(
      name = "Probability", values = c("#D55E00", "#A0A0A0A0", "#0072B2"), # colour friendly
      labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
    ggplot2::scale_x_continuous(breaks = c(0, .25, .5, .75, 1)) +
    ggplot2::scale_y_discrete(expand = c(0,0)) +
    ggplot2::geom_vline(xintercept=.50,
                        linetype="dashed",
                        color = "white",
                        alpha = .75,
                        show.legend = TRUE) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(face = "bold"),
                   axis.line = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "transparent",
                                                            colour = NA),
                   plot.background = ggplot2::element_rect(fill = "white",
                                                           colour = NA),
                   strip.placement = "outside",
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"),
                   plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot") +
    ggplot2::labs(title = "Density of Aggregated Confidence Scores",
                  subtitle = paste0("Claims Assessed N = ", number_of_claims),
                  x = "Predicted Replication Scores",
                  y = "Aggregation Method",
                  caption = "Prediction cutoff threshold depicted by dashed line (0.5)") +
    ggforce::facet_row(~type, scales = "free", space = "fixed")
}


#' Confidence Score Heat Map
#'
#' Confidence scores displayed on a colour spectrum across generated methods and assessed claims,
#' split into predicted replication outcomes (TRUE/FALSE). White indicative of around `.5` with higher predicted
#' confidence scores more blue (`>.5`) and lower more red (`<.5`). Each predicted replication outcome is then
#' split into the group type of the underlying statistical characteristic for each aggregation method (non-weighted linear,
#' weighted linear & Bayesian).
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
                                     data_outcomes = NULL,
                                     x_label = NULL){


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

  # formatting

  if(!is.null(x_label)) {
    x_label <- insight::format_capitalize(x_label)
  }

  data_confidencescores_lf <- confidence_scores

  # object for checking the number of claims
  number_of_claims <- dplyr::n_distinct(data_confidencescores_lf$paper_id)

  # object to throw a display warning
  number_of_methods <- dplyr::n_distinct(data_confidencescores_lf$method)

  if(number_of_claims <= 10) cli::cli_alert_warning("Claims less than ~ 10 might not display effectively.")

  data_cs_with_outcomes <- data_confidencescores_lf %>%
    dplyr::left_join(data_outcomes, by = "paper_id") %>%
    dplyr::mutate(replicated_outcome = ifelse(outcome == 1, "TRUE", "FALSE")) %>%
    dplyr::select(paper_id, method, cs, outcome, replicated_outcome)

  # Generate accuracy scores
  evaluated_outcomes <- aggreCAT::confidence_score_evaluation(confidence_scores = data_confidencescores_lf,
                                                              outcomes = data_outcomes)

  # Labels names
  labels_repl <- c("Replicated", "Did Not Replicate")
  names(labels_repl) <- c("TRUE", "FALSE")

  # Aggregation type grouping
  evaluated_outcomes <- method_types(evaluated_outcomes)

  # predicted replication TRUE
  plot1 <- evaluated_outcomes %>%
    dplyr::left_join(data_cs_with_outcomes) %>%
    dplyr::filter(replicated_outcome == "TRUE") %>%
    dplyr::mutate(AUC = round(AUC, digits = 3)) %>%
    dplyr::mutate(Brier_Score = round(Brier_Score, digits = 2)) %>%
    dplyr::mutate(paper_id = forcats::fct_reorder(paper_id, -cs)) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = paper_id,
                 y = factor(method, levels = rev(levels(factor(method)))),
                 fill = cs,
                 colour = cs) +
    ggplot2::geom_tile(linetype = 1, linejoin = "mitre") +
    ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = 1) +
    ggplot2::scale_color_distiller(palette = "RdYlBu", direction = 1) +
    ggplot2::labs(x = "",
                  y = "Aggregation Method",
                  title = "",
                  subtitle = "Predicted Outcome: TRUE",
                  caption = "",
                  fill = "Confidence \nScore", color = "Confidence \nScore") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 75, hjust = 1),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "none",
      legend.text = ggplot2::element_text(),
      legend.title =  ggplot2::element_text(),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggforce::facet_col(~type, scales = "free_y", space = "free")

  # predicted replication FALSE
  plot2 <- evaluated_outcomes %>%
    dplyr::left_join(data_cs_with_outcomes) %>%
    dplyr::filter(replicated_outcome == "FALSE") %>%
    dplyr::mutate(AUC = round(AUC, digits = 3)) %>%
    dplyr::mutate(Brier_Score = round(Brier_Score, digits = 2)) %>%
    dplyr::mutate(paper_id = forcats::fct_reorder(paper_id, -cs)) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = paper_id, y = factor(method, levels = rev(levels(factor(method)))),
                 fill = cs,
                 colour = cs) +
    ggplot2::geom_tile(linetype = 1, linejoin = "mitre") +
    ggplot2::scale_fill_distiller(palette = "RdYlBu", direction = 1) +
    ggplot2::scale_color_distiller(palette = "RdYlBu", direction = 1) +
    ggplot2::labs(x = "",
                  y = "Aggregation Method",
                  title = "",
                  subtitle = "Predicted Outcome: FALSE",
                  caption = "",
                  fill = "Confidence \nScore", color = "Confidence \nScore") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 75, hjust = 1),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(),
      legend.position = "none",
      legend.text = ggplot2::element_text(),
      legend.title =  ggplot2::element_text(),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    ggforce::facet_col(~type, scales = "free_y", space = "free")

  figure <- ggpubr::ggarrange(plot1, plot2,
                              #labels = c("A", "B"),
                              #font.label = font,
                              common.legend = TRUE,
                              legend = "right",
                              heights = c(2,1),
                              widths = c(1.23, 1))

  if(number_of_methods <= 5) cli::cli_alert_warning("Provide more than 5 methods for improved contrasting plot")

  ggpubr::annotate_figure(figure,
                          bottom = ggpubr::text_grob(x_label,

                                                     just = "centre"))
}


method_types <- function(conf_scores){

  # Aggregation type grouping
  NWL = c("ArMean", "GeoMean", "Median", "LOArMean", "LOGeoMean", "ProbitArMean")
  WLCI = c("ShiftWAgg", "BestShiftWAgg", "IntShiftWAgg", "DistShiftWAgg", "DistIntShiftWAgg",
           "IntWAgg", "IndIntWAgg", "AsymWAgg", "IndIntAsymWAgg", "VarIndIntWAgg", "KitchSinkWAgg",
           "DistLimitWAgg", "GranWAgg", "OutWAgg", "BetaArMean", "BetaArMean2", "DistribArMean", "TriDistribArMean",
           "CompWAgg", "EngWAgg","ReasonWAgg", "ReasonWAgg2", "QuizWAgg", "QuizWAgg2", "QuizWAgg3",
           "BetaReasonWAgg", "BetaReasonWAgg2", "BadgeWAgg",
           "ExperienceWAgg", "ExperienceWAgg2", "ExperienceWAgg3")
  BAYES = c("BayTriVar", "BayPRIORsAgg")

  conf_scores <- conf_scores %>%
    dplyr::mutate(type = dplyr::case_when(method %in% NWL ~ "Non-weighted Linear Combintation",
                                          method %in% WLCI ~ "Weighted Linear Combinations",
                                          #method %in% WLCE ~ "Weighted Linear Combinations (Supplementary Data)",
                                          method %in% BAYES ~ "Bayesian Methods"))


  # Levels for the plot output
  conf_scores$type <- factor(conf_scores$type,
                             levels = c("Non-weighted Linear Combintation",
                                        "Weighted Linear Combinations",
                                        #"Weighted Linear Combinations (Supplementary Data)",
                                        "Bayesian Methods"))

  return(conf_scores)

}
