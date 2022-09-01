gt_tbl <-
  readxl::read_xlsx("table_1_agg_methods_summary.xlsx") %>%
  dplyr::mutate(aggregator_function = paste0("`",aggregator_function , "`"),
                type = paste0("`",type , "`"),
                aggregator_fun_desc = paste0("*", aggregator_fun_desc, "*")) %>%
  tidyr::unite(agg_name_description, aggregator_function, aggregator_fun_desc, sep = "<br>") %>%
  # dplyr::group_by(agg_name_description) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns =c(agg_name_description,
                              type,
                              type_desc,
                              supp_data_requirements,
                              weighting_fn,
                              judgement_data_sources)) %>%
  gt::cols_label(agg_name_description = gt::md("Aggregator Function<br>& Short Description"),
                 type = gt::md("Aggregator Type"),
                 supp_data_requirements = gt::md("Supp. Data<br>Requirements"),
                 weighting_fn = gt::md("Weighting<br>Function"),
                 number_rounds = gt::md("Number of Elicitation<br>Rounds"),
                 elicitation_method = gt::md("Elicitation Method"),
                 judgement_data_sources = gt::md("Judgment Data<br>Sources")) %>%

  gt::tab_row_group(label = gt::md("`AverageWAgg()` *Averaged best-estimates*"),
                    rows = contains(match = "AverageWAgg", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("`LinearWAgg()` *Linearly-weighted best-estimates*"),
                    rows = contains(match = "LinearWAgg()", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("`IntervalWAgg()` *Linearly-weighted best estimates, with weights influenced by interval widths*"),
                    rows = contains("IntervalWAgg()", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("`ShiftingWAgg()` *Weighted by judgements that shift most after discussion*"),
                    rows = contains("ShiftingWAgg()", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("`ReasoningWAgg()` *Linearly-weighted best estimates, with weights constructed from supplementary reasoning data*"),
                    rows = contains("ReasoningWAgg()", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("`ExtremisationWAgg()` *Takes the average of best-estimates and transforms it using the cumulative distribution function of a beta distribution.*"),
                    rows = contains("ExtremisationWAgg()", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("`DistributionWAgg()` *Calculates the arithmetic mean of distributions created from expert judgements. The aggregate is the median of the average distribution fitted to individual estimates.*"),
                    rows = contains("DistributionWAgg()", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("`BayesianWAgg()` *Bayesian aggregation methods with either uninformative or informative prior distributions.*"),
                    rows = contains("BayesianWAgg()", vars = agg_name_description)) %>%
  gt::tab_row_group(label = gt::md("Do These fns exist anymore?"),
                    rows = contains("???", vars = agg_name_description)) %>%
  gt::cols_hide(agg_name_description) %>%
  sub_missing(columns=everything(), rows = everything(),
              missing_text="") %>%
  gtExtras::gt_theme_guardian()
# gt_tbl
gt::gtsave(gt_tbl, "table_1_summary_agg_methods.png",path = here::here("./test/images"))
# TODO: NEXT: widen columns with lots of text to make
# Increase table width ~ landscape style?
# render equations in last column
