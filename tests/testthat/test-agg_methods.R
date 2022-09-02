library(pointblank)


method_schema_default <- function(method_to_test, type){

  tbl <- data_ratings %>%
    method_to_test(type = {{type}}, percent_toggle = TRUE)

  test_that("column `method` exists", {

    expect_col_exists(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })


  test_that("column `paper_id` exists", {

    expect_col_exists(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("column `cs` exists", {

    expect_col_exists(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("column `n_experts` exists", {

    expect_col_exists(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })


  test_that("values in `n_experts` should be between `0` and `25`", {

    expect_col_vals_between(
      tbl,
      columns = vars(n_experts),
      left = 0,
      right = 25,
      threshold = 1
    )
  })


  test_that("values in `cs` should be between `0` and `1`", {

    expect_col_vals_between(
      tbl,
      columns = vars(cs),
      left = 0,
      right = 1,
      threshold = 1
    )
  })


  test_that("column `method` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("column `paper_id` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })


  test_that("column `n_experts` is of type: integer", {

    expect_col_is_integer(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

  test_that("all values in `method` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("all values in `paper_id` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("all values in `cs` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("all values in `n_experts` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

}

# Measures of unweighted aggregation
method_schema_default(AverageWAgg, type = "ArMean")
method_schema_default(AverageWAgg, type = "Median")
method_schema_default(AverageWAgg, type = "GeoMean")
method_schema_default(AverageWAgg, type = "LOArMean")
method_schema_default(AverageWAgg, type = "ProbitArMean")

method_schema_default(LinearWAgg, type = "DistLimitWAgg")
method_schema_default(LinearWAgg, type = "GranWAgg")
method_schema_default(LinearWAgg, type = "OutWAgg")

method_schema_default(IntervalWAgg, type = "IntWAgg")
method_schema_default(IntervalWAgg, type = "IndIntWAgg")
method_schema_default(IntervalWAgg, type = "AsymWAgg")
method_schema_default(IntervalWAgg, type = "IndIntAsymWAgg")
method_schema_default(IntervalWAgg, type = "VarIndIntWAgg")
method_schema_default(IntervalWAgg, type = "KitchSinkWAgg")

method_schema_default(DistributionWAgg, type = "DistribArMean")
method_schema_default(DistributionWAgg, type = "TriDistribArMean")

method_schema_default(ShiftingWAgg, type = "ShiftWAgg")
method_schema_default(ShiftingWAgg, type = "BestShiftWAgg")
method_schema_default(ShiftingWAgg, type = "IntShiftWAgg")
method_schema_default(ShiftingWAgg, type = "DistShiftWAgg")
method_schema_default(ShiftingWAgg, type = "DistIntShiftWAgg")



# Method Test Needing Additional Arguments --------------------------------


method_schema_linear_weights <- function(method_to_test, weights, type, name){

  tbl <- data_ratings %>%
    method_to_test(weights, type = {{type}} , name = {{name}}, percent_toggle = TRUE)

  test_that("column `method` exists", {

    expect_col_exists(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })


  test_that("column `paper_id` exists", {

    expect_col_exists(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("column `cs` exists", {

    expect_col_exists(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("column `n_experts` exists", {

    expect_col_exists(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })


  test_that("values in `n_experts` should be between `0` and `25`", {

    expect_col_vals_between(
      tbl,
      columns = vars(n_experts),
      left = 0,
      right = 25,
      threshold = 1
    )
  })


  test_that("values in `cs` should be between `0` and `1`", {

    expect_col_vals_between(
      tbl,
      columns = vars(cs),
      left = 0,
      right = 1,
      threshold = 1
    )
  })


  test_that("column `method` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("column `paper_id` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })


  test_that("column `n_experts` is of type: integer", {

    expect_col_is_integer(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

  test_that("all values in `method` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("all values in `paper_id` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("all values in `cs` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("all values in `n_experts` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

}



# LinearWAgg Additional Arguments -----------------------------------------

method_schema_linear_weights(LinearWAgg,
                             weights = data_supp_quiz %>%
                               dplyr::rename(weight = quiz_score),
                             type = "Participant_LO",
                             name = "QuizWAgg")

method_schema_linear_weights(LinearWAgg,
                             weights = data_supp_quiz %>%
                               dplyr::rename(weight = quiz_score_even),
                             type = "Participant_LO",
                             name = "QuizWAgg2")

method_schema_linear_weights(LinearWAgg,
                             weights =  data_supp_quiz %>%
                               dplyr::rename(weight = quiz_score_stats),
                             type = "Participant_LO",
                             name = "QuizWAgg3")

method_schema_linear_weights(LinearWAgg,
                             weights = data_ratings %>%
                               dplyr::filter(element == "three_point_best",
                                             round == "round_2") %>%
                               dplyr::select(paper_id,
                                             user_name,
                                             timestamp) %>%
                               dplyr::group_by(user_name) %>%
                               dplyr::arrange(timestamp) %>%
                               dplyr::mutate(claim_count = dplyr::row_number()) %>%
                               dplyr::ungroup() %>%
                               dplyr::mutate(weight = log(claim_count) + 1) %>%
                               dplyr::select(paper_id,
                                             user_name,
                                             weight),
                             type = "Judgement",
                             name = "ExperienceWAgg")

method_schema_linear_weights(LinearWAgg,
                             weights = data_ratings %>%
                               dplyr::filter(question == "comprehension",
                                             round == "round_2") %>%
                               dplyr::select(paper_id,
                                             user_name,
                                             value) %>%
                               dplyr::rename(weight = value),
                             type = "Judgement",
                             name = "CompWAgg")

method_schema_linear_weights(LinearWAgg,
                             weights = data_justifications %>%
                               dplyr::mutate(n_words = stringr::str_count(justification, pattern = " ") + 1) %>%
                               dplyr::group_by(user_name,
                                               paper_id) %>%
                               dplyr::summarise(word_count = sum(n_words,
                                                                 na.rm = TRUE)) %>%
                               dplyr::select(paper_id,
                                             user_name,
                                             word_count) %>%
                               dplyr::rename(weight = word_count),
                             type = "Judgement",
                             name = "EngWAgg")


# Reasoning ---------------------------------------------------------------


method_schema_reasoning <- function(method_to_test, reasons, type, name, beta_transform){

  tbl <- data_ratings %>%
    method_to_test(reasons, type = {{type}} , name = {{name}}, beta_transform = {{beta_transform}}, percent_toggle = TRUE)

  test_that("column `method` exists", {

    expect_col_exists(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })


  test_that("column `paper_id` exists", {

    expect_col_exists(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("column `cs` exists", {

    expect_col_exists(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("column `n_experts` exists", {

    expect_col_exists(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })


  test_that("values in `n_experts` should be between `0` and `25`", {

    expect_col_vals_between(
      tbl,
      columns = vars(n_experts),
      left = 0,
      right = 25,
      threshold = 1
    )
  })


  test_that("values in `cs` should be between `0` and `1`", {

    expect_col_vals_between(
      tbl,
      columns = vars(cs),
      left = 0,
      right = 1,
      threshold = 1
    )
  })


  test_that("column `method` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("column `paper_id` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })


  test_that("column `n_experts` is of type: integer", {

    expect_col_is_integer(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

  test_that("all values in `method` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("all values in `paper_id` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("all values in `cs` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("all values in `n_experts` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

}



method_schema_reasoning(ReasoningWAgg,
                        reasons = data_supp_reasons,
                        type = "ReasonWAgg",
                        name = "ReasonWAgg",
                        beta_transform = FALSE)

method_schema_reasoning(ReasoningWAgg,
                        reasons = data_supp_reasons,
                        type = "ReasonWAgg2",
                        name = "ReasonWAgg2",
                        beta_transform = FALSE)

method_schema_reasoning(ReasoningWAgg,
                        reasons = data_supp_reasons,
                        type = "ReasonWAgg",
                        name = "BetaReasonWAgg",
                        beta_transform = TRUE)

method_schema_reasoning(ReasoningWAgg,
                        reasons = data_supp_reasons,
                        type = "ReasonWAgg2",
                        name = "BetaReasonWAgg2",
                        beta_transform = TRUE)


# Bays --------------------------------------------------------------------



method_schema_bays <- function(method_to_test, priors, type, name){

  tbl <- data_ratings %>%
    method_to_test(priors, type = {{type}} , name = {{name}}, percent_toggle = TRUE)

  test_that("column `method` exists", {

    expect_col_exists(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })


  test_that("column `paper_id` exists", {

    expect_col_exists(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("column `cs` exists", {

    expect_col_exists(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("column `n_experts` exists", {

    expect_col_exists(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })


  test_that("values in `n_experts` should be between `0` and `25`", {

    expect_col_vals_between(
      tbl,
      columns = vars(n_experts),
      left = 0,
      right = 25,
      threshold = 1
    )
  })


  test_that("values in `cs` should be between `0` and `1`", {

    expect_col_vals_between(
      tbl,
      columns = vars(cs),
      left = 0,
      right = 1,
      threshold = 1
    )
  })


  test_that("column `method` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("column `paper_id` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })


  test_that("column `n_experts` is of type: integer", {

    expect_col_is_integer(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

  test_that("all values in `method` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("all values in `paper_id` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("all values in `cs` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("all values in `n_experts` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

}

method_schema_bays(BayesianWAgg, priors = NULL, type = "BayTriVar", name = "BayTriVar")

method_schema_bays(BayesianWAgg, priors = data_supp_priors, type = "BayPRIORsAgg", name = "BayTriVar")


# Extreme Wag -------------------------------------------------------------

method_schema_extreme <- function(method_to_test, type, name, cutoff_lower, cutoff_upper){


  tbl <- data_ratings %>%
    method_to_test(type = {{type}} , name = {{name}}, cutoff_lower = {{cutoff_lower}}, cutoff_upper = {{cutoff_upper}}, percent_toggle = TRUE)

  test_that("column `method` exists", {

    expect_col_exists(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })


  test_that("column `paper_id` exists", {

    expect_col_exists(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("column `cs` exists", {

    expect_col_exists(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("column `n_experts` exists", {

    expect_col_exists(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })


  test_that("values in `n_experts` should be between `0` and `25`", {

    expect_col_vals_between(
      tbl,
      columns = vars(n_experts),
      left = 0,
      right = 25,
      threshold = 1
    )
  })


  test_that("values in `cs` should be between `0` and `1`", {

    expect_col_vals_between(
      tbl,
      columns = vars(cs),
      left = 0,
      right = 1,
      threshold = 1
    )
  })


  test_that("column `method` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("column `paper_id` is of type: character", {

    expect_col_is_character(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })


  test_that("column `n_experts` is of type: integer", {

    expect_col_is_integer(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })

  test_that("all values in `method` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(method),
      threshold = 1
    )
  })

  test_that("all values in `paper_id` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(paper_id),
      threshold = 1
    )
  })

  test_that("all values in `cs` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(cs),
      threshold = 1
    )
  })

  test_that("all values in `n_experts` should not be NULL", {

    expect_col_vals_not_null(
      tbl,
      columns = vars(n_experts),
      threshold = 1
    )
  })


}

method_schema_extreme(ExtremisationWAgg,
                      type = "BetaArMean",
                      name = "BetaArMean",
                      cutoff_lower = NULL,
                      cutoff_upper = NULL)

method_schema_extreme(ExtremisationWAgg,
                      type = "BetaArMean2",
                      name = "BetaArMean2",
                      cutoff_lower = 0.4,
                      cutoff_upper = 0.6)






