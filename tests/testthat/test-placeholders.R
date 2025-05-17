
# Place holder Unit Tests Function -----------------------------------------


placeholder_test <- function(method_to_test, type){


  tbl <- data_ratings %>%
    method_to_test(placeholder = TRUE, type = {{type}})

  test_that("column `method` exists", {

    pointblank::expect_col_exists(
      tbl,
      columns = c("method"),
      threshold = 1
    )
  })

  test_that("column `paper_id` exists", {

    pointblank::expect_col_exists(
      tbl,
      columns = c("paper_id"),
      threshold = 1
    )
  })

  test_that("column `cs` exists", {

    pointblank::expect_col_exists(
      tbl,
      columns = c("cs"),
      threshold = 1
    )
  })

  test_that("column `n_experts` exists", {

    pointblank::expect_col_exists(
      tbl,
      columns = c("n_experts"),
      threshold = 1
    )
  })

  test_that("values in `cs` should be == `0.65`", {

    pointblank::expect_col_vals_equal(
      tbl,
      columns = c("cs"),
      value = 0.65,
      threshold = 1
    )
  })

  test_that("values in `n_experts` should be == `0`", {

    pointblank::expect_col_vals_equal(
      tbl,
      columns = c("n_experts"),
      value = 0,
      threshold = 1
    )
  })

  test_that("column `method` is of type: character", {

    pointblank::expect_col_is_character(
      tbl,
      columns = c("method"),
      threshold = 1
    )
  })

  test_that("column `paper_id` is of type: character", {

    pointblank::expect_col_is_character(
      tbl,
      columns = c("paper_id"),
      threshold = 1
    )
  })

}


# Place holder Tests -------------------------------------------------------

placeholder_test(AverageWAgg, type = "ArMean")
placeholder_test(AverageWAgg, type = "Median")
placeholder_test(AverageWAgg, type = "GeoMean")
placeholder_test(AverageWAgg, type = "LOArMean")
placeholder_test(AverageWAgg, type = "ProbitArMean")
placeholder_test(LinearWAgg, type = "DistLimitWAgg")
placeholder_test(LinearWAgg, type = "GranWAgg")
placeholder_test(LinearWAgg, type = "OutWAgg")
placeholder_test(IntervalWAgg, type = "IntWAgg")
placeholder_test(IntervalWAgg, type = "IndIntWAgg")
placeholder_test(IntervalWAgg, type = "AsymWAgg")
placeholder_test(IntervalWAgg, type = "IndIntAsymWAgg")
placeholder_test(IntervalWAgg, type = "VarIndIntWAgg")
placeholder_test(IntervalWAgg, type = "KitchSinkWAgg")
placeholder_test(DistributionWAgg, type = "DistribArMean")
placeholder_test(DistributionWAgg, type = "TriDistribArMean")
placeholder_test(ShiftingWAgg, type = "ShiftWAgg")
placeholder_test(ShiftingWAgg, type = "BestShiftWAgg")
placeholder_test(ShiftingWAgg, type = "IntShiftWAgg")
placeholder_test(ShiftingWAgg, type = "DistShiftWAgg")
placeholder_test(ShiftingWAgg, type = "DistIntShiftWAgg")
placeholder_test(ExtremisationWAgg, type = "BetaArMean")

placeholder_test(LinearWAgg, type = "Participant_LO")
placeholder_test(LinearWAgg, type = "Participant")
placeholder_test(LinearWAgg, type = "Judgement")

placeholder_test(ReasoningWAgg, type = "ReasonWAgg")
placeholder_test(ReasoningWAgg, type = "ReasonWAgg2")

# placeholder_test(BayesianWAgg, type = "BayTriVar")
# placeholder_test(BayesianWAgg, type = "BayPRIORsAgg")

placeholder_test(ExtremisationWAgg, type = "BetaArMean")
placeholder_test(ExtremisationWAgg, type = "BetaArMean2")
