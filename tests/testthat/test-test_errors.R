
test_that("AverageWAgg Expected Errors", {

  expect_error(AverageWAgg(expert_judgements = data_ratings,
                           type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "ArMean", "GeoMean", "Median", "LOArMean", "LOGeoMean", or "ProbitArMean"')
})

test_that("AverageWAgg Expected Prob Judgements Error", {

  expect_error(AverageWAgg(expert_judgements = data_ratings,
                           type = "LOArMean", percent_toggle = FALSE),
               "LOArMean requires probabilistic judgements. Check your data compatability or `percent_toggle` argument.")
})


test_that("LinearWAgg Expected Errors", {

  expect_error(LinearWAgg(expert_judgements = data_ratings,
                           type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "Judgement", "Judgement_LO", "Participant", "Participant_LO", "DistLimitWAgg", "GranWAgg", or "OutWAgg"')
})

test_that("LinearWAgg Expected Prob Judgements Error", {

  expect_error(LinearWAgg(expert_judgements = data_ratings,
                           type = "DistLimitWAgg", percent_toggle = FALSE),
               "DistLimitWAgg requires judgements bounded 0-1. Check your data compatability or `percent_toggle` argument.")
})

test_that("LinearWAgg Expected Prob Judgements Error", {

  expect_error(LinearWAgg(expert_judgements = data_ratings,
                          type = "GranWAgg", percent_toggle = FALSE),
               "GranWAgg requires probabilistic judgements. Check your data compatability or `percent_toggle` argument.")
})

test_that("DistributionWAgg Expected Errors", {

  expect_error(DistributionWAgg(expert_judgements = data_ratings,
                           type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "DistribArMean" or "TriDistribArMean"')
})

test_that("ExtremisationWAgg Expected Errors", {

  expect_error(ExtremisationWAgg(expert_judgements = data_ratings,
                                type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "BetaArMean" or "BetaArMean2')
})

test_that("IntervalWAgg Expected Errors", {

  expect_error(IntervalWAgg(expert_judgements = data_ratings,
                          type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "IntWAgg", "IndIntWAgg", "AsymWAgg", "IndIntAsymWAgg", "VarIndIntWAgg" or "KitchSinkWAgg"')
})

test_that("ReasoningWAgg Expected Errors", {

  expect_error(ReasoningWAgg(expert_judgements = data_ratings, reasons = data_supp_ReasonWAgg,
                            type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "ReasonWAgg" or "ReasonWAgg2"')
})

test_that("ShiftingWAgg Expected Errors", {

  expect_error(ShiftingWAgg(expert_judgements = data_ratings,
                             type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "ShiftWAgg", "BestShiftWAgg", "IntShiftWAgg", "DistShiftWAgg", or "DistIntShiftWAgg"')
})


test_that("BayesianWAgg Expected Errors", {

  expect_error(ShiftingWAgg(expert_judgements = data_ratings,
                            type = "WrongType", percent_toggle = TRUE),
               '`type` must be one of "ShiftWAgg", "BestShiftWAgg", "IntShiftWAgg", "DistShiftWAgg", or "DistIntShiftWAgg"')
})


# Error for less than 2 claims provided

one_claim <- data_ratings %>%
  dplyr::filter(paper_id == "czttvy")

# test_that("BayTriVar stops on less than 2 ids provided", {
# 
#   expect_error(BayesianWAgg(expert_judgements = one_claim,
#                             type = "BayTriVar",
#                             percent_toggle = TRUE),
#                'Model requires n > 1 ids to successfully execute.')
# })
# 
# test_that("BayPRIORsAgg stops on less than 2 ids provided", {
# 
#   expect_error(BayesianWAgg(expert_judgements = one_claim,
#                             priors = data_priors,
#                             type = "BayPRIORsAgg",
#                             placeholder = FALSE,
#                             percent_toggle = TRUE),
#                'Model requires n > 1 ids to successfully execute.')
# })
