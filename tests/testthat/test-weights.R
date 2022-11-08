weight_nans <- function(method_to_test) {
  agg_weight <-
    data_ratings %>%
    preprocess_judgements() %>%
    method_to_test() %>%
    purrr::pluck("agg_weight")

  expect_false(any(is.na(agg_weight)))
  expect_false(any(is.nan(agg_weight)))
  expect_false(any(is.infinite(agg_weight)))
  expect_type(agg_weight, "double")

}



# test weights ------------------------------------------------------------

test_that("weight_asym method", {
  weight_nans(weight_asym)
})

test_that("weight_interval method", {
  weight_nans(weight_interval)
})

test_that("weight_nIndivInterval method", {
  weight_nans(weight_nIndivInterval)
})

test_that("weight_outlier method", {
  weight_nans(weight_outlier)
})

test_that("weight_varIndivInterval method", {
  weight_nans(weight_varIndivInterval)
})

