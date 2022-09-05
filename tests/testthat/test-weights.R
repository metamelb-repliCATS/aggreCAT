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


# Test Experience ---------------------------------------------------------

#
#
# tbl <- weight_experience(data_ratings, round_for_agg = "round_1")
#
# test_that("column `paper_id` exists", {
#
#   expect_col_exists(
#     tbl,
#     columns = vars(paper_id),
#     threshold = 1
#   )
# })
#
# test_that("column `user_name` exists", {
#
#   expect_col_exists(
#     tbl,
#     columns = vars(user_name),
#     threshold = 1
#   )
# })
#
# test_that("column `weight` exists", {
#
#   expect_col_exists(
#     tbl,
#     columns = vars(weight),
#     threshold = 1
#   )
# })
#
# test_that("column `weight` is of type: numeric", {
#
#   expect_col_is_numeric(
#     tbl,
#     columns = vars(weight),
#     threshold = 1
#   )
# })
#
# test_that("column `paper_id` is of type: character", {
#
#   expect_col_is_character(
#     tbl,
#     columns = vars(paper_id),
#     threshold = 1
#   )
# })
#
# test_that("column `user_name` is of type: character", {
#
#   expect_col_is_character(
#     tbl,
#     columns = vars(user_name),
#     threshold = 1
#   )
# })
