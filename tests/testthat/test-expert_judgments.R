library(pointblank)

cli::cli_alert_info("Testing Data Assets")

tbl <- data_ratings

test_that("column `round` exists", {
  
  pointblank::expect_col_exists(
    tbl,
    columns = c("round"),
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

test_that("column `user_name` exists", {
  
  pointblank::expect_col_exists(
    tbl,
    columns = c("user_name"),
    threshold = 1
  )
})

test_that("column `question` exists", {
  
  pointblank::expect_col_exists(
    tbl,
    columns = c("question"),
    threshold = 1
  )
})

test_that("column `element` exists", {
  
  pointblank::expect_col_exists(
    tbl,
    columns = c("element"),
    threshold = 1
  )
})

test_that("column `value` exists", {
  
  pointblank::expect_col_exists(
    tbl,
    columns = c("value"),
    threshold = 1
  )
})

test_that("column `round` is of type: character", {
  
  pointblank::expect_col_is_character(
    tbl,
    columns = c("round"),
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

test_that("column `user_name` is of type: character", {
  
  pointblank::expect_col_is_character(
    tbl,
    columns = c("user_name"),
    threshold = 1
  )
})

test_that("column `question` is of type: character", {
  
  pointblank::expect_col_is_character(
    tbl,
    columns = c("question"),
    threshold = 1
  )
})

test_that("column `element` is of type: character", {
  
  pointblank::expect_col_is_character(
    tbl,
    columns = c("element"),
    threshold = 1
  )
})


test_that("column `value` is of type: numeric", {
  
  pointblank::expect_col_is_numeric(
    tbl,
    columns = c("value"),
    threshold = 1
  )
})

test_that("values in `value` should be between `-1` and `100`", {
  
  pointblank::expect_col_vals_between(
    tbl,
    columns = c("value"),
    left = -1,
    right = 100,
    threshold = 1
  )
})

test_that("column `group` is of type: character", {
  
  pointblank::expect_col_is_character(
    tbl,
    columns = c("group"),
    threshold = 1
  )
})