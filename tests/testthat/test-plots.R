
# Test Plot Functions -----------------------------------------------------


test_that("Plots Labelled Correctly'",{
  p <- confidence_score_ridgeplot(confidence_scores = data_confidence_scores)
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$y, "Aggregation Method")
  expect_identical(p$labels$x, "Predicted Replication Scores")
  expect_identical(p$labels$colour, "cs")
})


test_that("Plots Labelled Correctly'",{
  p <- confidence_score_heatmap(confidence_scores = data_confidence_scores,
                                data_outcomes = data_outcomes)
  expect_true(ggplot2::is.ggplot(p))

})
