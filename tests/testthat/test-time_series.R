# Test that time-series returns a data frame
test_that("time_series returns a data frame", {
  input <- "tests/testthat/test-inputs/indices"
  output <- "tests/testthat/tmp/timeseries"
  ts_df <- time_series(input, output)
  expect_is(ts_df, "data.frame")
  expect_true(nrow(ts_df) > 0)
  expect_true(length(list.files(output, pattern = "^TS.", full.names = TRUE)) > 0)
  unlink(output, recursive = TRUE)
})
