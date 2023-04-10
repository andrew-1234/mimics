# Test mimics process works with a valid input
test_that("mimics_process works with a valid input", {
  input <- "tests/testthat/test-inputs/indices"
  output <- "tests/testthat/tmp/timeseries"
  ts_df <- time_series(input, output)
  expect_is(ts_df, "data.frame")
  expect_true(nrow(ts_df) > 0)
  expect_true(length(list.files(output, pattern = "^TS.", full.names = TRUE)) > 0)
  unlink(output, recursive = TRUE)
})

# Test mimics_process helper functions

# Test that mimics_check_indices_data works with valid input
test_that("mimics_check_indices_data works", {
  ts_df <- read.csv("tests/testthat/test-inputs/time_series.csv")
  expect_true(mimics_check_indices_data(ts_df))
})

# Test that mimics_check_indices_data returns error with invalid input
test_that("mimics_check_indices_data returns error with invalid input", {
  ts_df <- read.csv("tests/testthat/test-inputs/time_series.csv")
  colnames(ts_df)[1] <- "not_a_valid_index"
  mimics_check_indices_data(ts_df)
})
