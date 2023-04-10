# test that run_hime works
test_that("run_hime works and generates output", {
  input <- "tests/testthat/test-inputs/timeseries"
  output <- "tests/testthat/tmp/hime"
  run_hime(input, output)
  expect_true(length(list.dirs("tests/testthat/tmp", recursive = FALSE)) == 2)
  expect_true(length(list.files(output)) > 0)
  unlink(list.dirs("tests/testthat/tmp", recursive = FALSE), recursive = TRUE)
})
