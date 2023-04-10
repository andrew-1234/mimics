# Test that ap_main(help) works on mac if AP is installed
test_that("ap_main works on mac", {
  skip_on_os("windows")
  expect_invisible(ap_main("help"))
})

# Test that find_program("AP") works on mac if AP is installed
test_that("find_program works on mac", {
  skip_on_os("windows")
  expect_output(find_program("AP"), "package AP is installed")
})

# Test that find_program() returns correct message if program not installed
test_that("find_program returns correct message if program not installed", {
  skip_on_os("windows")
  expect_output(find_program("not_a_program"), "not_a_program is not installed")
})

# Test ap_prepare produces correct output on mac
test_that("ap_prepare produces correct output on mac", {
  skip("this is interactive only and takes a long time")
  skip_on_os("windows")
  ap_prepare("tests/testthat/input/A2O-mini", "tests/testthat/tmp/indices")
  expect_true(length(list.dirs("tests/testthat/tmp/indices", recursive = FALSE)) == 8)
  unlink("tests/testthat/tmp/indices", recursive = TRUE)
})
