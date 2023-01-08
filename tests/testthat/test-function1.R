# File: test-function1.R
data <- read.csv("tests/testthat/testdata/my_indices_data.csv")

# or use mtcars?
# data_bad <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))

#-------------------------------------------------
test_that("Data has expected number of columns", {
  expect_equal(ncol(data), 13)
})

test_that("Data has rows", {
  expect_true(nrow(data) > 0)
})

# test that the number of sites is correct

# test that the number of months is corect

# test that the length of the split data is correct
# where length should equal the interaction of sites and months
# if 2 months, and 8 sites, then length should be 16
# if 4 months, and 8 sites, then length should be 32
# this is assuming that all sites have data for all months
# and if they don't?

# test that nrow is correct after splitting the data
# nrow should equal 1 row per minute, per how many recordings etc
# edge cases sometimes a minute is dropped (i think) if there is not enough for
# an index values to be calculated for that minute (like at the end of a
# recording, minut 59 or something)
