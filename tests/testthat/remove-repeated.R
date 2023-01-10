# Read in the testing dataframe(s)

############################################################################
# Data frames descriptions
############################################################################

# Remove_repeated_df1 ------------------------------------------------------

# "/Users/andrew/Documents/GitHub/mimics/output/hime-clean/Res_TS_AcousticComplexity_Dec_Booroopki-Dry-B.txt"
# Available in csv as /testdata/remove_repeated_df1_full.csv
# The state of this csv is from motif_results_12, when an overlap = NA
# column has been added, in the motif_plots function (new version)
# The version used below was filtered to remove the length and distance columns
# while I was developing. TODO: update to full test workflow

remove_repeated_df1 <- read.csv("tests/testthat/testdata/remove_repeated_df1.csv")

############################################################################
# Tests
############################################################################

# Test that the remove_repeated_prep function works
test_that("remove_repeated_prep works", {
  # expect that it returns a dataframe
  expect_is(remove_repeated_prep(remove_repeated_df1), "data.frame")
  # expect it contains columns length and action
  expect_true("length" %in% colnames(remove_repeated_prep(remove_repeated_df1)))
  expect_true("action" %in% colnames(remove_repeated_prep(remove_repeated_df1)))
})

# Test the remove_repeated function returns a column with action all NA
tryCatch(test_that("Check if remove_repeated_df1$action is all NA", {
  expect_true(any(is.na(
    remove_repeated_prep(remove_repeated_df1)$action
  )))
}), error = function(e) {
  print("remove_repeated_df1$action is not all NA")
})

# Test that the function_remove_loop acts as expected
test_that("function_remove_loop works", {
  # expect that it returns a dataframe
  expect_is(function_remove_loop(remove_repeated_prep(remove_repeated_df1)), "data.frame")
  # expect it contains columns length and action
  expect_true("length" %in% colnames(function_remove_loop(remove_repeated_prep(remove_repeated_df1))))
  expect_true("action" %in% colnames(function_remove_loop(remove_repeated_prep(remove_repeated_df1))))
})

# make a 1 row test case
remove_repeated_df1_v2_1row <- remove_repeated_prep(remove_repeated_df1)[1, ]

# test that if length of df is 1 row, function works
test_that("function_remove_loop works with 1 row", {
  # expect that it returns a dataframe
  expect_is(function_remove_loop(remove_repeated_df1_v2_1row), "data.frame")
  # expect message last row
  expect_message(function_remove_loop(remove_repeated_df1_v2_1row), "last row")
})

# Check the function_remove_loop
# With a test df after prep function
remove_repeated_df1_v2 <- remove_repeated_prep(remove_repeated_df1)
remove_repeated_df1_v3 <- function_remove_loop(remove_repeated_df1_v2)

# if you have a case like remove keep, where on row two, keep should change to
# remove, does it work?
remove_repeated_df1_v4 <- remove_repeated_df1_v3[1:3, ]
remove_repeated_df1_v4[3, ] <- remove_repeated_df1_v3[3, ]
remove_repeated_df1_v4$Start <- 6
remove_repeated_df1_v4$End[1:3] <- c(41, 42, 43)
remove_repeated_df1_v4$length <- remove_repeated_df1_v4$End - remove_repeated_df1_v4$Start

# it does work
# these are consecutively longer overlaps, all with same start
# first row looped will mark row 1 and 2 as remove and keep
# second row looped will mark row 2 now as keep, and 3 as remove
test_that("function_remove_loop works with consecutive increases", {
  expect_that(function_remove_loop(remove_repeated_df1_v4)[3, ]$action, equals("keep"))
})

# Test function_remove_v1

# does it return a dataframe? assuming it's been run through
# remove_repeated_prep
test_that("function_remove_v1 returns a dataframe", {
  expect_is(function_remove_v1(remove_repeated_prep(remove_repeated_df1)), "data.frame")
})

# what happens if all rows keep? is it possible?

# if there are rows marked for remove, does it remove them?
# this would be on the second iteration, not the first
# expect that length of output is - 2 (two overlaps removed)
test_that("function_remove_v1 removes rows marked for remove", {
  expect_equal(
    nrow(function_remove_v1(function_remove_v1(remove_repeated_prep(remove_repeated_df1)))),
    nrow(function_remove_v1(remove_repeated_prep(remove_repeated_df1))) - 2
  )
})

# test that first run detection and remove repeated prep works as expected
# in a loop
# TODO: how will the extra columns i've added with the repeated prep affect
# things?
test_that("function remove_repeated_master returns a data frame", {
  expect_is(remove_repeated_master(remove_repeated_df1), "data.frame")
})

# test that remove_repeated_master works with a 1 row df
test_that("function remove_repeated_master works with 1 row df", {
  expect_is(remove_repeated_master(remove_repeated_df1[1, ]), "data.frame")
})

# test that remove_repeated_master works with a 0 length df
empty_df <- remove_repeated_df1[0, ]
test_that("function remove_repeated_master works with 0 row df", {
  # expect_message(remove_repeated_master(empty_df))
})

# test that remove_repeated_master works as expected on the practice data frame
# and removes two rows
test_that("remove_repeated_master removes the two overlaps", {
  expect_equal(
    nrow(remove_repeated_master(remove_repeated_df1)),
    nrow(remove_repeated_df1) - 2
  )
})

# what happens if all rows are equal?
remove_repeated_df1_v5 <- remove_repeated_df1_v4
remove_repeated_df1_v5$End <- 41
remove_repeated_df1_v5$length <- 35
remove_repeated_df1_v5$action <- NA

# with the recursive call it should handle cases like this
# three overlapping rows are reduced to one
test_that("function_remove_loops works if all rows are 100% overlapping", {
  expect_equal(
    nrow(remove_repeated_master(remove_repeated_df1_v5)),
    1
  )
})

# TODO: check behaviour on big datasets, if recursion is working
remove_repeated_df1_big <- rbind(
  remove_repeated_df1,
  remove_repeated_df1,
  remove_repeated_df1
)
# test that row length is equal between runs on big df and original
test_that("function_remove_loop works on big df", {
  expect_equal(
    nrow(remove_repeated_master(remove_repeated_df1_big)),
    nrow(remove_repeated_master(remove_repeated_df1))
  )
})

# TODO: Test the old function vs new function
# asdf <- read.table("/Users/andrew/Documents/GitHub/mimics/output/hime-clean/Res_TS_AcousticComplexity_Dec_Booroopki-Dry-B.txt")
# remove_repeated(remove_repeated_df1)

# -------
# if a dataframe of length 0 row in list, it should be skipped
test_that("function_remove_loop skips dfs with 0 rows and continues", {

})
