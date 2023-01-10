remove_repeated_prep <- function(data) {
  # create length column
  data$length <- data$End - data$Start

  # create action column
  data$action <- NA
  return(data)
}

function_remove_loop <- function(practice) {
  for (row in seq_along(practice$id)) {
    message(row)
    # check if last row in the dataframe
    if (row == nrow(practice)) {
      message("last row")
    } else {
      # check if the start of the next row is less than the end of the current row
      if (practice$Start[row + 1] < practice$End[row]) {
        message("overlap")
        # determine the range
        y_values <- c(practice$End[row], practice$End[row + 1])
        y_min <- min(y_values)
        x_values <- c(practice$Start[row], practice$Start[row + 1])
        x_max <- max(x_values)
        range_both <- y_min - x_max
        # determine the overlap % for row
        overlap_length <- range_both / (practice$length[row])
        # determine the overlap % for row + 1
        overlap_length_next <- range_both / (practice$length[row + 1])

        # if either overlap is equal to or greater than 0.95
        if (overlap_length >= 0.95 | overlap_length_next >= 0.95) {
          message("yes, significant overlap")

          # check if length is equal
          if (practice$length[row] == practice$length[row + 1]) {
            message("lengths are equal")
            practice$action[row] <- "keep"
            practice$action[row + 1] <- "remove"
          } else if (practice$length[row] > practice$length[row + 1]) {
            message("row > row + 1")
            practice$action[row] <- "keep"
            practice$action[row + 1] <- "remove"
          } else {
            message("row < row + 1")
            practice$action[row] <- "remove"
            practice$action[row + 1] <- "keep"
          }
        }
      }
    }
  }
  return(practice)
}

function_remove_v1 <- function(practice) {
  # remove rows then run the loop
  # # assign the action column "keep" if == NA
  # practice_cleaned <- practice %>%
  #   dplyr::mutate(action = ifelse(is.na(action), "keep", action))
  practice_cleaned <- practice
  # remove rows where action is "remove"
  practice_cleaned <- practice_cleaned[is.na(practice_cleaned$action) | practice_cleaned$action != "remove", ]
  # sort by Start column
  practice_cleaned <- practice_cleaned[order(practice_cleaned$Start), ]
  # run the loop
  practice_out <- suppressMessages(function_remove_loop(practice_cleaned))
  return(practice_out)
}


test <- function_remove_v1(practice)

#--------------------------------------------
# Tests:
# does the first run detection work
# and does the recursion work

# create a test file
write.csv(practice, "tests/testthat/testdata/rmv_rpt_test1.csv")
test_that("function_remove_v1 works", {
  # expect that it returns a dataframe
  expect_is(function_remove_v1(practice), "data.frame")
})

#--------------------------------------------
# now create the main function
input_df <- practice_fresh_run
input_df <- output_df

# recursion_counter <- 0

remove_repeated_master <- function(input_df) {
  # recursion_counter <<- recursion_counter + 1

  # check if dataframe is empty, and if so, issue a warning
  # the expr_label should return the name of the df which had no data
  if (nrow(input_df) == 0) {
    msg <- paste0("The input ", rlang::expr_label(substitute(input_df)), "contains no data.")
    warning(msg)
    stop()
  } else if (condition) {
    selected
  }

  # check if only 1 row, and if so, return unmodified data frame
  if (nrow(input_df) == 1) {
    return(input_df)
  }

  # base case: if practice$action does not contain "remove" and is not all NA
  if (!any(input_df$action %in% "remove") && !all(is.na(input_df$action))) {
    print("base case, no more removal needed")
    # print(paste("The function has recursed", recursion_counter, "times."))
    return(input_df)
  } else {
    # first run condition
    if (all(is.na(input_df$action))) {
      message("first run:")
      message("first run:")
      message("first run:")
      output_df <- suppressMessages(function_remove_v1(remove_repeated_prep(input_df)))
      remove_repeated_master(output_df)
    } else {
      # not first run, so check the base case
      # if practice$action contains "remove"
      if (!any(input_df$action %in% "remove")) {
        print("base case, no more removal needed")
        # print(paste("The function has recursed", recursion_counter, "times."))
        # return the data frame
        return(input_df)
      } else {
        # rows tagged with remove, running again
        # call the function
        output_df <- suppressMessages(function_remove_v1(input_df))
        remove_repeated_master(output_df)
      }
    }
  }
}
