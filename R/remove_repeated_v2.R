# first function
# perform first check for overlap

practice <- motif_results_12[3:7]

# create length column
practice$length <- practice$End - practice$Start

# create action column
practice$action <- NA

row <- 1

# make some test DFs
# practice_first_run is to make sure the first run works
practice_first_run <- practice
# practice_fresh_run is to make sure the entire function works
practice_fresh_run <- practice


# make sure practice_first_run$action is all NA
tryCatch(test_that("Check if practice_first_run$action is all NA", {
  expect_true(any(is.na(practice_first_run$action)))
}), error = function(e) {
  print("practice_first_run$action is not all NA")
})

function_remove_loop <- function(practice) {
  for (row in seq_along(practice$id)) {
    message(row)
    # check if last row in the dataframe
    if (row == nrow(practice)) {
      message("last row")
    } else {
      # check if the start of the next row is less than the end of the current row
      if (practice$Start[row + 1] <= practice$End[row]) {
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
asd <- function_remove_loop(practice_fresh_run)
function_remove_v1(asd)

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

remove_repeated_master <- function(input_df) {
  recursion_counter <<- recursion_counter + 1
  # base case: if practice$action does not contain "remove" and is not all NA
  if (!any(input_df$action %in% "remove") && !all(is.na(input_df$action))) {
    print("base case, no more removal needed")
    print(paste("The function has recursed", recursion_counter, "times."))
    return(input_df)
  } else {
    # first run condition
    if (all(is.na(input_df$action))) {
      message("first run:")
      message("first run:")
      message("first run:")
      recursion_counter <- -0
      output_df <- suppressMessages(function_remove_v1(input_df))
      remove_repeated_master(output_df)
    } else {
      # not first run, so check the base case
      # if practice$action contains "remove"
      if (!any(input_df$action %in% "remove")) {
        print("base case, no more removal needed")
        print(paste("The function has recursed", recursion_counter, "times."))
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


asdf <- remove_repeated_master(practice_fresh_run)

practice_fresh_run_big <- rbind(
  practice_fresh_run,
  practice_fresh_run,
  practice_fresh_run
)
test1 <- function_remove_loop(practice_fresh_run_big)
test2 <- function_remove_v1(practice_fresh_run_big)
test2_2 <- function_remove_v1(test2)
test2_3 <- function_remove_v1(test2_2)
test2_4 <- function_remove_v1(test2_3)
test2_5 <- function_remove_v1(test2_4)
test2_6 <- function_remove_v1(test2_5)

test3 <- remove_repeated_master(practice_fresh_run_big)
# why is there na in test1 but not in test2...?
nrow(practice_fresh_run_big)
nrow(test1)
nrow(test2)

# make sure all edge cases are covered like if == or only one row or no rows
# etc.
# if nrow = 1 does it end?
# check samsung notes

#--------------------------------------------
# Plotting to help visualise overlaps and how they are handled

# convert practice dataset to long format based on the start and end columns
practice_plot <- practice %>%
  pivot_longer(
    cols = c(Start, End),
    names_to = "Start_End",
    values_to = "Value"
  )

# plot the long format data
ggplot2::ggplot(data = practice_plot, ggplot2::aes(x = Value, y = instance, color = as.character(id), group = instance)) +
  ggplot2::geom_line(size = 30) +
  # ggplot2::facet_wrap(~Name, ncol = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  ggplot2::labs(x = "asdf", y = "asdf", color = "")
