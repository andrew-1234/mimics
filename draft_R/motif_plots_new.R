# This should be broken down into smaller functions
# It does two things: combines data, and generates plots
# It takes data_indices_all as input, which is the data frame returned by the
# function time_series

# Function 1: complete_time_series
# This function returns the complete time series per site per month

# Function 2: plot_time_series
# This function plots the time series per site per month

# Function 3: hime_series / hime_indices
# This function combines the indices time series with the hime motif output
# This function needs to be broken down into smaller functions

####################################################################

# Function 1: complete_time_series
# Need 1 dataframe, or 1 plot, per point per month
# I think each plot will have all three indices
# Visualise with a flow chart

# test that my_indices_data contains the right columns
test_that("my_indices_data contains the right columns", {
  expect_equal(colnames(my_indices_data), c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy", "FileName", "date_time", "month", "date", "time", "ResultMinute", "FID", "site", "filepath"))
})

# Step 1: split the data into sites and months
split_data <- split(
  my_indices_data,
  interaction(my_indices_data$site, my_indices_data$month),
  drop = TRUE # levels that do not occur should be dropped
)

# test that the split data contains the right number of elements
# elements should equal unique months * unique sites
test_that("Split data contains the right number of elements", {
  expect_equal(length(split_data), length(unique(my_indices_data$month)) * length(unique(my_indices_data$site)))
})
names(split_data[[1]])
# Step 2: for each data frame, add position column
split_data_ts <- lapply(
  split_data,
  function(x) {
    x %>%
      dplyr::mutate(., position = seq_len(nrow(.)))
  }
)

####################################################################

# Function 2: plot_time_series using the split_data_ts

# Step 1: prepare the dataframe for plotting
# define the indices
total_indices <-
  c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy")

# test that each data frame in the list contains the columns in total_indices
test_that("Split data contains all indices", {
  lapply(split_data, function(x) {
    expect_true(all(total_indices %in% names(x)))
  })
})


# real: pivot the data (on list)
split_data_ts_pivot <- lapply(split_data_ts, function(x) {
  x %>%
    dplyr::select(position, dplyr::all_of(total_indices)) %>%
    tidyr::pivot_longer(.,
      cols = 2:4,
      names_to = "index",
      values_to = "value"
    )
})

# test that value column is numeric
test_that("Value is numeric", {
  lapply(split_data_ts_pivot, function(x) {
    expect_true(is.numeric(x$value))
  })
})

# test that index column contains three indices still
test_that("index column contains three indices", {
  lapply(split_data_ts_pivot, function(x) {
    expect_equal(length(unique(x$index)), 3)
  })
})

# define a function that plots the time series
plot_ts <- function(x, name) {
  ggplot2::ggplot(x, ggplot2::aes(x = position, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(. ~ index) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(name)
}

# get element names
ele_names <- names(split_data_ts_pivot)

# make names suitable for file naming use
ele_names <- gsub("\\.", "_", ele_names)

# add the suffix now
ele_names <- paste(ele_names, "indicespertime.jpg", sep = "_")

# loop through each element and plot
# using a loop doesn't return plots, but when lapply is used it does
# need to specify print(plot) to return a plot to environment
# set the pathid
pathid <- "output/figures"
# add a setting to return plots to environment
# add a setting to turn plot title on or off

# working loop
for (i in seq_along(split_data_ts_pivot)) {
  plot <- plot_ts(split_data_ts_pivot[[i]], ele_names[i])
  ggplot2::ggsave(file.path(pathid, ele_names[i]))
}

# test that output/figures contains the right number of plots
# not a good test because output/figures can contain lots of things
test_that("output/figures contains the right number of plots", {
  expect_equal(length(list.files(pathid)), length(split_data_ts_pivot))
})

# Function 3: hime_series / hime_indices
# This function combines the indices time series with the hime motif output
# This function needs to be broken down into smaller functions

# test that data in split_data_ts contains all indices columns
test_that("data in split_data_ts contains all indices columns", {
  lapply(split_data_ts, function(x) {
    expect_true(all(total_indices %in% names(x)))
  })
})

# Step 1: Dataframe for each index, ready for motif data

# Get the names in the list of dataframes
dfs_so_far <- names(split_data_ts)

# Prepare an empty list to store the dataframes
dfs_motif <- list()

# For each dataframe, create three separate dataframes, one for each index
# And add the motif columns ready for later merging
for (i in dfs_so_far) {
  df <- split_data_ts[[i]] %>%
    dplyr::mutate(., motif = NA) %>%
    dplyr::mutate(., distance = NA) %>%
    dplyr::mutate(., length = NA) %>%
    dplyr::mutate(., reference = "0_ts") %>%
    dplyr::mutate(., id = 0)
  # dplyr::rename(., Index = index)

  cols_names <- colnames(df)
  AC <- df[!cols_names %in% c("EventsPerSecond", "TemporalEntropy")]
  EV <- df[!cols_names %in% c("AcousticComplexity", "TemporalEntropy")]
  TE <- df[!cols_names %in% c("EventsPerSecond", "AcousticComplexity")]

  # first column should always be an index
  # rename index position 1 to Index
  # TODO not sure if this was necessary? but was in the old steps
  # on HOLD for now

  newname1 <- paste0(i, "_", "AC")
  newname2 <- paste0(i, "_", "EV")
  newname3 <- paste0(i, "_", "TE")

  this_list <- list(newname1 = AC, newname2 = EV, newname3 = TE)

  names(this_list) <- c(newname1, newname2, newname3)

  dfs_motif <- c(dfs_motif, this_list)
}

# test that the list contains the right number of elements
# should be 3 times the number of elements in split_data_ts
test_that("dfs_motif contains the right number of elements", {
  expect_equal(length(dfs_motif), 3 * length(split_data_ts))
})

# test the the number of columns in each dataframe is minus 2 compared to
# split_data_ts, and plus 5 new columns
test_that("dfs_motif contains the right number of columns", {
  lapply(dfs_motif, function(x) {
    expect_equal(ncol(x), ncol(split_data_ts[[1]]) - 2 + 5)
  })
})
# pre step 2:
# replace . with _ in the names of dfs_motif
# so that the names can be used to generate file names
names(dfs_motif) <- gsub("\\.", "_", names(dfs_motif))

# test that names(df_motif) end in _AC, _EV, or _TE
test_that("names(dfs_motif) end in _AC, _EV, or _TE", {
  lapply(names(dfs_motif), function(x) {
    expect_true(grepl("_AC$|_EV$|_TE$", x))
  })
})

# Step 2: import motif data for each site/month/inex
# Step 2.1 generate the hime file import path

# create a function that generates a path to matching hime file for each data
# frame in dfs_motif
hime_file_is <- function(df) {
  # unique months
  month_id <- unique(df$month)

  # TODO unique site id (from name, from column, or from metadata table?)
  site_id <- unique(df$site)

  # Index ID, index should always be in position 1
  index_id <- names(df)[1]

  # generate the file ID
  file_id <- paste0("Res_TS_", index_id, "_", month_id, "_", site_id, ".txt")

  # where is the hime data sotred?
  himepath <- "output/hime-clean"

  # use full path to hime file
  full_path <- get_data_path(himepath, file_id)

  # check file exists
  tryCatch(test_that("hime file exists", {
    expect_true(file.exists(full_path))
  }), error = function(e) {
    message(paste0("🚨 hime file does not exist: \n", full_path))
  })

  return(full_path)
}

# test that hime_file_is returns the right path
# run this on dfs_motif
lapply(dfs_motif, hime_file_is)

###
# Step 2.2 for each data frame in list, read the hime file
motif_results <- utils::read.table(full_path, row.names = NULL)
# Rename the columns
motif_results_2 <- motif_results %>%
  dplyr::rename(
    .,
    FirstInstance_Start = V1,
    FirstInstance_End = V2,
    SecondInstance_Start = V3,
    SecondInstance_End = V4,
    Length = V5,
    Distance = V6
  )
# check that motif_results_2 contains rows
test_that("motif_results_2 contains rows", {
  expect_true(nrow(motif_results_2) > 0)
})

# check that motif_results_2 contsins 6 columns with the correct names
test_that("motif_results_2 contains the right number of columns and correct names", {
  expect_equal(ncol(motif_results_2), 6)
  expect_equal(names(motif_results_2), c("FirstInstance_Start", "FirstInstance_End", "SecondInstance_Start", "SecondInstance_End", "Length", "Distance"))
})

# add an id column to the motif_results_2 data frame with a sequential count
motif_results_3 <- motif_results_2 %>%
  dplyr::mutate(., id = 1:as.numeric(dplyr::count(.)))

# check for distance <= 5 keep only those
# TODO: explain why this is done
motif_results_4 <- motif_results_3 %>%
  dplyr::filter(., Distance <= 5)

# test that distance column is <= 5
test_that("distance column is <= 5", {
  expect_true(all(motif_results_4$Distance <= 5))
})

# test that motif_results_4 still contains rows
# try catch to avoid error if motif_results_4 is empty
tryCatch(
  test_that("motif_results_4 still contains rows", {
    expect_true(nrow(motif_results_4) > 0)
  }),
  error = function(e) {
    message("🚨 motif_results_4 is empty after filtering for distance <= 5")
  }
)

# pivot longer (from wide to long)
motif_results_5 <- motif_results_4 %>%
  tidyr::pivot_longer(.,
    cols = 1:4,
    names_to = "Instance",
    values_to = "position"
  )

# test that motif_results_5 contains five columns
test_that("motif_results_5 contains the right number of columns", {
  expect_equal(ncol(motif_results_5), 5)
})

# the terminology motif and match is what Nina uses
# this changes the pattern "FirstInstance" in the Instance column to "motif"
motif_results_6 <- motif_results_5 %>%
  dplyr::mutate(.,
    Instance = gsub(
      pattern = "FirstInstance",
      replacement = "motif",
      x = Instance
    )
  )
# this changes the pattern "SecondInstance" in the Instance column to "match"
motif_results_7 <- motif_results_6 %>%
  dplyr::mutate(.,
    Instance = gsub(
      pattern = "SecondInstance",
      replacement = "match",
      x = Instance
    )
  )

# test that motif_results_7 contains 4 unique values in the Instance column
test_that("motif_results_7 contains the right number of unique values in the Instance column", {
  expect_equal(length(unique(motif_results_7$Instance)), 4)
})

# test that motif_results_7 Instance column does not contain text Instance
# to verify that the gsub worked
test_that("motif_results_7 Instance column does not contain text Instance", {
  expect_false("Instance" %in% motif_results_7$Instance)
})

# now separate the Instance column into instance and moment based on _
motif_results_8 <- motif_results_7 %>%
  tidyr::separate(.,
    Instance,
    into = c("instance", "moment"),
    sep = "_"
  )

# test that the ncol of notif_results_8 is + 1 compared to motif_results_7
test_that("motif_results_8 contains the right number of columns", {
  expect_equal(ncol(motif_results_8), ncol(motif_results_7) + 1)
})

# test that the instance and moment columns both contain only 2 unique values
test_that("motif_results_8 instance and moment columns contain the right number of unique values", {
  expect_equal(length(unique(motif_results_8$instance)), 2)
  expect_equal(length(unique(motif_results_8$moment)), 2)
})

# pivot wider again, taking the names start and end from moment and the values
# from position
motif_results_9 <- motif_results_8 %>%
  tidyr::pivot_wider(.,
    names_from = moment,
    values_from = position
  )

test_that("motif_results_9 has less rows than motif_results_8", {
  expect_true(nrow(motif_results_9) < nrow(motif_results_8))
})

# modify the instance column to include the id
motif_results_10 <- motif_results_9 %>%
  dplyr::mutate(., instance = paste(id, instance, sep = "_"))

# arrange the data frame by the Start column
motif_results_11 <- motif_results_10 %>%
  dplyr::arrange(., Start)

# test that the data was sorted correctly by the Start column
# make sure that each row is less than the next row
# what happens if you have two rows that are equal - it fails
# is this a problem having two equal rows?
# using is.unsorted instead passes the test if rows are identical
test_that("each row is less than the next row", {
  expect_false(is.unsorted(motif_results_11$Start))
})

# add an overlap column = NA
motif_results_12 <- motif_results_11 %>%
  dplyr::mutate(., overlap = NA)
# this is a good practice dataset for testing the overlap functions
write.csv(
  motif_results_12,
  "tests/testthat/testdata/motif_results_12.csv"
)

# run the remove_repeated function
# when overlappying, the longer motif is kept (e.g. choice between keeping
# 1_motif and 3_motif, but 3_motif duration > 1, 3_motif is kept)
# so then you will have 1_match and no 1_motif but this doesn't matter
# the point is removing the things that don't have repitions (this is the
# underlying goal)
# these names _match _motif are just because need unique identifiers, they won't
# actually be used in a paired way like matching 1_motif to 1_match, it's not
# important
# the start and end minutes is based on the entire time series for that month,
# so that's why you can work with one motif df per month and still know where
# things came from
motif_results_13 <- MIMiCS::remove_repeated(motif_results_12)

# call the function recursively make sure all overlaps are removed
# only fully overlapping things or fully contained overlaps are removed
# Start End compare 134 to 168, and next row 135 to 169, these overlap but
# weren't removed because not full contained, but should probably be removed
# could check for 95% overlap and remove

# create a df with columns start and end
new_df <- data.frame(
  Distance = c(3.5, 2.5, 4.9, 2.7, 3.7, 2.9, 3.22),
  Start = c(6, 6, 20, 40, 134, 135, 200),
  End = c(40, 42, 100, 150, 168, 169, 300)
)
# do the rows overlap?
testing <- iteration1(new_df)
# can see here that the overlap isn't detected for rows 5 and 6

calculate_overlap_percentage <- function(df, x_col, y_col) {
  n <- nrow(df)
  print(n)
  df$overlap_percentage <- rep(NA, n)
  print(df$overlap_percentage)
  print("starting first loop")
  for (i in 1:n) {
    print(paste("this is i", i))
    x1 <- df[i, x_col]
    print(paste("this is x1", x1))
    y1 <- df[i, y_col]
    print(paste("this is y1", y1))
    for (j in 1:n) {
      print(paste("this is j", j))
      x2 <- df[j, x_col]
      print(x2)
      y2 <- df[j, y_col]
      print(y2)
      overlap <- min(y1, y2) - max(x1, x2)
      print(overlap)
      total_length <- max(y1 - x1, y2 - x2)
      print(total_length)
      overlap_percentage <- overlap / total_length
      df$overlap_percentage[i] <- overlap_percentage
    }
  }
  return(df)
}
# ignore the last row which will always have overlap_percentage of 1

# overlap percentage should be 0 when there is no overlap
# overlap percentage should be 1 when there is full overlap
# overlap percentage should be 0.5 when there is half overlap

calculate_overlap_percentage(new_df, "Start", "End")

# check the recursion with a sample dataset that has overlaps that appear after
# each iteration
# so lots of 6,6,6,6,6 matching 40,41,42,43,45
# after 3 runs, there are still overlaps (there should be)
# but with a recursive call, the final row should be 6, 45, which is the longest
# overlap
# Step 3 wrangle the hime data
# Step 3.1 rename the columns
# Step 3.2 pivot and mutate
# Step 4 run the remove_repeated function on the data frames
# Step 5 loop through each row in the dfs_motif list
# Step 6 write the files to the output folder
# Step 7 two different plot dataframes

# How do the motifs get matched back to the file id??
