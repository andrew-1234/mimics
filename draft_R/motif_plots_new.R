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
  expect_equal(
    colnames(my_indices_data),
    c(
      "AcousticComplexity",
      "EventsPerSecond",
      "TemporalEntropy",
      "FileName",
      "date_time",
      "month",
      "date",
      "time",
      "ResultMinute",
      "FID",
      "site",
      "filepath"
    )
  )
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
# remove dots and replace with underscores
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
# TODO: not a good test because output/figures can contain lots of things
test_that("output/figures contains the right number of plots", {
  # expect_equal(length(list.files(pathid)), length(split_data_ts_pivot))
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
  # on HOLD for now, and first index position renames to the actual index
  # abbreviation

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

# Step 2: import motif data for each site/month/index

# replace . with _ in the names of dfs_motif
# so that the names can be used to generate file names
names(dfs_motif) <- gsub("\\.", "_", names(dfs_motif))

# test that names(dfs_motif) end in _AC, _EV, or _TE
test_that("names(dfs_motif) end in _AC, _EV, or _TE", {
  lapply(names(dfs_motif), function(x) {
    expect_true(grepl("_AC$|_EV$|_TE$", x))
  })
})


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
  # TODO: double check that hime-clean will always be the right folder
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

# function to create a list of matching hime file names
# and return a list of hime files
read_hime_files <- function(df) {
  hime_files <- list()
  hime_files <- lapply(df, hime_file_is)
  motif_results <- list()
  motif_results <- lapply(hime_files, function(x) {
    utils::read.table(x, row.names = NULL)
  })
  return(motif_results)
}

motif_results <- read_hime_files(dfs_motif)

# confirm that length of dfs_motif matchs length of motif_results
test_that("length of dfs_motif matchs length of motif_results", {
  expect_equal(length(dfs_motif), length(motif_results))
})

# for each dataframe, rename the columns
motif_results_2 <- list()
motif_results_2 <- lapply(motif_results, function(x) {
  x %>%
    dplyr::rename(
      .,
      FirstInstance_Start = V1,
      FirstInstance_End = V2,
      SecondInstance_Start = V3,
      SecondInstance_End = V4,
      Length = V5,
      Distance = V6
    )
})

# check that each data frame in the list contains rows
test_that("each data frame in the list contains rows", {
  lapply(motif_results_2, function(x) {
    expect_true(nrow(x) > 0)
  })
})

# test that each data frame in the list contains the right number of columns
# with the correct names
test_that("each data frame in the list contains the right number of columns", {
  lapply(motif_results_2, function(x) {
    expect_equal(ncol(x), 6)
    expect_equal(names(x), c(
      "FirstInstance_Start",
      "FirstInstance_End",
      "SecondInstance_Start",
      "SecondInstance_End",
      "Length",
      "Distance"
    ))
  })
})
# Create a function that adds a count column and filters by distance column
# Remember - why are we filtering by distance?
add_id_column <- function(df) {
  df_2 <- df %>%
    dplyr::mutate(., id = 1:as.numeric(dplyr::count(.)))

  df_3 <- df_2 %>%
    dplyr::filter(., Distance <= 5)

  # test that distance column is <= 5
  test_that("distance column is <= 5", {
    expect_true(all(motif_results_4$Distance <= 5))
  })
  return(df_3)
}

# apply the add_id_column function to each data frame in the list
motif_results_3 <- list()
motif_results_3 <- lapply(motif_results_2, add_id_column)

# continue with function unless row count is 0
# add a skip function or something here, or a check that the row count is > 0 if
# else condition

tryCatch(
  test_that("motif_results still contains rows", {
    expect_true(nrow(motif_results_3) > 0)
  }),
  error = function(e) {
    message("🚨 motif_results is empty after filtering for distance <= 5")
  }
)
###
# Step 2.2 for each data frame in list, read the hime file

# For testing purposes
full_path <- "/Users/andrew/Documents/GitHub/mimics/output/hime-clean/Res_TS_AcousticComplexity_Dec_Booroopki-Dry-B.txt"

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

# Step 3 wrangle the hime data
# Step 3.1 rename the columns
# Step 3.2 pivot and mutate

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
# this dataset saved for testing in testdata as Remove_repeated_df1
motif_results_12 <- motif_results_11 %>%
  dplyr::mutate(., overlap = NA)

# run the remove_repeated function to remove overlapping (0.95%) motifs

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

# Step 4 run the remove_repeated function on the data frames
# can see here how the two functions differ
motif_results_13 <- MIMiCS::remove_repeated(motif_results_12)
motif_results_13 <- remove_repeated_master(motif_results_12)


# Step 5 loop through each row in the dfs_motif list
# Remember the dfs_motif list is a list of data frames with the acoustic index
# value for each result minute. We need to match these values to the motifs
# For testing, the motif result Res_TS_AcousticComplexity_Dec_Booroopki-Dry-B is
# being used, and the df is Booroopki-Dry-B_Dec_AC

# get the data frame from the list
names(dfs_motif)
dfs_motif_sub <- dfs_motif[[10]]
# this will be called motif_results
motif_results <- motif_results_13

# TODO: so this is case senesitive, and i'm using length in motif_results, which
# I think is created in the remove_repeated_master
# Instead of the Length column (original script). In this specific case I'm
# testing, the Length column shows 34, but the length column shows 36.

# TODO: This loop will overwrite overlapping motifs. If motif 1 goes from result
# minute 1 to 30, and motif 2 goes from minute 11 to 40, then motif 1 will only
# be annotated for the first 10 minutes. This could be an issue because when
# spectrograms are cropped, the motif will appear shorter than it should be.
# Although usually it won't be an issue and a classification can still be made. 
# But it would be nice to special case this and allow overlapping motifs and
# cropping in the future. 
for (row in seq_len(nrow(dfs_motif_sub))) {
skip_to_next <- FALSE
    tryCatch(
      {
        dfs_motif_sub[motif_results$Start[row]:motif_results$End[row],
        c("motif", "distance", "length")] <- motif_results[row, c("instance", "Distance", "length")]
      },
      error = function(e) {
        skip_to_next <<- TRUE
        }
    )
    if (skip_to_next) {
      next
    }
}
names(dfs_motif_sub)

# adding a count and filtering
# filter for n >= 30. originally this was because the remove function wasn't
# recursive and there were lots of little motifs like 5 minutes, so this was
# annoying for spectrograms.
# motifs that are 20 or 30 minutes or more are easier to look at and categorise
# But with the new function this is less of an issue.
# for now, going to filter at 20. And come back to this depending on adjustments
# to the recursive overlap threshold. Because this is almost a redundancy step
# to make sure there are no small or overlapping motifs that will later be
# generated into spectrograms to categorise.
# TODO: but may want to add a way to have overlapping motifs at a certain
# threshold, so multiple columns, so that the spectrograms can be cropped for
# each of them, and a motif that is actually 34 long but appears as 19 long
# because of overlapping isn't discarded.

# create a new column (n) with the number of non-NA values in the motif column
# per group (motif). NA values are ignored in the sum
# then filter for n >= 20, but keep NA values (needed for time series plotting,
# I think...)
# TODO: if nas are needed for the time series plots, then wouldn't filtering out
# N values below 20 also create gaps in the plots?
dfs_motif_sub_2 <- dplyr::group_by(dfs_motif_sub, motif) %>%
  dplyr::mutate(n = ifelse(is.na(motif),
    NA,
    sum(!is.na(motif),
      na.rm = TRUE
    )
  )) %>%
  dplyr::filter(is.na(n) | n >= 20)

# Step 6 write the files to the output folder (himepath = hime-clean at the
# moment)
# writing dfs_motif_sub_2 to a csv file
# need current site ID, month ID, and index
dfs_motif_sub_2 %>% dplyr::glimpse()
unique(dfs_motif_sub_2$month)
dfs_motif_sub_2$FileName[1]
himepath
get_data_path(himepath)

create_file_name_motif_csv <- function(df) {
  df <- dfs_motif_sub_2
  month_id <- unique(df$month)
  site_id <- unique(df$site)
  index_id <- names(df)[1]
  file_id <- paste0("Motif_",
                    index_id, "_",
                    month_id, "_",
                    site_id,
                    ".csv")
  return(file_id)
}

write.csv(
  x = dfs_motif_sub_2,
  file = file.path(
    get_data_path(himepath),
    create_file_name_motif_csv(dfs_motif_sub_2)
  ),
  row.names = F
)

# TODO: i can easily lapply the entire above thing
# TODO: at this point, ID = 0, should it be matched to the motif ID when the
# data frames are merged?

# Step 7 two different plot dataframes
# TODO: at this stage, there already exist plots _indicespertime which show ALL
# three acoustic indices. These should be in a different folder I think. And the
# below plots which are index specific should be in another folder

# Step 7.1 create main plot data frames
# Before, the first col was called Index which made this easier
# But I will need the index name for running this in a loop for making the
# titles
# I'll save it as a variable, and rename the first column to Index
# i added ungroup here otherwise it was adding motif column in. not sure if it
# is needed or not.
index <- names(dfs_motif_sub_2)[1]

dfs_motif_sub_3 <- dfs_motif_sub_2
names(dfs_motif_sub_3)[1] <- "Index"

plot_ts <- dfs_motif_sub_3 %>%
  dplyr::ungroup() %>%
  dplyr::select(Index, reference, position, date, time) %>%
  tidyr::separate(.,
    reference,
    into = c("number", "what"),
    remove = FALSE
  )

# Step 7.2 create the plot data frame for the geom_lines
plot_motif <- dfs_motif_sub_3 %>%
dplyr::ungroup() %>%
  dplyr::select(motif, position, Index) %>%
  dplyr::rename(., reference = motif) %>%
  dplyr::filter(reference != "NA") %>%
  tidyr::separate(reference,
    into = c("number", "what"),
    remove <- FALSE,
    sep = "_"
  )

# Construct the plot
# line parts commented out weren't working or something

ggplot2::ggplot(plot_ts, ggplot2::aes(x = position, y = Index)) +
  ggplot2::geom_line(ggplot2::aes(colour = reference, linetype = reference), colour = "grey") +
  # geom_vline(xintercept = line_intercept1$position, linetype = "dotted") +
  # geom_text(data = line_intercept1,
  #           aes(label = time, y = 10, size = 1),
  #           check_overlap = T) +
  # geom_vline(xintercept = line_intercept2$position, linetype = "dotted") +
  # geom_text(data = line_intercept2,
  #           aes(label = time, y = 10, size = 1),
  #           check_overlap = T) +
  ggplot2::scale_linetype_manual(values = "dotted") +
  ggplot2::geom_line(data = plot_motif, ggplot2::aes(
    x = position,
    y = Index,
    colour = reference
  )) +
  ggplot2::scale_color_manual(values = c(replicate(nrow(
    motif_results
  ), "#2ca25f"))) +
  ggplot2::theme_classic() +
  ggplot2::labs(title = paste(index, sep = " ")) +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "none"
  )

  pathid

# The order of naming is different here to previously?
create_file_name_index_plot <- function(df) {
  df <- dfs_motif_sub_2
  month_id <- unique(df$month)
  site_id <- unique(df$site)
  index_id <- names(df)[1]
  file_id <- paste0(
    site_id, "_",
    month_id, "_",
    index_id, "_",
    "indicespertime.jpg"
  )
  return(file_id)
}

ggplot2::ggsave(
  get_data_path(pathid, create_file_name_index_plot(dfs_motif_sub_3))
)
