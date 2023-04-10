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
# dfs_motif <- vector("list", length(dfs_so_far)*3)
# names(dfs_motif) <- dfs_so_far

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

  # dfs_motif[[i]] <- this_list
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
  x <- x %>%
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
    expect_true(all(df_3$Distance <= 5))
  })
  return(df_3)
}

# apply the add_id_column function to each data frame in the list
motif_results_3 <- list()
motif_results_3 <- lapply(motif_results_2, add_id_column)

# continue with function unless row count is 0
# add a skip function or something here, or a check that the row count is > 0 if
# else condition
empty <- lapply(motif_results_3, function(x) {
  if (nrow(x) == 0) {
    cat("🚨 motif_results is empty after filtering for distance <= 5\n")
  } else {
    cat("👏 is not empty after filtering for distance <= 5\n")
  }
})


# check that each dataframe in the list contsins 6 columns with the correct
# names
test_that("each dataframe in the list contsins 6 columns with the correct names", {
  lapply(motif_results_3, function(x) {
    expect_equal(ncol(x), 7)
    expect_equal(names(x), c(
      "FirstInstance_Start",
      "FirstInstance_End",
      "SecondInstance_Start",
      "SecondInstance_End",
      "Length",
      "Distance",
      "id"
    ))
  })
})

# Step 3 wrangle the hime data
# Step 3.1 rename the columns
# Step 3.2 pivot and mutate

motif_pivot_longer <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      tidyr::pivot_longer(.,
        cols = 1:4,
        names_to = "Instance",
        values_to = "position"
      )
  })
}
motif_results_4 <- motif_pivot_longer(motif_results_3)

# Test that each dataframe in the list contains five columns
test_that("each dataframe in the list contains five columns", {
  lapply(motif_results_4, function(x) {
    expect_equal(ncol(x), 5)
  })
})
rename_motif_match <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      dplyr::mutate(.,
        Instance = gsub(
          pattern = "FirstInstance",
          replacement = "motif",
          x = Instance
        )
      ) %>%
      dplyr::mutate(.,
        Instance = gsub(
          pattern = "SecondInstance",
          replacement = "match",
          x = Instance
        )
      )
  })
}

motif_results_5 <- rename_motif_match(motif_results_4)

# test that each dataframe in the list contains the right number of columns
# and the the instance column contains four unique values
test_that("each dataframe in the list contains the right number of columns and the the instance column contains four unique values", {
  lapply(motif_results_5, function(x) {
    if (nrow(x) > 0) {
      expect_equal(ncol(x), 5)
      expect_equal(length(unique(x$Instance)), 4)
    }
  })
})

# test that in the list, the instance column in each data frame does not contain
# the text "Instance"
test_that("in the list, the instance column in each data frame does not contain the text Instance", {
  lapply(motif_results_5, function(x) {
    expect_false("Instance" %in% x$Instance)
  })
})

separate_instance_column <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      tidyr::separate(.,
        Instance,
        into = c("instance", "moment"),
        sep = "_"
      )
  })
}

motif_results_6 <- separate_instance_column(motif_results_5)


# test that the instance and moment columns of each dataframe in the list
# contains the right number of unique values
test_that("the instance and moment columns of each dataframe in the list contains the right number of unique values", {
  lapply(motif_results_6, function(x) {
    if (nrow(x) > 0) {
      expect_equal(length(unique(x$instance)), 2)
      expect_equal(length(unique(x$moment)), 2)
    }
  })
})

# pivot wider again
motif_pivot_wider <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      tidyr::pivot_wider(.,
        names_from = moment,
        values_from = position
      )
  })
}
motif_results_7 <- motif_pivot_wider(motif_results_6)

# function to add an id to the instance columnm, sort by `Start`, and add a new
# column for overlap = NA
add_instance_id <- function(motif_list) {
  lapply(motif_list, function(x) {
    x <- x %>%
      dplyr::mutate(.,
        instance = paste(id, instance, sep = "_")
      )
    if (nrow(x) > 0) {
      x <- x %>%
        dplyr::arrange(., Start, End) %>%
        dplyr::mutate(., overlap = NA)
    }
  })
}

motif_results_8 <- add_instance_id(motif_results_7)


# test that the data was sorted correctly by the Start column
# make sure that each row is less than the next row
# what happens if you have two rows that are equal - it fails
# is this a problem having two equal rows?
# using is.unsorted instead passes the test if rows are identical
test_that("each row is less than the next row", {
  lapply(motif_results_8, function(x) {
    expect_false(is.unsorted(x$Start))
  })
})

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

remove_repeated_wrapped <- function(motif_results, threshold = 0.95) {
  new_list <- list()

  for (i in seq_along(motif_results)) {
    item <- motif_results[[i]]
    if (!is.null(item) && nrow(item) > 0) {
      first_run <<- TRUE
      new_list[[i]] <- remove_repeated_master(item, threshold)
      names(new_list)[i] <- names(motif_results)[i]
    }
  }
  filtered_list <- Filter(Negate(is.null), new_list)
  return(filtered_list)
}

motif_results_9 <- remove_repeated_wrapped(motif_results_8, threshold = 0.9)


# TODO: so this is case senesitive, and i'm using length in motif_results, which
# I think is created in the remove_repeated_master
# Instead of the Length column (original script). In this specific case I'm
# testing, the Length column shows 34, but the length column shows 36.
# Look at the original motif_results as it is read in, v5 column shows 34, but
# length of v1 to v2 is 36, and length of v3 to v4 is 34, so probably v5 column
# length is based on the length of the match?

# TODO: This loop will overwrite overlapping motifs. If motif 1 goes from result
# minute 1 to 30, and motif 2 goes from minute 11 to 40, then motif 1 will only
# be annotated for the first 10 minutes. This could be an issue because when
# spectrograms are cropped, the motif will appear shorter than it should be.
# Although usually it won't be an issue and a classification can still be made.
# But it would be nice to special case this and allow overlapping motifs and
# cropping in the future.

motif_match_df <- function(dfs_motif, motif_results) {
  for (i in seq_along(dfs_motif)) {
    # get the dataframe
    item <- dfs_motif[[i]]
    # get the data frame name
    name <- names(dfs_motif)[i]

    # get matching motif results (if it exists)
    motif_results_sub <- motif_results[[name]]
    if (is.null(motif_results_sub)) {
      print("no motif results, next!")
    } else {
      print("motif results exist! cool!")
      for (row in seq_len(nrow(motif_results_sub))) {
        tryCatch(
          {
            item[
              motif_results_sub$Start[row]:motif_results_sub$End[row],
              c("motif", "distance", "length")
            ] <- motif_results_sub[row, c("instance", "Distance", "length")]
          },
          error = function(e) {
            print("error")
          }
        )
      }
      dfs_motif[[i]] <- item
    }
  }
  return(dfs_motif)
}

dfs_motif_new <- motif_match_df(dfs_motif, motif_results_9)


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
# should instead turn n less than 20 into NA???

motif_filtering <- function(dfs_motif_new) {
  dfs_motif_new_2 <- lapply(dfs_motif_new, function(x) {
    x <- x %>%
      dplyr::group_by(motif) %>%
      dplyr::mutate(n = ifelse(is.na(motif),
        NA,
        sum(!is.na(motif),
          na.rm = TRUE
        )
      )) %>%
      dplyr::mutate(n = ifelse(n < 20,
        NA,
        n
      )) # %>%
    # dplyr::filter(is.na(n) | n >= 20)
  })
  return(dfs_motif_new_2)
}

dfs_motif_new_2 <- motif_filtering(dfs_motif_new)

# Step 6 write the files to the output folder (himepath = hime-clean at the
# moment)
# writing dfs_motif_sub_2 to a csv file
# need current site ID, month ID, and index


save_final_ts <- function(dfs_motif, himepath) {
  invisible(
    lapply(dfs_motif,
      create_file_name_motif_csv,
      himepath = himepath
    )
  )
}

create_file_name_motif_csv <- function(df, himepath) {
  month_id <- unique(df$month)
  site_id <- unique(df$site)
  index_id <- names(df)[1]
  file_id <- paste0(
    "Motif_",
    index_id, "_",
    month_id, "_",
    site_id,
    ".csv"
  )

  write.csv(
    x = df,
    file = file.path(
      get_data_path(himepath),
      file_id
    ),
    row.names = F
  )
}

save_final_ts(dfs_motif_new_2, himepath = himepath)


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

# this will lapply the function to each dataframe in dfs_motif_new_2

ts_plot_call <- function(dfs_motif, motif_results, pathid) {
  if (!dir.exists(pathid)) {
    dir.create(pathid)
  }

  for (i in seq_along(dfs_motif)) {
    current_df_name <- names(dfs_motif)[[i]]
    matching_motif <- motif_results[[current_df_name]]
    if (!is.null(matching_motif)) {
      ts_plot_construct(dfs_motif[[i]], matching_motif, pathid)
    } else {
      print("no motifs for this timeseries/index, skipping plot...")
    }
  }
}

ts_plot_call(dfs_motif_new_2, motif_results_9, pathid = "output/figures/")

# Takes one data frame as input, and returns one plot as output
ts_plot_construct <- function(x, motif_list, pathid) {
  x_new <- ts_plot_prep(x)
  x_ts <- ts_plot_prep_ts(x_new)
  x_motif <- ts_plot_prep_motif(x_new)
  file_id <- create_file_name_index_plot(x)
  plot_out <- ts_plot_together(x_ts, x_motif, motif_list)
  ggplot2::ggsave(filename = get_data_path(pathid, file_id), plot = plot_out)
}

# Rename index column to "Index"
ts_plot_prep <- function(x) {
  # Get the index name of the current dataframe
  current_index <- names(x)[1]
  print(current_index)
  names(x)[1] <- "Index"
  return(x)
}

ts_plot_prep_ts <- function(x) {
  # Create a motified data frame for plotting
  plot_ts <- x %>%
    dplyr::ungroup() %>%
    dplyr::select(Index, reference, position, date, time) %>%
    tidyr::separate(.,
      reference,
      into = c("number", "what"),
      remove = FALSE
    )
}

ts_plot_prep_motif <- function(x) {
  # Create a motified data frame for plotting
  # For the geom_lines
  plot_motif <- x %>%
    dplyr::ungroup() %>%
    dplyr::select(motif, position, Index) %>%
    dplyr::rename(., reference = motif) %>%
    dplyr::filter(reference != "NA") %>%
    tidyr::separate(reference,
      into = c("number", "what"),
      remove <- FALSE,
      sep = "_"
    )
}


# Construct the plot
# line parts commented out weren't working or something
ts_plot_together <- function(plot_ts, plot_motif, motif_list) {
  plot_out <- ggplot2::ggplot(plot_ts, ggplot2::aes(x = position, y = Index)) +
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
      motif_list
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
}

# The order of naming is different here to previously?
create_file_name_index_plot <- function(df) {
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
