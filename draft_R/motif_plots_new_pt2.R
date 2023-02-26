motif_plot_master <- function(my_indices_data, outputfigures, himepath, threshold) {
  mimics_check_indices_data(my_indices_data)

  # part one ----
  split_data_ts <- mimic_split_data(my_indices_data)

  # do some plots, no output
  total_indices <<-
    c("AcousticComplexity", "EventsPerSecond", "TemporalEntropy")

  split_data_ts_pivot <- mimics_pivot_split(split_data_ts)
  plot_ts_run(split_data_ts_pivot, outputfigures)

  # part two ----
  # create a list of dataframes, one for each index
  dfs_timeseries <- df_per_index(split_data_ts)

  # read the hime files
  motif_results <- read_hime_files(dfs_timeseries)

  # wrangle
  motif_wrangle <- motif_wrangle(motif_results)

  # report on empty motif dfs after filtering / wrangling
  summary_empty_motif_dfs <- summary_empty_motif(motif_wrangle)

  # remove repeated motifs
  motif_wrangle_removed <- remove_repeated_wrapped(motif_wrangle,
    threshold = threshold
  )

  # match time series dfs to motif results
  ts_motif_matched <- motif_ts_match(dfs_timeseries, motif_wrangle_removed)

  # save final ts csv files
  save_final_ts(ts_motif_matched, himepath = himepath)

  # save final plots
  ts_plot_call(ts_motif_matched, motif_wrangle_removed, pathid = outputfigures)

  return(list(ts_motifs = ts_motif_matched, motif_dfs = motif_wrangle_removed))
}

mimics_check_indices_data <- function(my_indices_data) {
  # test that my_indices_data contains the right columns
  tryCatch(
    {
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
    },
    error = function(e) {
      cat("Input data is not of valid input type and may contain incorrect collumns, please double check your input data.\n")
    }
  )
}

# part 1 ----

mimic_split_data <- function(my_indices_data) {
  split_data <- split(
    my_indices_data,
    interaction(my_indices_data$site, my_indices_data$month),
    drop = TRUE # levels that do not occur should be dropped
  )

  split_data_ts <- lapply(
    split_data,
    function(x) {
      x %>%
        dplyr::mutate(., position = seq_len(nrow(.)))
    }
  )
  return(split_data_ts)
}

mimics_pivot_split <- function(split_data_ts) {
  split_data_ts_pivot <- lapply(split_data_ts, function(x) {
    x <- x %>%
      dplyr::select(position, dplyr::all_of(total_indices)) %>%
      tidyr::pivot_longer(.,
        cols = 2:4,
        names_to = "index",
        values_to = "value"
      )
  })
  return(split_data_ts_pivot)
}

# define a function that plots the time series
plot_ts <- function(x, name) {
  ggplot2::ggplot(x, ggplot2::aes(x = position, y = value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(. ~ index) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::ggtitle(name)
}

plot_ts_run <- function(split_data_ts_pivot, outputfigures) {
  # get element names
  ele_names <- names(split_data_ts_pivot)

  # make names suitable for file naming use
  # remove dots and replace with underscores
  ele_names <- gsub("\\.", "_", ele_names)

  # add the suffix now
  ele_names <- paste(ele_names, "indicespertime.jpg", sep = "_")

  for (i in seq_along(split_data_ts_pivot)) {
    plot <- plot_ts(split_data_ts_pivot[[i]], ele_names[i])
    ggplot2::ggsave(file.path(outputfigures, ele_names[i]))
  }
}

# part 2 ---
df_per_index <- function(split_data_ts) {
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

    # dfs_motif[[i]] <- this_list
    dfs_motif <- c(dfs_motif, this_list)
  }
  # replace . with _ in the names of dfs_motif
  # so that the names can be used to generate file names
  names(dfs_motif) <- gsub("\\.", "_", names(dfs_motif))
  return(dfs_motif)
}

summary_empty_motif <- function(motif_wrangle) {
  nrow <- sapply(motif_wrangle, nrow)
  for (i in seq_along(nrow)) {
    if (is.null(nrow[[i]])) {
      nrow[[i]] <- 0
    }
  }
  summary_empty_motif <- data.frame(
    motif = names(motif_wrangle),
    nrow = unlist(nrow),
    row.names = NULL
  )
  return(summary_empty_motif)
}
