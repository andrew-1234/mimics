# function input: a indices time series data frame

pathid <- "output/figures"
my_indices_data <- my_indices_data

motif_plot_master <- function(my_indices_data, pathid) {
  
  mimics_check_indices_data(my_indices_data)

  split_data_ts <- mimic_split_data(my_indices_data)
  
  # do some plots, no output
  split_data_ts_pivot <- mimics_pivot_split(split_data_ts)
  plot_ts_prep(split_data_ts_pivot, pathid)
  # TODO START HERE START HERE 20/FEB/2023
  # continue with other stuff
  # line 162
}

mimics_check_indices_data <- function(my_indices_data) {
# test that my_indices_data contains the right columns
  tryCatch({
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
  }, error = function(e) {
    cat("Input data is not of valid input type and may contain incorrect collumns, please double check your input data.\n")
  })
}

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

plot_ts_prep <- function(split_data_ts_pivot, pathid) {
  # get element names
  ele_names <- names(split_data_ts_pivot)

  # make names suitable for file naming use
  # remove dots and replace with underscores
  ele_names <- gsub("\\.", "_", ele_names)

  # add the suffix now
  ele_names <- paste(ele_names, "indicespertime.jpg", sep = "_")

  for (i in seq_along(split_data_ts_pivot)) {
    plot <- plot_ts(split_data_ts_pivot[[i]], ele_names[i])
    ggplot2::ggsave(file.path(pathid, ele_names[i]))
  }
}

# save time series
save_final_ts(dfs_motif_new_2, himepath = himepath)

# function master plot
ts_plot_call(dfs_motif_new_2, motif_results_9, pathid = "output/figures/")
