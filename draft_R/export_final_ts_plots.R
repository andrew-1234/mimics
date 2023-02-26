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

# Takes one data frame as input, and returns one plot as output
ts_plot_construct <- function(x, motif_list, pathid) {
  x_new <- ts_plot_prep(x)
  x_ts <- ts_plot_prep_ts(x_new$x)
  x_motif <- ts_plot_prep_motif(x_new$x)
  file_id <- create_file_name_index_plot(x)
  plot_out <- ts_plot_together(x_ts, x_motif, motif_list, x_new$current_index)
  ggplot2::ggsave(filename = get_data_path(pathid, file_id), plot = plot_out)
}

# Rename index column to "Index"
ts_plot_prep <- function(x) {
  # Get the index name of the current dataframe
  current_index <- names(x)[1]
  print(current_index)
  names(x)[1] <- "Index"
  return(list(x = x, current_index = current_index))
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
ts_plot_together <- function(plot_ts, plot_motif, motif_list, indexstring) {
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
    ggplot2::labs(title = indexstring) +
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
