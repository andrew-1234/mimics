motif_ts_match <- function(dfs_motif, motif_results) {
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
  return(motif_filtering(dfs_motif))
}

# filter
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
  message("👍 motif filtering complete")
  return(dfs_motif_new_2)
}
