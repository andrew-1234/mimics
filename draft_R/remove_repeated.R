#' Iteration function
#'
#' iteration1 will be used when cleaning up the motifs to remove repetition. This function is assigning the word “repeated” to motifs that overlap.
#'
#' @param motif_results_df a motif results df
#'
#' @return returns a motif results df
#' @export
#'
#' @examples
iteration1 <- function(motif_results_df) {
  # for each row in the dataframe
  for (row in 1:nrow(motif_results_df)) {
    # case when returns when true, or if false, checks the next case
    motif_results_df$overlap[row] <- dplyr::case_when(
      # first case: is start value of the next row less than or equal to the
      # start value of the current row, and is the end value of the next row
      # greater than or equal to the end value of the current row?
      motif_results_df$Start[row + 1] <= motif_results_df$Start[row] &
        motif_results_df$End[row + 1] >= motif_results_df$End[row] ~ "repeated",
      # second case
      motif_results_df$Start[row + 1] <= motif_results_df$End[row] &
        motif_results_df$End[row + 1] <= motif_results_df$End[row] ~ "repeated",
      # third case: test if the start or end of the next row is within 30 units
      # of the start or end of the current row, respectively. If either
      # of these onditions is TRUE, then the value of Distance for the
      # next row is returned as a character.
      # return as a character so that it will not be evaluated in the next iteration?
      motif_results_df$Start[row + 1] <= motif_results_df$End[row] &
        motif_results_df$End[row + 1] - motif_results_df$End[row] <= 30 ~
        as.character(motif_results_df$Distance[row + 1]),
      # fourth case
      motif_results_df$Start[row + 1] <= motif_results_df$End[row] &
        motif_results_df$Start[row + 1] - motif_results_df$Start[row] <= 30 ~
        as.character(motif_results_df$Distance[row + 1]),
      # fifth case
      motif_results_df$Start[row + 1] - motif_results_df$Start[row] <= 30 &
        motif_results_df$Start[row + 1] - motif_results_df$End[row] <= 30 ~
        as.character(motif_results_df$Distance[row + 1]),
      # final case: if none of the above are true, return the distance value of
      # the current row as a character
      TRUE ~ as.character(motif_results_df$Distance[row])
    )
  }
  motif_results_df <- motif_results_df
}

#' remove repeated
#'
#' Now this function actually cleans up the motifs.
#' remove_repeated runs the iteration 3 times so it removes
#' all the possible repetitions and overlaps and
#' removes motifs assigned as repeated.
#'
#' @param motif_results_df a motif results df
#'
#' @return
#' @export
#'
#' @examples
remove_repeated <- function(motif_results_df) {
  result <- iteration1(motif_results_df) %>%
    dplyr::filter(., .$overlap != "repeated") %>%
    with(., .[order(Start), ]) %>%
    iteration1(.) %>%
    dplyr::filter(., .$overlap != "repeated") %>%
    with(., .[order(Start), ]) %>%
    iteration1(.) %>%
    dplyr::filter(., .$overlap != "repeated") %>%
    with(., .[order(Start), ])
}
