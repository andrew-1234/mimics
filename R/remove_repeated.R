#' Iteration function
#'
#' This might be the most complicated function here, but nothing to worry about. iteration1 will be used when cleaning up the motifs seeing there are lots of repetition. Again, the authors of the paper that did the algorithm and everything propose a different solution to cleaning them up, but this was the way I found to make it easier and straightforward when dealing with heaps of data as I was. This function is assigning the word “repeated” to motifs that overlap.
#'
#' @param motif_results_df a motif results df
#'
#' @return returns a motif results df
#' @export
#'
#' @examples
iteration1 <- function(motif_results_df) {
        for (row in 1:nrow(motif_results_df)) {
                motif_results_df$overlap[row] = case_when(
                        motif_results_df$Start[row + 1] <= motif_results_df$Start[row] &
                                motif_results_df$End[row + 1] >= motif_results_df$End[row] ~ "repeated",
                        motif_results_df$Start[row +
                                                       1] <= motif_results_df$End[row] &
                                motif_results_df$End[row + 1] <= motif_results_df$End[row] ~ "repeated",
                        motif_results_df$Start[row +
                                                       1] <= motif_results_df$End[row] &
                                motif_results_df$End[row + 1] - motif_results_df$End[row] <= 30 ~ as.character(motif_results_df$Distance[row +
                                                                                                                                                 1]),
                        motif_results_df$Start[row +
                                                       1] <= motif_results_df$End[row] &
                                motif_results_df$Start[row + 1] - motif_results_df$Start[row] <= 30 ~ as.character(motif_results_df$Distance[row +
                                                                                                                                                     1]),
                        motif_results_df$Start[row +
                                                       1] - motif_results_df$Start[row] <= 30 &
                                motif_results_df$Start[row + 1] - motif_results_df$End[row] <= 30 ~ as.character(motif_results_df$Distance[row +
                                                                                                                                                   1]),
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
                filter(., .$overlap != "repeated") %>%
                with(., .[order(Start), ]) %>%
                iteration1(.) %>%
                filter(., .$overlap != "repeated") %>%
                with(., .[order(Start), ]) %>%
                iteration1(.) %>%
                filter(., .$overlap != "repeated") %>%
                with(., .[order(Start), ])
}
