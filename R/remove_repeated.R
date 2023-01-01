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
        for (row in 1:nrow(motif_results_df)) {
                motif_results_df$overlap[row] <- dplyr::case_when(
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
                dplyr::filter(., .$overlap != "repeated") %>%
                with(., .[order(Start), ]) %>%
                iteration1(.) %>%
                dplyr::filter(., .$overlap != "repeated") %>%
                with(., .[order(Start), ]) %>%
                iteration1(.) %>%
                dplyr::filter(., .$overlap != "repeated") %>%
                with(., .[order(Start), ])
}
