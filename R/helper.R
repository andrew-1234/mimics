
# get dat path function

get_data_path <- function(...) {
        return(file.path(getwd(),  ...))
}
#list_myfiles is going to list all the files in one folder because most times we will be using for loops and doing the same task in different files.

folder <- "data/indices-output/"

list_myfiles <- function(step = NULL, search_pattern) {
        if (is.null(step)) {
                list.files(
                        get_data_path(folder),
                        pattern = search_pattern,
                        recursive = T,
                        full.names = T
                )

        } else {
                list.files(
                        get_data_path(folder, step),
                        pattern = search_pattern,
                        recursive = T,
                        full.names = T
                )
        }

}

# create_mydir will create a directory inside the parent folder so we can store results
create_mydir <- function(current_step) {
        dir.create(getDataPath(folder, current_step))
}

# ordering_files will be needed in step 2 when we build the time-series, i.e. putting files in order.

ordering_files <-
        function(original_dataframe,
                 index_name) {
                if (is.null(index_name)) {
                        c <- read.csv(original_dataframe) %>%
                                with(., .[order(date_time, ResultMinute), ])
                        new_dataframe <<- rbind(new_dataframe, c)
                        write.csv(new_dataframe,
                                  getDataPath(folder,
                                              step2,
                                              paste("TS_", geo, "_", month, ".csv", sep = "")),
                                  row.names = F)

                } else {
                        c <- read.csv(original_dataframe) %>%
                                with(., .[order(date_time, ResultMinute), ]) %>%
                                select(., all_of(index_name))
                        new_dataframe <<- rbind(new_dataframe, c)
                        write.table(
                                new_dataframe,
                                getDataPath(
                                        folder,
                                        step2,
                                        paste("TS_", geo,
                                              "_",
                                              month,
                                              "_",
                                              index_name,
                                              ".txt",
                                              sep = "")
                                ),
                                row.names = F,
                                col.names = F
                        )


                }
        }

# HIME calls the HIME algorithm in powershell. This can be done separately by openning Powershell and running the command directly, I just put it here so it is easier for people without familiarity with Powershell.

# TODO need to make windows/vs mac compatible. can you call powershell on windows with pwsh?
#' Title
#'
#' @param command
#'
#' @return
#' @export
#'
#' @examples
HIME <- function(command) {
        system2('pwsh', command)
}

# This might be the most complicated function here, but nothing to worry about. iteration1 will be used when cleaning up the motifs seeing there are lots of repetition. Again, the authors of the paper that did the algorithm and everything propose a different solution to cleaning them up, but this was the way I found to make it easier and straightforward when dealing with heaps of data as I was. This function is assigning the word “repeated” to motifs that overlap.
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

# Now this function actually cleans up the motifs. remove_repeated runs the iteration 3 times so it removes all the possible repetitions and overlaps and remove motifs assigned as repeated.

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
# Firstly name the parent folder where you have your indices and where we will be creating new folders to save our results

# folder <- "AIndices"

# step1 <- "1_IndicesToTs"
# step2 <- "2_MonthlyAI"
# step3 <- "3_HIME"
# step4 <- "4_ProcessingRes"
# step5 <- "5_CleaningUp"
# step6 <- "6_CompleteMotif"
# step7 <- "7_CropSpectrogram"
# step8 <- "8_FeatureExtraction"


