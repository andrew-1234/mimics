#list_myfiles is going to list all the files in one folder because most times we will be using for loops and doing the same task in different files.

# folder <- "data/indices-output/"

# list_myfiles <- function(step = NULL, search_pattern) {
#         if (is.null(step)) {
#                 list.files(
#                         get_data_path(folder),
#                         pattern = search_pattern,
#                         recursive = T,
#                         full.names = T
#                 )
#
#         } else {
#                 list.files(
#                         get_data_path(folder, step),
#                         pattern = search_pattern,
#                         recursive = T,
#                         full.names = T
#                 )
#         }
#
# }

# create_mydir will create a directory inside the parent folder so we can store results
# create_mydir <- function(current_step) {
#         dir.create(getDataPath(folder, current_step))
# }

# ordering_files will be needed in step 2 when we build the time-series, i.e. putting files in order.

#' Ordering files
#'
#' Needed to build the time-series (put the files in order)
#' @param original_dataframe
#' @param index_name
#'
#' @return todo
#' @export
#'
#' @examples
ordering_files <- function(original_dataframe, index_name) {
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


