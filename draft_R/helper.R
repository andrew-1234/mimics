# create_mydir will create a directory inside the parent folder so we can store results
# create_mydir <- function(current_step) {
#         dir.create(getDataPath(folder, current_step))
# }


#' Ordering files - NOTE: commented out because this should happen in time_series.R by default now. But double check this is working as intended.
#'
#' Needed to build the time-series (put the files in order)
#' @param original_dataframe
#' @param index_name
#'
#' @return todo
#' @export
#'
#' @examples
# ordering_files <- function(original_dataframe, index_name) {
#                 if (is.null(index_name)) {
#                         c <- read.csv(original_dataframe) %>%
#                                 with(., .[order(date_time, ResultMinute), ])
#                         new_dataframe <<- rbind(new_dataframe, c)
#                         write.csv(new_dataframe,
#                                   getDataPath(folder,
#                                               step2,
#                                               paste("TS_", geo, "_", month, ".csv", sep = "")),
#                                   row.names = F)
#
#                 } else {
#                         c <- read.csv(original_dataframe) %>%
#                                 with(., .[order(date_time, ResultMinute), ]) %>%
#                                 select(., all_of(index_name))
#                         new_dataframe <<- rbind(new_dataframe, c)
#                         write.table(
#                                 new_dataframe,
#                                 getDataPath(
#                                         folder,
#                                         step2,
#                                         paste("TS_", geo,
#                                               "_",
#                                               month,
#                                               "_",
#                                               index_name,
#                                               ".txt",
#                                               sep = "")
#                                 ),
#                                 row.names = F,
#                                 col.names = F
#                         )
#
#
#                 }
#         }


