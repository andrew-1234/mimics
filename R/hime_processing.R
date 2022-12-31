#' Hime processing
#'
#' @param himeoutput the output folder
#'
#' @return creates a (whatever you called your himeoutput)-clean folder with cleaned hime txt files
#' @export
#'
#' @examples
hime_processing <- function(himeoutput) {
        # list the files in the himeoutput path
        files <- list.files(
                path = himeoutput,
                pattern = "*.txt", full.names = TRUE
        )
        files <- unlist(lapply(files, get_data_path))

        # sets the output directory for cleaned hime files
        hime_cleaned_path <- get_data_path(
                paste(himeoutput, "-clean", sep = "")
        )
        if (!dir.exists(hime_cleaned_path)) {
                dir.create(hime_cleaned_path)
        }

        # macOS ----
        # maybe don't need to export this?
        # could do this in shell. keep in mind the output from pwsh and grep looks slightly different
        # so this has to be able to handle both versions
        for (file in files) {
                read.table(
                        file,
                        sep = " ",
                        blank.lines.skip = T,
                ) %>%
                        dplyr::select(., 2:7) %>%
                        write.table(file.path(hime_cleaned_path, basename(file)),
                                row.names = F,
                                col.names = F
                        )
        }
}



# TODO windows ----
# for (file in files) {
#         read.table(
#                 file,
#                 sep = " ",
#                 blank.lines.skip = T,
#                 fileEncoding = "UTF-16" # this is important maybe on windows? on macOS it doesn't work
#         ) %>%
#                 select(., 2:7) %>%
#                 write.table(get_data_path(folder, step4, gsub("res", "", basename(file))),
#                             row.names = F,
#                             col.names = F)
# }
# get_data_path(... = "data/pwsh-test-hold/", basename(file))
#
# read.table(file = file,
#         sep = " ",
#         blank.lines.skip = T
# )
