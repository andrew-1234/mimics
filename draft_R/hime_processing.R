# list the files in the timeseries_output path
files <- list.files(path = timeseries_output, pattern = "*.txt", full.names = TRUE)

# set an output directory
# TODO: create an input and output folder in data rather than having lots of different folders being created in data directory

# sets the output directory for cleaned hime files
hime_cleaned_path <- get_data_path("data/hime-cleaned/")
if (!dir.exists(hime_cleaned_path)) {
        dir.create(hime_cleaned_path)
}

# macOS ----
# maybe don't need to export this?
for (file in files) {
        read.table(
                file,
                sep = " ",
                blank.lines.skip = T,
        ) %>%
                select(., 2:7) %>%
                write.table(file.path(hime_cleaned_path, basename(file)), #TODO clean up
                            row.names = F,
                            col.names = F)
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
