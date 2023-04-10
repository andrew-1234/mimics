#' Run hime
#'
#' @param timeseriesdata the folder you output the timeseries data to
#' @param himeoutput the folder you want to store the hime output
#' @param himepath is the path to your hime_release.jar. default is a folder in your WD
#'
#' @return
#' @export run_hime
#'
#' @examples
#' run_hime(run_hime(timeseriesdata, himeoutput, himepath = "~/HIME/bin/HIME_release.jar"))
run_hime <- function(timeseriesdata, himeoutput, himepath = file.path(getwd(), "HIME-master/bin/HIME_release.jar")) {
  # look for HIME_release.jar in himepath
  if (!file.exists(himepath)) {
    stop("HIME_release.jar not found. Please check your himepath.")
  } else {
    cat("HIME_release.jar found. Proceeding with HIME analysis.\n")
  }

  # get the full path to the output directory
  timeseries_output <- get_data_path(himeoutput)

  # create the output directory if it doesn't exist
  if (!dir.exists(timeseries_output)) {
    dir.create(timeseries_output)
  }

  # get the list of timeseries .txt files
  ts_files <- list.files(timeseriesdata, pattern = "*.txt", full.names = TRUE)

  # for each file, run hime using full path
  # TODO this is the mac version, only tested with zsh. need a pwsh version.
  for (file in ts_files) {
    string_input_file <-
      shQuote(get_data_path(file), type = "sh")

    string_output_file <-
      shQuote(file.path(get_data_path(himeoutput), paste("Res_", basename(file), sep = "")),
        type = "sh"
      )

    # prepare shell commands, using shQuote to escape special characters
    # set where the tmp.log file will be located in the working directory
    templogfile_create <- paste("4 32 >", shQuote(file.path(getwd(), "tmp.log")))

    # TODO: what does 4 32 do?
    run_hime_on_input <-
      paste("-jar", himepath, string_input_file, templogfile_create)

    # set where the tmp.log file will be located again
    templogfile_pipe <- shQuote(file.path(getwd(), "tmp.log >"))

    # uses grep to output each string starting with " Motif" and pipe to a new .txt file
    grep_to_output_file <-
      paste(
        " Motif", "tmp.log >",
        string_output_file
      )

    # finally, execute the commands
    system2("java", run_hime_on_input) # run the hime command
    system2("grep", grep_to_output_file) # pipe the results into an output file with a real name
  }

  # remove the left over tmp.log file in the working directory
  unlink(file.path(getwd(), "tmp.log"))

  # run the hime_processing function which cleans up the file a little bit
  # could probably integrate this step better so that cleanup happens on the original hime files
  hime_processing(himeoutput)

  # did the hime analysis work and produce output files?
  hime_clean_files <- length(
    list.files(himeoutput, pattern = "*.txt")
  )
  if (hime_clean_files == 0) {
    stop("No HIME output files found. Check your function arguements.")
  } else {
    sprintf("HIME analysis complete, %d output files found.", hime_clean_files)
  }
}

# if windows hime path settings
# paste("'", "~/HIME/bin/HIME_release.jar", "'", sep  = "")

# TODO: powershell version
# calling pwsh with system2 is crashing my Rstudio for some reason. so creating a zsh option (above)

# powershell version (in progress)----
# for (file in ts_files) {
#         string_input_file <-
#                 paste("'", timeseries_input, basename(ts_files), "'", sep = "")
#         string_output_file <-
#                 paste("'",
#                       timeseries_output,
#                       "Res_", basename(ts_files),
#                       "'",
#                       sep = "")
#
#         # prepare command
#         # TODO: what does 4 32 do?
#         command2 <-
#                 paste("java -jar", HIME_path, pws_input_file, "4 32 > tmp.log")
#
#         # working but need to allow for different HIME path which is where log will store
#         command3 <-
#                 paste("select-string Motif", paste(c("~/HIME/bin/", "tmp.log >"), collapse = ""), pws_output_file)
#
#         # finally, execute the command
#         #HIME(cd_timeseries_input) # change directory to input folder
#         HIME(command2) # run the hime command
#         HIME(command3) # pipe the results into an output file with a real name
# }

# calling pwsh shortcut
# HIME <- function(command) {
#         system2('pwsh', command)
# }

#' Hime processing
#'
#' @param himeoutput the output folder
#'
#' @return creates a (whatever you called your himeoutput)-clean folder with cleaned hime txt files
#'
#' @keywords internal
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
      blank.lines.skip = TRUE,
      skipNul = TRUE
    ) %>%
      dplyr::select(., 2:7) %>%
      write.table(file.path(hime_cleaned_path, basename(file)),
        row.names = FALSE,
        col.names = FALSE
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
