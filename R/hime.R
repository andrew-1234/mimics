#' Run hime
#'
#' @param timeseriesdata the folder you output the timeseries data to
#' @param himeoutput the folder you want to store the hime output
#' @param himepath is the path to your hime_release.jar. default is a folder in your WD
#'
#' @return
#' @export
#'
#' @examples
#' run_hime(run_hime(timeseriesdata, himeoutput, himepath = "~/HIME/bin/HIME_release.jar"))
run_hime <- function(timeseriesdata, himeoutput, himepath = file.path(getwd(), "HIME/bin/HIME_release.jar")) {

        timeseries_output <- get_data_path(himeoutput)

        # create the directory if it doesn't exist
        if (!dir.exists(timeseries_output)) {
                dir.create(timeseries_output)
        }

        # double check the himepath is correct
        print(himepath)

        # get the list of timeseries .txt files
        ts_files <- list.files(timeseriesdata, pattern = "*.txt", full.names = TRUE)

        # for each file, run hime
        # using full paths
        # TODO this is the mac version, only tested with zsh. need a pwsh version.
        for (file in ts_files) {
                string_input_file <-
                        shQuote(get_data_path(file), type = "sh")

                string_output_file <-
                        shQuote(file.path(get_data_path(himeoutput), paste("Res_", basename(file), sep = "")),
                                type = "sh")

                # prepare commands

                # set where the tmp.log file will be located
                # for now just going to put it in the current working directory
                templogfile_create <- paste("4 32 >", shQuote(file.path(getwd(), "tmp.log")))

                # TODO: what does 4 32 do?
                run_hime_on_input <-
                        paste("-jar", himepath, string_input_file, templogfile_create)

                # set where the tmp.log file will be located again
                templogfile_pipe <- shQuote(file.path(getwd(), "tmp.log >"))

                # uses grep to output each string starting with " Motif" and pipe to a new .txt file
                grep_to_output_file <-
                        paste(" Motif", "tmp.log >",
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
