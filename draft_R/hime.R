# set the directory with the input files for hime
# should still be timeseries_input
timeseries_input

# set an output directory for HIME
# timeseries_input <- get_data_path("data/timeseries-input/")
timeseries_output <- get_data_path("data/timeseries-output/")

# create the directory if it doesn't exist
if (!dir.exists(timeseries_output)) {
        dir.create(timeseries_output)
}

#Build the command
# make a function here that checks for hime in input directory or else specify path to hime
# will work on mac but windows will need to check
# building function to use zsh with mac
# TODO: test powershell option and how to handle the txt fiels output by select-string
# setting hime path (windows and zsh version needed)
# i don't think quotes etc work on mac
HIME_path <-
        paste("~/HIME/bin/HIME_release.jar", sep  = "")

# if windows
# paste("'", "~/HIME/bin/HIME_release.jar", "'", sep  = "")

#Listing files
ts_files <- list.files(timeseries_input, pattern = "*.txt", full.names = TRUE)


# TODO: fix quoting etc.
# NOTE: If i use pwsh as the default shell instead of zsh, i can use `` and don't worry about the backslashes. TODO: fix for all instaces of invoking shell.
# TODO: this won't work on mac if there is a space in a file path, need to escape it
# actually just need to force using powershell. add the quotes in where i deleted them in prvious arguements
# TODO: could specify the output location of tmp.log? instead of assuming it gets created in /bin?

#Iterate through each file, prepare the command and execute in the for loop
# MAC version
# calling pwsh with system2 is crashing my Rstudio
# try just do it zshell, is there a way to format for output to zshell? Shquote?
# shQuote(command2, type = "sh")
# note: the working directory is set in shell commands as the R project working directory
# note: system 2 needs command and then args format

# note: set shQuote for the file paths

for (file in ts_files) {
        string_input_file <-
                paste(timeseries_input, basename(file),sep = "")
        string_input_file <- shQuote(string_input_file, type = "sh")

        string_output_file <-
                paste(timeseries_output,
                      "Res_", basename(file),
                      sep = "")
        string_output_file <- shQuote(string_output_file, type = "sh")
        # prepare command
        # TODO: what does 4 32 do?
        run_hime_on_input <-
                paste("-jar", HIME_path, string_input_file, "4 32 > tmp.log")

        # working but need to allow for different HIME path, where log file will be located
        # TODO:
        grep_to_output_file <-
                paste(" Motif", "tmp.log >",
                      string_output_file
                )

        # finally, execute the command
        #HIME(cd_timeseries_input) # change directory to input folder
        system2("java", run_hime_on_input) # run the hime command
        system2("grep", grep_to_output_file) # pipe the results into an output file with a real name
}

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
