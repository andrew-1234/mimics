# set the directory with the input files for hime
# should still be timeseries_input
timeseries_input

# set an output directory for HIME
timeseries_output <- get_data_path("data/timeseries-output/")

# create the directory if it doesn't exist
if (!dir.exists(timeseries_output)) {
        dir.create(timeseries_output)
}

#Build the command
# make a function here that checks for hime in input directory or else specify path to hime
# will work on mac but windows will need to check

# setting hime path (windows and zsh version needed)
# i don't think quotes etc work on mac
HIME_path <-
        paste("~/HIME/bin/HIME_release.jar", sep  = "")

noquote(HIME_path)

# if windows
# paste("'", "~/HIME/bin/HIME_release.jar", "'", sep  = "")

#Command to set directory in shell
# i don't think this is needed any more
cd_timeseries_input <- paste("cd ", timeseries_input, sep = "")

#Listing files
ts_files <- list.files(timeseries_input, pattern = "*.txt", full.names = TRUE)


# TODO: fix quoting etc.
# NOTE: If i use pwsh as the default shell instead of zsh, i can use `` and don't worry about the backslashes. TODO: fix for all instaces of invoking shell.
# TODO: this won't work on mac if there is a space in a file path, need to escape it
# actually just need to force using powershell. add the quotes in where i deleted them in prvious arguements
# TODO: could specify the output location of tmp.log? instead of assuming it gets created in /bin?

#Iterate through each file, prepare the command and execute in the for loop

ts_files <- ts_files[1]

for (file in ts_files) {
        pws_input_file <-
                paste("'", timeseries_input, basename(ts_files), "'", sep = "")
        pws_output_file <-
                paste("'",
                      timeseries_output,
                      "Res_", basename(ts_files),
                      "'",
                      sep = "")

        # prepare command
        # TODO: what does 4 32 do?
        command2 <-
                paste("java -jar", HIME_path, pws_input_file, "4 32 > tmp.log")

        # working but need to allow for different HIME path which is where log will store
        command3 <-
                paste("select-string Motif", paste(c("~/HIME/bin/", "tmp.log >"), collapse = ""), pws_output_file)

        # finally, execute the command
        #HIME(cd_timeseries_input) # change directory to input folder
        HIME(command2) # run the hime command
        HIME(command3) # pipe the results into an output file with a real name
}

system('pwsh')
# powershell is crashign Rstudio
# try just do it zshell, is there a way to format for output to zshell? Shquote?
# shQuote(command2, type = "sh")
