#----
# TODO general:
# research try function()
# research shQuote
# modify to #.Platform$OS.type? unix vs windows
# this file was first authored by Anthony and then modified by Andrew into functions and with zsh options
# --
# create a setting for dealing with different file types
# the A2O files have information in the names so 1 directory per file is self explanatory
# but some people might not have such detailed names in which case preserved structure could be useful
# add some default settings for these functions if no arguments are supplied
#----
# path_to_audio <- "data/A2O-mini-test"
# path_to_output <- "output/indices-output-ap-mini"

#' Run AP commands in R wrapper function
#'
#' Tries to pass a command to AP or else return AP not installed error.
#'
#' @param command command you want to pass to AP
#'
#' @return TODO
#' @export
#'
#' @examples
#' call_AP("help")
AP <- function(command) {
        if (.Platform$OS.type == "unix") {
                if (find_program("AP") == "package AP is installed") {
                        try(system2("AP", command))
                } else {
                        print("AP is not installed or detected in PATH")
                }
        } else {
                try(system2("powershell AP", command))
        } # untested on windows
}


#' This function runs AP analysis
#'
#' Generates acoustic indices using AP with Towsey config, for all `flac` files detected in the supplied path.
#'
#' @param path_to_audio the path to your audio files
#' @param path_to_output the output directory name that you would like to create and store the results in
#'
#' @return returns AP generated files
#' @export
#'
#' @examples
#' AP_prepare("data/my_audio", "output/indices")
AP_prepare <- function(path_to_audio, path_to_output) {
        if (!dir.exists("output")) {
                dir.create("output")
        }
        my_recordings <- get_data_path(path_to_audio)

        # Set directory containing the folders with audio files

        dirs <- list.dirs(path = my_recordings, recursive = FALSE)
        cat("detected", length(dirs), "different directories in folder", basename(my_recordings))

        if (interactive()) {
                answer <- askYesNo("Is this the right number of sensors/sites?")
        }

        if (answer == FALSE) {
                cat("No.", "Cancelling action.")
        } else {
                cat("Yes.", "Continuing...")

                # The directory to store the results
                # check if exists else create
                base_output_directory <- get_data_path(path_to_output)
                if (!dir.exists(base_output_directory)) {
                        cat("directory ", base_output_directory, "doesn't exist... creating directory", fill = 1)
                        dir.create(base_output_directory)
                } else {
                        cat("directory:", base_output_directory, "already exists...", fill = 1)
                }
        }

        # prepare to run the get audio files function
        # todo this doesn't work as intended. if you write no it continues
        if (interactive()) {
                answer <- askYesNo("Ready to process audio and generate indices. This could take a long time! Proceed?", )
        }

        if (answer == FALSE) {
                cat("No.", "Cancelling action.")
        } else {
                cat("Yes.", "Continuing...")
                motifR::get_audio_files(dirs = dirs, base_output_directory = base_output_directory)
        }
}



# directory <- dirs[1]
# file <- files[1]
#' Get audio files and run AP analysis
#'
#' This function is called in AP_prepare. It gets the files, builds, and runs the AP commands
#'
#' @param dirs specified in AP_prepare: where audio files are stored
#' @param base_output_directory specified in AP_prepare: where output files should be stored
#'
#' @export
get_audio_files <- function(dirs, base_output_directory) {
        for (directory in dirs) {
                files <- list.files(directory, pattern = "*.flac", full.names = TRUE)

                # iterate through each file NEW ----

                for (file in files) {
                        if (.Platform$OS.type == "windows") {
                                message("Processing ", file)

                                # get just the name of the file
                                file_name <- basename(file)

                                # make a folder for results
                                output_directory <-
                                        normalizePath(file.path(base_output_directory, file_name))
                                dir.create(output_directory, recursive = TRUE)

                                # prepare command (this might be outdated?)
                                command <-
                                        sprintf(
                                                'audio2csv "%s" "Towsey.Acoustic.yml" "%s" ',
                                                file,
                                                output_directory
                                        )

                                # finally, execute the command
                                system2("C:\\AP\\AnalysisPrograms.exe", command)
                        } else {
                                message("Processing ", file)

                                # get just the name of the file
                                file_name <- basename(file)

                                # make folders for the results (one folder per audio file)
                                # mustwork false silences the error because the directory doesn't exist yet
                                output_directory <-
                                        normalizePath(file.path(base_output_directory, file_name), mustWork = FALSE)

                                dir.create(output_directory, recursive = TRUE)
                                # prepare command
                                command <-
                                        paste(
                                                "audio2csv",
                                                shQuote(file, type = "sh"),
                                                "Towsey.Acoustic.yml",
                                                shQuote(output_directory, type = "sh"),
                                                collapse = " "
                                        )

                                # finally, execute the command
                                motifR::AP(command)
                        }
                }
        }
}
