# todo general
# research try function()
# research shQuote

# todo - simplify to return 1 or 0
# modify to #.Platform$OS.type? unix vs windows
# this file was first authored by Anthony and then modified by Andrew to run on macOS

# get_data_path("data/Motif_data/")

# run AP commands in R function
call_AP <- function(command) {
        if (.Platform$OS.type == "unix") {
                if (find_program("AP") == "package AP is installed") {
                        try(system2('AP', command))
                } else
                        print("AP is not installed or detected in PATH")
        } else
                try(system2('powershell AP', command)) # untested on windows
}


AP_prepare <- function(path_to_audio, path_to_output) {

  my_recordings <- get_data_path(path_to_audio)

  #Set directory containing the folders with audio files

  dirs <- list.dirs(path = my_recordings, recursive = FALSE)
        cat("detected", length(dirs), "different directories in folder", basename(my_recordings))

        if (interactive())
                answer <- askYesNo("Is this the right number of sensors/sites?", )

        if (answer == FALSE){
                cat("No.", "Cancelling action.")
                #stopQuietly()
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

        # now get the audio files
        if (interactive())
                answer <- askYesNo("Ready to process audio and generate indices. This will take a while. Proceed?", )

        if (answer == FALSE){
                cat("No.", "Cancelling action.")

        } else {
                cat("Yes.", "Continuing...")
        get_audio_files(dirs = dirs, base_output_directory = base_output_directory)
        }
}




# Get a list of audio files inside the directory
# (Get-ChildItem is just like ls, or dir)
# create a setting for flac or wav, or detect, or both?

# tested for 1 directory, now run for a list of directories done
# TODO: sort the output into directories that mimic the parent directory strucutre
# the A2O files have information in the names so 1 directory per file is self explanatory
# but some people might not have such detailed names in which case preserved structure could be useful


get_audio_files <- function(dirs, base_output_directory) {

        for (directory in dirs){

                files <- list.files(directory, pattern = "*.flac", full.names = TRUE)

                # iterate through each file NEW ----

                for(file in files) {

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
                          sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" ',
                                  file,
                                  output_directory)

                  # finally, execute the command
                  system2('C:\\AP\\AnalysisPrograms.exe', command)
          } else {
                  message("Processing ", file)
                  # get just the name of the file
                  file_name <- basename(file)

                  # make folders for the results (one folder per audio file)
                  # i dont think this part is working TODO but the script is auto making folders anyway??
                  # maybe create directories at the start of the script and iterate through the AP X amount of times changing the output directory each time
                  files <- list.files("data/A2O/253_SEQPSamfordDryA/", pattern = "*.flac", full.names = TRUE)
                  files <- files[1]
                  file_name <- basename(files)

                  base_output_directory <- "output/indices-output-ap"
                  output_directory <-
                          normalizePath(file.path(base_output_directory, file_name))

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
                  AP(command)
          }
  }
  }
}
