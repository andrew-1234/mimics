# todo general
# research try function()
# research shQuote

# todo - simplify to return 1 or 0
# modify to #.Platform$OS.type? unix vs windows
# this file was first authored by Anthony and then modified by Andrew to run on macOS

find_program <- function(package) {
        if (Sys.info()[1] == "Darwin") {
                return <-
                        system(
                                paste(c("which", package), collapse = " "),
                                ignore.stderr = TRUE,
                                intern = FALSE,
                                ignore.stdout = TRUE
                        )

                if (return == 1) {
                        paste(c(package, "is not installed"), collapse = " ")
                } else {
                        paste(c("package", package, "is installed"), collapse = " ")
                }
        }
}

# get_data_path("data/Motif_data/")

# run AP commands in R function
AP <- function(command) {
        if (.Platform$OS.type == "unix") {
                if (find_program("AP") == "package AP is installed") {
                        try(system2('AP', command))
                } else
                        print("AP is not installed or detected in PATH")
        } else
                try(system2('powershell AP', command)) # untested on windows
}



AP("--version")

get_data_path("data/A2O")

#Set directory containing the folders with audio files----
dirs <- list.dirs(path = "data/A2O", recursive = FALSE)
dirs[1]

get_data_path(dirs[1])

# The directory to store the results
# check if exists else create
base_output_directory <- get_data_path("data/indices-output")
if (!dir.exists(base_output_directory)) {
        dir.create(base_output_directory)
}

# Get a list of audio files inside the directory
# (Get-ChildItem is just like ls, or dir)
# create a setting for flac or wav, or detect, or both?

# tested for 1 directory, now run for a list of directories done
# TODO: sort the output into directories that mimic the parent directory strucutre
# the A2O files have information in the names so 1 directory per file is self explanatory
# but some people might not have such detailed names in which case preserved structure could be useful

for (directory in dirs){
files <- list.files(get_data_path(directory), pattern = "*.flac", full.names = TRUE)
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


#concat ----



# Set the directory containing the files



# (Get-ChildItem is just like ls, or dir)

files <- list_myfiles(step2, search_pattern = glob2rx("TS_*.txt"))

# iterate through each file
for (file in files) {
  pws_input_file <-
    paste("'", input_directory, "/", basename(file), "'", sep = "")
  pws_output_file <-
    paste("'",
          output_directory,
          "/",
          gsub("TS_", "res", basename(file)),
          "'",
          sep = "")

  # prepare command

  command2 <-
    paste("java -jar", HIME_command, pws_input_file, "4 32 > tmp.log")
  command3 <-
    paste("select-string Motif tmp.log >", pws_output_file)

  # finally, execute the command
  HIME(command1)
  HIME(command2)
  HIME(command3)
}
