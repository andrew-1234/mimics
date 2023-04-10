#' Run AP commands in R wrapper function
#'
#' Tries to pass a command to AP or else return AP not installed error.
#' - modify to #.Platform$OS.type unix vs windows
#' - A2O files have information in the names so 1 directory per file is self
#'   explanatory for now.
#'
#' @param command Character string; Command to pass to AP
#'
#' @return Returns the output of the AP command in the console.
#'
#' @examples
#' ap_main("help")
#'
#' @export ap_main
ap_main <- function(command) {
  if (.Platform$OS.type == "unix") {
    if (find_program("AP")) {
      try(system2("AP", command))
    } else {
      stop("AP is not installed or detected in PATH")
    }
  } else {
    try(system2("powershell AP", command))
  } # untested on windows
}

#' Generate acoustic indices using Analysis Programs
#'
#' Generates acoustic indices using AP with Towsey config, for all `flac` files
#' detected in the supplied path.
#'
#' @param path_to_audio Character string; The path to your audio files.
#' @param path_to_output Character string; The output directory name that you
#' would like to use to store the results in. Created if it doesn't exist.
#'
#' @return Returns AP generated files in the output directory.
#'
#' @examples
#' ap_prepare("data/my_audio", "output/indices")
#'
#' @export ap_prepare
ap_prepare <- function(path_to_audio, path_to_output) {
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
    ap_get_audio_files(
      dirs = dirs,
      base_output_directory = base_output_directory
    )
  }
}

#' Runs `which` package
#'
#' Check if a package is available on path or not.
#'
#' @param package name of package to check
#'
#' @examples
#' find_program("ffmpeg")
#'
#' @export find_program
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
      cat(c(package, "is not installed\n"), collapse = " ")
      return(invisible(FALSE))
    } else {
      cat(c("package", package, "is installed\n"), collapse = " ")
      return(invisible(TRUE))
    }
  }
}

# ap_prepare helper function to get audio files
#'
#' This function is called in `ap_prepare`. It gets the files, builds, and runs
#' the AP commands.
#'
#' @param dirs Specified in AP_prepare: where audio files are stored.
#' @param base_output_directory Specified in AP_prepare: where output files
#' should be stored.
#'
#' @keywords internal
ap_get_audio_files <- function(dirs, base_output_directory) {
  for (directory in dirs) {
    files <- list.files(directory, pattern = "*.flac", full.names = TRUE)

    for (file in files) {
      if (.Platform$OS.type == "windows") {
        message("Processing ", file)

        # get just the name of the file
        file_name <- basename(file)

        # make a folder for results
        output_directory <-
          normalizePath(file.path(base_output_directory, file_name))
        if (!dir.exists(output_directory)) {
          dir.create(output_directory, recursive = TRUE)
        }

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
        # must work false silences the error because the directory doesn't exist
        # yet
        output_directory <-
          normalizePath(file.path(base_output_directory, file_name), mustWork = FALSE)

        if (!dir.exists(output_directory)) {
          dir.create(output_directory, recursive = TRUE)
        }

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
        ap_main(command)
      }
    }
  }
}
