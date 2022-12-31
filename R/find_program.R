#' Runs `which` package
#'
#' To check if a package is available on path or not
#'
#' @param package name of package to check
#'
#' @return todo
#' @export
#'
#' @examples
#' find_program("ffmpeg")
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
