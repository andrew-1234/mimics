#' Get data path
#'
#' Enter a directory relative to the current working directly, and return the full path.
#' @param ... enter your directory or multiple directories
#'
#' @return a full path
#' @export
#'
#' @examples
#' get_data_path()
#' get_data_path("data/sounds")
get_data_path <- function(...) {
        return(file.path(getwd(),  ...))
}
