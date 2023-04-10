#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Get data path
#'
#' Enter a directory relative to the current working directly, and return the
#' full path.
#' @param ... enter your directory or multiple directories
#'
#' @return A full path
#'
#' @examples
#' get_data_path()
#' get_data_path("data/sounds")
#'
#' @keywords internal
get_data_path <- function(...) {
  return(file.path(getwd(), ...))
}
