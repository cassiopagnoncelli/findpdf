# Suppress R CMD check NOTEs for NSE variables used in dplyr and ggplot2
utils::globalVariables(c("value", "group", "prob", "x", "y"))

#' Package initialization
#'
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Suppress S3 method registration messages
  suppressMessages({
    # S3 methods are automatically registered via NAMESPACE
    invisible(NULL)
  })
}

#' Package attach hook
#'
#' @param libname Library name
#' @param pkgname Package name
#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  # Silently attach without messages
  invisible(NULL)
}
