#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
  flag <- requireNamespace(library_name, quietly = TRUE)
  if (isFALSE(flag)) {
    msg <- sprintf("Please install the `%s` package.", library_name)
    return(msg)
  } else {
    return(TRUE)
  }
}

assert_dependency <- function(library_name) {
  flag <- check_dependency(library_name)
  if (!isTRUE(flag)) stop(flag, call. = FALSE)
  return(invisible())
}
