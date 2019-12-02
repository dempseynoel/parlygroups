# Functions to filter data if arguments used

#' Filter basic common parameter
#'
#' @keywords internal

filter_basic <- function(table, parameter, col_name) {
  if (is.character(parameter)) {
    table <- dplyr::filter(table, col_name %in% parameter)
  } else {
    table
  }
}

#' Filter between two values
#'
#' @keywords internal

filter_value <- function(table, from, to, col_name) {
  if (is.numeric(from) && is.numeric(to)) {
    table <- dplyr::filter(table, dplyr::between(col_name, from, to))
  } else if (is.numeric(from)) {
    to <- max(col_name)
    table <- dplyr::filter(table, dplyr::between(col_name, from, to))
  } else if (is.numeric(to)) {
    from <- min(col_name)
    table <- dplyr::filter(table, dplyr::between(col_name, from, to))
  } else {
    table
  }
}

#' Filter between two dates
#'
#' @keywords internal

filter_date <- function(table, from, to, col_name) {
  if (is.character(from) && is.character(to)) {
    table <- dplyr::filter(table, dplyr::between(col_name, as.Date(from), as.Date(to)))
  } else if (is.character(from)) {
    to <- max(col_name)
    table <- dplyr::filter(table, dplyr::between(col_name, as.Date(from), as.Date(to)))
  } else if (is.character(to)) {
    from <- min(col_name)
    table <- dplyr::filter(table, dplyr::between(col_name, as.Date(from), as.Date(to)))
  } else {
    table
  }
}
