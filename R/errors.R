# Functions for reporting errors

#' Report an error parsing a date string
#'
#' @param register_date The date string that could not be parsed.
#' @keywords internal

date_format_error <- function(register_date) {
  stringr::str_glue(stringr::str_c(
    "{register_date} is not a valid register date or date string: check ",
    "register date of interest and use format YYYY-MM-DD within a pair of ",
    "single qoute or double qoutes"))
}

#' Report an error fetching cached appgs
#'
#' @keywords internal

appg_cache_error <- function() {
  stringr::str_c("The Register of All-Party Parliamentary Groups has not ",
    "been downloaded: use download_appgs()")
}

#' Report an error with pause
#'
#' @keywords internal

pause_number_error <- function(pause) {
  stringr::str_glue(stringr::str_c(
    "{pause} is not a valid non-negative numeric"))
}

#' Report error with save
#'
#' @keywords internal

save_cache_error <- function(save) {
  stringr::str_glue(stringr::str_c(
    "{save} is not a valid boolean"))
}
