# Functions for downloading all appg pages

# Cache -----------------------------------------------------------------------

cache <- new.env(parent = emptyenv())

#' Download Register of All-Party Parliamentary Groups
#'
#' \code{download_appg} scrapes and caches entries in the Register of All-Party
#' Parliamentary Groups. It is not necessary to assign when using download_appg(),
#' although if you do a list object of raw tables is returned. If the environment
#' is closed the cached download will be lost.
#'
#' @param register_date A character string of the date of the register of interest
#' written within a pair of single qoute or double qoutes. The date must be in
#' ISO 8601 format, i.e. ("YYYY-MM-DD").
#' @param pause A non-negative numeric indicating length of time in seconds to
#' pause between accessing APPG pages. Set to 1 by default. A longer pause helps
#' minimise burden on the Parliamentary website.
#' @param save A boolean which if TRUE saves the cached tables as a series of
#' .csv files. Additionally, a list object of the raw APPG download is saved as an
#' .RDS file. All files are saved in the current working environmnent. By default
#' save is FALSE.
#' @return NULL
#' @examples
#' download_appg("2019-11-05")
#' download_appg("2019-11-05", pause = 0.5)
#' download_appg("2019-11-05", pause = 0.5, save = TRUE)
#' data <- download_appg("2019-11-05")
#' @export

download_appg <- function(register_date, pause = 1, save = FALSE) {

  # Check for errors
  if (!is.character(register_date))
    stop(date_format_error(register_date))
  if (stringr::str_length(register_date) != 10)
    stop(date_format_error(register_date))
  if (!is.numeric(pause))
    stop(pause_number_error(pause))
  if (pause < 0)
    stop(pause_number_error(pause))
  if (!is.logical(save))
    stop(save_cache_error(save))

  # Get list of appg urls and check if empty/error
  appg_urls <- get_appg_urls(register_date)
  if (nrow(appg_urls) == 0) stop(date_format_error(register_date))

  # Show progress bar
  message("This may take a minute...")
  progress_bar <- dplyr::progress_estimated(nrow(appg_urls))

  # Create list of data frames
  tables <- purrr::map(appg_urls$appg_hrefs, get_appg_tables, progress_bar, pause)
  message("Nearly done! Just doing some tidying up...")

  # Assign to cache
  assign(CACHE_APPG_GROUPS, process_appg_groups(tables), envir = cache)
  assign(CACHE_APPG_OFFICERS, process_appg_officer(tables), envir = cache)
  assign(CACHE_APPG_FINANCIAL, process_appg_financial(tables), envir = cache)
  assign(CACHE_APPG_BENEFITS, process_appg_benefits(tables), envir = cache)
  assign(CACHE_APPG_AGM, process_appg_agm(tables), envir = cache)
  assign(CACHE_APPG_REGISTER, tables, envir = cache)

  # Save data if requested
  if (save == TRUE) {
    save_cached_tables(register_date)
  }
}

#' Get cached appg tables
#'
#' @keywords internal

get_cached_tables <- function(get = c("tables", "groups", "officers", "financial", "benefits", "agm")) {

  # Check for errors
  if (!exists(CACHE_APPG_REGISTER, envir = cache)) stop(appg_cache_error())

  # Retrieve desired cache
  if (get == "groups") {
    get(CACHE_APPG_GROUPS, envir = cache)
  } else if (get == "officers") {
    get(CACHE_APPG_OFFICERS, envir = cache)
  } else if (get == "financial") {
    get(CACHE_APPG_FINANCIAL, envir = cache)
  } else if (get == "benefits") {
    get(CACHE_APPG_BENEFITS, envir = cache)
  } else if (get == "agm") {
    get(CACHE_APPG_AGM, envir = cache)
  } else {
    get(CACHE_APPG_REGISTER, envir = cache)
  }
}

#' Save cached tables to working directory
#'
#' @keywords internal

save_cached_tables <- function(register_date) {

  # Replace - with _ in date
  register_date <- stringr::str_replace_all(register_date, "-", "_")

  # Groups
  readr::write_csv(
    get_cached_tables("groups"),
    stringr::str_glue("groups_{register_date}.csv"))

  # officers
  readr::write_csv(
    get_cached_tables("officers"),
    stringr::str_glue("officers_{register_date}.csv"))

  # Financial
  readr::write_csv(
    get_cached_tables("financial"),
    stringr::str_glue("financial_{register_date}.csv"))

  # Benefits
  readr::write_csv(
    get_cached_tables("benefits"),
    stringr::str_glue("benefits_{register_date}.csv"))

  # AGM
  readr::write_csv(
    get_cached_tables("agm"),
    stringr::str_glue("agm_{register_date}.csv"))

  # Save list object
  saveRDS(
    get_cached_tables("tables"),
    stringr::str_glue("raw_appg_{register_date}.RDS"))
}
