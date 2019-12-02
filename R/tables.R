# Functions for extracting the table elements from appg pages

#' Get all appg tables
#'
#' @keywords internal

get_appg_tables <- function(appg_full_url, progress_bar, pause) {
  progress_bar$tick()$print()
  Sys.sleep(pause)
  appg_tables <- xml2::read_html(appg_full_url) %>%
    rvest::html_nodes(xpath = '//*[@id="mainTextBlock"]/table') %>%
    rvest::html_table()
}


#' Get title table
#'
#' @keywords internal

get_title_table <- function(table) {
  title_table <- tidyr::spread(table[[1]], key = X1, value = X2) %>%
    dplyr::rename(category = Category, purpose = Purpose, title = Title)
  title_table$category <- stringr::str_remove_all(title_table$category, pattern = "Group")
  title_table$category <- stringr::str_trim(title_table$category, side = "both")
  title_table <- tibble::as_tibble(title_table)
}

#' Get officer table
#'
#' @keywords internal

get_officer_table <- function(table) {
  officer_table <- table[[2]][-1,]
  officer_table <- dplyr::rename(
    officer_table,
    officer_role = X1,
    officer_name = X2,
    officer_party = X3)
  officer_table <- officer_table[-1,]
  officer_table <- tibble::as_tibble(officer_table)
}

#' Get agm table
#'
#' @keywords internal

get_agm_table <- function(table) {
  agm_table <- table[[4]][-1,]
  agm_table <- tidyr::spread(agm_table, key = X1, value = X2) %>%
    dplyr::rename(
      latest_agm = 1,
      statement = 2,
      reporting_deadline = 3,
      reporting_year = 4)
  agm_table <- tibble::as_tibble(agm_table)
}

#' Get financial table
#'
#' @keywords internal

get_financial_table <- function(table) {
  if (any(table[[5]] == "Financial Benefits")) {
    financial_table <- table[[5]][-1:-2,]
    financial_table <- dplyr::rename(
      financial_table,
      financial_source = X1,
      financial_value = X2,
      financial_received = X3,
      financial_registered = X4)
    financial_table <- financial_table[-1,]
    financial_table <- tibble::as_tibble(financial_table)
  } else {
    financial_table <- tibble::tibble(
      financial_source = NA,
      financial_value = NA,
      financial_received = NA,
      financial_registered = NA)
    financial_table <- financial_table[-1,]
  }
}

#' Get benefits table
#'
#' @keywords internal

get_benefits_table <- function(table) {
  if (length(table) == 6) {
    benefits_table <- table[[6]][-1:-2,]
    benefits_table <- dplyr::rename(
      benefits_table,
      benefit_source = X1,
      benefit_description = X2,
      benefit_value = X3,
      benefit_received = X4,
      benefit_registered = X5)
    benefits_table <- tibble::as_tibble(benefits_table)
  } else if (any(table[[5]] == "Benefits In Kind")) {
    benefits_table <- table[[5]][-1:-2,]
    benefits_table <- dplyr::rename(
      benefits_table,
      benefit_source = X1,
      benefit_description = X2,
      benefit_value = X3,
      benefit_received = X4,
      benefit_registered = X5)
    benefits_table <- tibble::as_tibble(benefits_table)
  } else {
    benefits_table <- tibble::tibble(
      benefit_source = NA,
      benefit_description = NA,
      benefit_value = NA,
      benefit_received = NA,
      benefit_registered = NA)
  }
}
