# Functions for processing and cleaning the table elements from appg pages

#' Process groups table
#'
#' @keywords internal

process_appg_groups <- function(tables) {
  groups_table_list <- purrr::map(tables, function(table) {
    groups_table <- get_title_table(table)
    tibble::as_tibble(groups_table)
  })
  gro_table <- dplyr::bind_rows(groups_table_list)
  gro_table <- gro_table[, c(3, 2, 1)]
}

#' Process members table
#'
#' @keywords internal

process_appg_officer <- function(tables) {
  officer_table_list <- purrr::map(tables, function(table) {
    officer_table <- dplyr::bind_rows(
      get_title_table(table),
      get_officer_table(table)) %>%
        tidyr::fill(
          category,
          purpose,
          title)
    tibble::as_tibble(officer_table[-1,])
  })
  off_table <- dplyr::bind_rows(officer_table_list)
  off_table <- off_table[, c(3, 2, 1, 4, 5, 6)]
}

#' Process financial table
#'
#' @keywords internal

process_appg_financial <- function(tables) {
  financial_table_list <- purrr::map(tables, function(table) {
    financial_table <- dplyr::bind_rows(
      get_title_table(table),
      get_financial_table(table)) %>%
        tidyr::fill(
          category,
          purpose,
          title)
    tibble::as_tibble(financial_table[-1,])
  })
  fin_table <- dplyr::bind_rows(financial_table_list)
  fin_table$financial_value <- stringr::str_remove_all(fin_table$financial_value, ",")
  fin_table$financial_value <- as.numeric(fin_table$financial_value)
  fin_table$financial_received <- lubridate::dmy(fin_table$financial_received)
  fin_table$financial_registered <- lubridate::dmy(fin_table$financial_registered)
  fin_table <- fin_table[, c(3, 2, 1, 4, 5, 6, 7)]
}

#' Process benefits table
#'
#' @keywords internal

process_appg_benefits <- function(tables) {
  benefits_table_list <- purrr::map(tables, function(table) {
    benefits_table <- dplyr::bind_rows(
      get_title_table(table),
      get_benefits_table(table)) %>%
        tidyr::fill(
          category,
          purpose,
          title)
    tibble::as_tibble(benefits_table[-1,])
  })
  ben_table <- dplyr::bind_rows(benefits_table_list)
  ben_table <- tidyr::separate(
    ben_table,
    col = benefit_value,
    into = c("benefit_value_lower", "benefit_value_higher"),
    sep = "-")
  ben_table$benefit_description <- stringr::str_squish(ben_table$benefit_description)
  ben_table$benefit_description <- stringr::str_remove_all(ben_table$benefit_description, " :")
  ben_table$benefit_description <- stringr::str_remove_all(ben_table$benefit_description, "\n")
  ben_table$benefit_value_lower <- stringr::str_remove_all(ben_table$benefit_value_lower, ",")
  ben_table$benefit_value_higher <- stringr::str_remove_all(ben_table$benefit_value_higher, ",")
  ben_table$benefit_value_lower <- as.numeric(ben_table$benefit_value_lower)
  ben_table$benefit_value_higher <- as.numeric(ben_table$benefit_value_higher)
  ben_table$benefit_received <- lubridate::dmy(ben_table$benefit_received)
  ben_table$benefit_registered <- lubridate::dmy(ben_table$benefit_registered)
  ben_table <- tidyr::drop_na(ben_table)
  ben_table <- ben_table[, c(3, 2, 1, 4, 5, 6, 7, 8, 9)]
}

#' Process agm table
#'
#' @keywords internal

process_appg_agm <- function(tables) {
  agm_table_list <- purrr::map(tables, function(table) {
    agm_table <- dplyr::bind_rows(
      get_title_table(table),
      get_agm_table(table)) %>%
        tidyr::fill(
          category,
          purpose,
          title)
    tibble::as_tibble(agm_table[-1,])
  })
  agm_table <- dplyr::bind_rows(agm_table_list)
  agm_table$latest_agm <- lubridate::dmy(agm_table$latest_agm)
  agm_table$reporting_deadline <- lubridate::dmy(agm_table$reporting_deadline)
  agm_table <- agm_table[, c(3, 2, 1, 4, 5, 6, 7)]
}
