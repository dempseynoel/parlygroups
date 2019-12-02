# Functions for getting appg urls and links

#' Get appg contents url
#'
#' @keywords internal

get_appg_contents_url <- function(register_date) {
  register_date <- stringr::str_sub(register_date, 3) %>%
    stringr::str_remove_all(pattern = "-")
  appg_contents_url <- stringr::str_glue(
    "{CONTENTS_BASE_URL}{register_date}/contents.htm")
}

#' Get appg href tags
#'
#' @keywords internal

get_appg_href_tags <- function(register_date) {
  appg_contents_url <- get_appg_contents_url(register_date)
  xml2::read_html(appg_contents_url) %>%
    rvest::html_nodes(xpath = '//*[@id="mainTextBlock"]/ul/li/a/@href') %>%
    rvest::html_text() %>%
    tibble::tibble(appg_hrefs = .)
}

#' Get appg urls
#'
#' @keywords internal

get_appg_urls <- function(register_date) {
  appg_base_url <- stringr::str_remove(
    get_appg_contents_url(register_date), "contents.htm")
  appg_full_url <- get_appg_href_tags(register_date)
  appg_full_url$appg_hrefs <- stringr::str_glue(
    "{appg_base_url}{appg_full_url$appg_hrefs}")
  appg_full_url
}
