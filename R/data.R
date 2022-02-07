#' Basic workflow codes.
#'
#' A tibble providing the basic workflow codes and descriptions for the different event types in the LUCA Office log data.
#'
#' @format A tibble with XX rows and YY variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source Constructed manually based on the unique event types provided in the log data of LUCA Office.
"basic_wf_codes"


# helper function to construct the tibble including the basic workflow codes
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
write_unique_events <- function (event_table, file="unique_events.csv") {

  # Construction of a tibble including each existing event type and providing
  # the basic structure for the tibble including the basic workflow codes
  unique_events <- event_table %>%
    filter(!duplicated(eventType)) %>%
    select(eventType, data, index) %>%
    mutate(across(c(eventType, index), ~sapply(.x, function(x) x[[1]]))) %>%
    mutate(across(c(data), ~sapply(.x,  rjson::toJSON))) %>%
    mutate(label="", wf_code="", .after=eventType)

  # Save tibble as csv for manual editing
  readr::write_csv2(unique_events, file=file)

  return(unique_events)
}


# helper function to import and add edited table including the basic workflow
# codes to the package project
read_unique_events <- function (file="basic_wf_codes.csv") {
  basic_wf_codes <-  readr::read_csv2(file)
  usethis::use_data(basic_wf_codes)
  return (readr::read_csv2(file))
}
