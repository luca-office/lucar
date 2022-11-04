#' Standard workflow coding
#'
#' A tibble providing the basic workflow codes and descriptions for the different event types in the LUCA Office log data.
#'
#' Events with event_codes defined as '#' are ignored from the preparation of the workflow data
#'
#' @format A tibble with XX rows and YY variables:
#' \describe{
#'   \item{event_type}{all of the currently 44 different log event types}
#'   \item{label}{label for the event type}
#'   \item{event_code}{a code of 9 uppercase characters characterizing a given log event}
#'   \item{data_example}{supplement data for each log event, in json format}
#'   \item{index}{running number of the event as given in the original log table}
#' }
#' @source Constructed manually based on the unique event types provided in the log data of LUCA Office.
"event_codes"

#' Standard tool coding.
#'
#' A dataframe providing two digit codes for each tool available in LUCA office
#'
#' @format A tibble with XX rows and YY variables:
#' \describe{
#'   \item{tool}{name of the tool in the LUCA office log data}
#'   \item{code}{a two digit code that is assigned the given tool}
#'   \item{label}{a label for the tool}
#' }
#' @source Constructed manually based on the unique event types provided in the log data of LUCA Office.
"tool_codes"


#' Helper function to construct a table including the information for the workflow coding
#'
#' It provides all unique log events provided in the events table and
#' formats the output table in the form needed to define the basic
#' workflow table (see also function import_basic_coding).
#'
#' The function is only used during development of the package.
#'
#' @param events Dataframe including log events from LUCA office
#' @param file Name of the csv file for saving the results
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom utils write.csv2
write_unique_events <- function (events, file="dev/unique_events.csv") {

  unique_events <- events %>%
    filter(!duplicated(event_type)) %>%
    select(event_type, data_example=data2) %>%  ## data
    mutate(across(c(event_type), ~sapply(.x, function(x) x[[1]]))) %>%
    mutate(across(c(data_example), ~sapply(.x,  rjson::toJSON))) %>%
    mutate(label="", wf_code="", .after=event_type)

  # Save tibble as csv for manual editing
  write.csv2(unique_events, file=file)

  return(unique_events)
}
globalVariables(c("data_example"))


#' Helper function to import tables including the basic coding information and
#' add them to the package project.
#'
#' The function is only used during development of the package.
#'
#' @param file Name of the Excel file including the tables with the standard basic workflow codes, the standard tool codes and example table for defining project specific document types (e.g. considering their general content)
#'
#' @importFrom readxl read_excel
#' @importFrom usethis use_data
import_basic_coding <- function (file="dev/basic_codes.xlsx") {

  # Importing standard basic worklflow codes as data
  event_codes <- readxl::read_excel("dev/basic_codes.xlsx", sheet = "event_codes")
  usethis::use_data(event_codes, overwrite=TRUE)
  # Importing standard tool codes as data
  tool_codes <- readxl::read_excel("dev/basic_codes.xlsx", sheet = "tool_codes")
  usethis::use_data(tool_codes, overwrite=TRUE)

}
