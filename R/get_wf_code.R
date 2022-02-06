#' Getting the workflow data from a single participation
#'
#' Takes the log data from a single participation and returns a list describing the workflow.
#'
#' @param event_typ The log data for a single participation in form of a nested list
#'
#' @param scenario_data The log data for a single participation in form of a nested list
#'
#' @return A list including the workflow data
#'
#' @examples
#'
#' @export
get_wf_code <- function (event, scenario_elements, basic_wf_codes=basic_wf_codes) {

  event

  inner_join(event, basic_wf_codes, by="eventType")

  hreturn(code)
}
