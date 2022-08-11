#' Inserts event that are marked as idle
#'
#' Takes the event data from a single participation and returns the data including
#' additional events that are marked as idle when the participant was not active
#' for more than `idle_time` seconds.
#'
#' @param workflow A list including the event data
#' @param idle_time Numeric describing after how many seconds an event is considered as idle.
#'
#' @return An event list including additional idle events
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' workflow <- get_workflow(json_data)
#' workflow_with_idle_events <- insert_idle_events(workflow)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
insert_idle_events <- function (workflow, idle_time=5) {

  idle_events <- workflow %>%
    # helper variable to chek if the event_code is the same as the previous one
    dplyr::filter(event_duration>idle_time) %>%
    dplyr::mutate(project_time=project_time+idle_time) %>%
    dplyr::mutate(event_code=paste0(substr(event_code, 1,6),"_IDL")) %>%
    dplyr::mutate(label=paste0("Idled ", label))

  result_workflow <- rbind(workflow, idle_events) %>%
    # Sort enlarged event list according to time
    arrange(time) %>%
    # Calculation of the corrected event durations after inserting the idle events
    dplyr::mutate(event_duration=dplyr::lead(project_time)-project_time)


  return(result_workflow)
}

