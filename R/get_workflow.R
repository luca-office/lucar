#' Getting the workflow data from a single participation
#'
#' Takes the log data from a single participation and returns a list describing the workflow.
#'
#' @param jsondata The log data for a single participation in form of a nested list
#'
#' @return A list including the workflow data
#'
#' @examples
#'
#' @export
get_workflow <- function (json_data) {

  # return empty list if no events were recorded
  if (length(json_data$surveyEvents)==0) {
  }

  # formatting the events into a tibble
  events <- as_tibble(data.frame(matrix(unlist(json_data$surveyEvents, recursive=FALSE), nrow=length(events), byrow=TRUE))) %>% # transforming nested list into a iibble
    rename_with(~names(events[[1]])) %>% # retrieving original column names
    mutate(across(c(timestamp, eventType, index), ~sapply(.x, function(x) x[[1]]))) # unnest columns provided in lists

  # get all project elements and their respective workflow codes
  project_elements <- get_project_elements(json_data)

  workflow <- left_join(events, basic_wf_codes, by="eventType") %>%
    select(invitation_id=invitationId, survey_id=surveyId, timestamp, label, eventType, wf_code, data=data.x, index=index.x) %>%
    mutate(time=lubridate::as_datetime(timestamp[[1]]) )%>%
    tidyr::unnest_wider(data) %>%
    left_join(project_elements, by="id") %>%
    mutate(wf_code.y=replace(wf_code.y, is.na(wf_code.y), "")) %>%
    mutate(wf_code=paste0(wf_code.x, wf_code.y)) %>%
    mutate(scenario_time = as.difftime("")) %>%
    mutate(event_duration = as.difftime("")) %>%
    select(invitation_id, surveyId, scenario_id=scenarioId, time, scenario_time, event_duration, label, event_type=eventType, wf_code, tool, name, usage_type)

  for (i in 1:nrow(workflow)) {
    event <- workflow[i,]

    if (is.null(workflow)) {
      # in case this event is the first in the considered workflow, set some further initial values and use current workflow_event to initialize the workflow table
      workflow_event$scenario_time <- as.difftime("0:0:0")
      workflow <- workflow_event
    } else {
      # calculating the duration of the event before in the workflow (currently the last one)
      event_before <- nrow(workflow)
      workflow[event_before,]$event_duration <- workflow_event$time - workflow[event_before,]$time
      # calculating the time since the start of the scenarion
      workflow_event$scenario_time <- workflow_event$time - workflow[1,]$time
      # adding event data to already existing events in the workflow
      workflow <- rbind(workflow, workflow_event)
    }

  }

  return(workflow)
}
