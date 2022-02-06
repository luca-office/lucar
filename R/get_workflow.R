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

  # formatting the events into a table
  events <- json_data$surveyEvents
  event_table <- data.frame(matrix(unlist(events, recursive=FALSE), nrow=length(events), byrow=TRUE))
  names(event_table) <- names(events[[1]])

  # filtering only relevant scenario event types
  scenario_events <- event_table %>%
    filter(!eventType %in% c("StoreParticipantData", "StartProject", "EndProject", "StartSurveyCommand", "EndSurveyCommand"))

  # retreiving the scenarion data, which includes the table with workflow codes
  scenario_data <- get_scenario_data(json_data)

  for (i in 1:nrow(scenario_events)) {
    event <- scenario_events[i,]
    workflow_event <- data.frame(id = event$invitationId[[1]])
    workflow_event$survey_id <- event$surveyId[[1]]
    workflow_event$invitation_token <- event$invitationToken[[1]]
    workflow_event$scenario_id <- event$data[[1]]$scenarioId
    workflow_event$event_type <- event$eventType[[1]]
    workflow_event$wf_code <- get_wf_code(event, scenario_data)
    workflow_event$time <- lubridate::as_datetime(event$timestamp[[1]])
    workflow_event$scenario_time <- as.difftime("")
    workflow_event$event_duration <- as.difftime("")


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

  #n_senarios <- length(json_data$project$projectModules)
  #for (i in 1:n_scenario) {
  #  scenarioId <- json_data$project$projectModules[[i]]$id
  #}
}
