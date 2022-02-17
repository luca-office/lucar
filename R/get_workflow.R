#' Getting the workflow data from a single participation
#'
#' Takes the log data from a single participation and returns a list describing the workflow.
#'
#' @param json_data The log data for a single participation in form of a nested list
#' @param workflow_coding Dataframe with the workflow coding that is used to structure the log data
#' @param tool_coding Dataframe with the tool coding that is used to assign each used tool to a common code
#'
#' @return A list including the workflow data
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' workflow <- get_workflow(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr rename_with
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom lubridate as_datetime
#' @importFrom dplyr filter
#' @export
get_workflow <- function (json_data, workflow_coding=workflow_coding, tool_coding=tool_coding) {

  # return empty list if no events were recorded
  if (length(json_data$surveyEvents)==0) {
  }

  # formatting the events into a tibble
  events <-
    # transforming nested list into a iibble
    as_tibble(data.frame(matrix(unlist(json_data$surveyEvents, recursive=FALSE), nrow=length(json_data$surveyEvents), byrow=TRUE))) %>%
    # retrieving original column names
    rename_with(~names(json_data$surveyEvents[[1]])) %>%
    # unnest columns provided in lists
    mutate(across(c(timestamp, eventType, index), ~sapply(.x, function(x) x[[1]]))) %>%
    # rename columns
    rename(event_type=eventType)

  # get all project elements and their respective workflow codes
  project_elements <- get_project_elements(json_data)

  workflow <-
    # match events with the basic_wf_codes
    left_join(events, workflow_coding, by="event_type") %>%
    # exclude irrelevant variables
    select(invitation_id=invitationId, survey_id=surveyId, timestamp, label, event_type, wf_code, data=data, index=index.x) %>%
    # unnest columns with ids
    mutate(invitation_id=unlist(invitation_id), survey_id=unlist(survey_id)) %>%
    # format time variable
    mutate(time=as_datetime(timestamp)) %>%
    # unnest all variables included in the list variable data
    unnest_wider(data) %>%
    # match table providing the ids of the project elements
    left_join(project_elements, by="id") %>%
    # match table email ids for events in which this one is provided as email_id and not as id
    left_join(project_elements, by=c("emailId"="id", "name", "usage_type")) %>%
    # replace NAs provided for project elements without individual id by empty string
    mutate(wf_code.y=replace(wf_code.y, is.na(wf_code.y), "")) %>%
    # join basic wf codes with individual project element id
    mutate(wf_code=paste0(wf_code.x, wf_code.y)) %>%
    # calculate variables for event times
    mutate(scenario_time = time-time[1], event_duration = time-dplyr::lag(time)) %>%
    # select final set of variable
    select(invitation_id, survey_id, scenario_id=scenarioId, time, scenario_time, event_duration,
           label, event_type, wf_code, tool, name, usage_type) %>%
    # exclude cases with a wf code of "#" (see basic table with wf codes)
    filter(wf_code!="#")

  return(workflow)
}
