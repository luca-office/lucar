#' Getting the workflow data from a single participation
#'
#' Takes the log data from a single participation and returns a list describing the workflow.
#'
#' @param json_data The log data for a single participation in form of a nested list
#' @param workflow_codes Dataframe with the workflow coding that is used to structure the log data
#' @param tool_codes Dataframe with the tool coding that is used to assign each used tool to a common code
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
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom lubridate as_datetime
#' @importFrom dplyr filter
#' @importFrom plyr mapvalues
#' @export
get_workflow <- function (json_data, workflow_codes=workflow_coding, tool_codes=tool_coding) {

  # return empty list if no events were recorded
  if (length(json_data$surveyEvents)==0) {
  }

  # formatting the events into a tibble
  events <-
    # transforming nested list into a iibble
    dplyr::as_tibble(data.frame(matrix(unlist(json_data$surveyEvents, recursive=FALSE), nrow=length(json_data$surveyEvents), byrow=TRUE))) %>%
    # retrieving original column names
    dplyr::rename_with(~names(json_data$surveyEvents[[1]])) %>%
    # unnest columns provided in lists
    dplyr::mutate(across(c(timestamp, eventType, index), ~sapply(.x, function(x) x[[1]]))) %>%
    # rename column
    dplyr::mutate(event_type=eventType)

  # get all project elements and their respective workflow codes
  project_elements <- get_project_elements(json_data)

  workflow <-
    # match events with the basic_wf_codes
    dplyr::left_join(events, workflow_codes, by="event_type") %>%
    # exclude irrelevant variables
    dplyr::select(invitation_id=invitationId, survey_id=surveyId, timestamp, label, event_type, wf_code, data=data, index=index.x) %>%
    # unnest columns with ids
    dplyr::mutate(invitation_id=unlist(invitation_id), survey_id=unlist(survey_id)) %>%
    # format time variable
    dplyr::mutate(time=as_datetime(timestamp)) %>%

    dplyr::mutate(data2=data) %>%


    # unnest all variables included in the list variable data
    tidyr::unnest_wider(data) %>%

    # match event ids with the ids of the project elements
    dplyr::left_join(select(project_elements,-c("binaryFileId","spreadsheetId")), by="id") %>%
    dplyr::left_join(select(project_elements,-c("id","spreadsheetId")), by="binaryFileId") %>%
    dplyr::left_join(select(project_elements,-c("id","binaryFileId")), by="spreadsheetId") %>%

    # merge element codes from the joins above and replace NAs by empty string
    dplyr::mutate(element_code=dplyr::coalesce(element_code, element_code.x, element_code.y)) %>%
    dplyr::mutate(element_code=replace(element_code, is.na(element_code), "")) %>%
    # join basic wf codes with individual project element code
    dplyr::mutate(wf_code=paste0(wf_code, element_code)) %>%

    # calculate variables for event times
    dplyr::mutate(scenario_time = time-time[1], event_duration = time-dplyr::lag(time)) %>%
    # select final set of variable
    #dplyr::select(invitation_id, survey_id, scenario_id=scenarioId, time, scenario_time, event_duration,
    #       label, event_type, wf_code, tool, name, usage_type) %>%

    # only for debugging purposes (replacing the above line)
    select(  binaryFileId, emailId, spreadsheetId, fileId, label, event_type, time,event_duration, wf_code, element_code, tool, name, usage_type, data2) %>%

    # exclude cases with a wf code of "#" (see basic table with wf codes)
    dplyr::filter(wf_code!="#") %>%

    # integrate tool id into the wf_codes were necessary
    dplyr::mutate(wf_code=replace(wf_code, grepl("^T##", wf_code),
                           paste0("T",
                                  plyr::mapvalues(tool, tool_codes$tool, tool_codes$code, warn_missing = FALSE)[grepl("^T##", wf_code)],
                                  substr(wf_code[grepl("^T##", wf_code)], 4, 10) )))

  return(workflow)
}
