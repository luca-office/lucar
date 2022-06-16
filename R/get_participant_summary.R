#' Getting the General Participation Information
#'
#' Takes the log data and the already prepared workflow data from a single participant
#' and returns a dataframe including a single line with general information of the participation.
#'
#' @param json_data The log data for a single participation in form of a json object
#' @param workflow The prepared data from the workflow of the participant
#' @param debug_mode If TRUE the internal hash IDs for the project elements are included and no module specific data is returned.
#'
#' @return A dataframe (consisting of one row) including general information for a single participation
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' workflow <- get_workflow(json_data, module_specific=TRUE)
#' participation_data <- get_participation_data(json_data, workflow)
#' }
#'
#' @importFrom dplyr tibble
#' @export
get_participant_summary <- function (json_data, workflow, debug_mode=FALSE){

  # Initialization of the dataframe row for the general answer data of the participant with its ID
  participant_summary <- tibble(id = json_data$surveyInvitation$id) %>%

      # extract basic project and survey info
      mutate(project = json_data$project$title, survey = json_data$survey$title) %>%
      # extract basic participation info
      mutate(token = json_data$surveyInvitation$token,
             account_participation = json_data$surveyInvitation$isUserAccountParticipation,
             open_participation = json_data$surveyInvitation$isOpenParticipation,
             did_participate = as.logical(length(json_data$surveyEvents)>0),
             project_start = getTime(json_data$surveyInvitation$firstSurveyEventTimestamp),
             project_end = getTime(json_data$surveyInvitation$lastSurveyEventTimestamp)) %>%
      # Remove ID variable if indicated by Boolean 'hash_ids'
      dplyr::select_if(debug_mode|!grepl("^id$|Id$", names(.)))

  # Checking if invited participant actually participated
  # (to avoid errors by calling non existent variables)
  if (participant_summary$did_participate) {
    participant_summary <- participant_summary %>%
      mutate(salutation = json_data$surveyEvents[[1]]$data$salutation,
             first_name = json_data$surveyEvents[[1]]$data$firstName,
             last_name = json_data$surveyEvents[[1]]$data$lastName)
  }

  # If not debugging mode: Add summary information on the specific module workflows
  # (in debug mode there is no module specific info)
  if (!debug_mode) {
    for (module in names(workflow)) {
      participant_summary[[paste0("module_",module, "_total_duration")]] <- workflow[[module]]$module_time[length(workflow[[module]]$module_time)]
      participant_summary[[paste0("module_",module, "_events")]] <- list(workflow[[module]]$event_code)
      participant_summary[[paste0("module_",module, "_event_durations")]] <- list(workflow[[module]]$event_duration)
    }
  }

  return(participant_summary)

}


#' Formatting time data
#' @param time Time value in string format that is formatted
#' @param tzone Time zone to be used for the formatting of the time value
#'
getTime <- function(time, tzone="CET"){
  return(lubridate::with_tz(lubridate::ymd_hms(time), tzone))
}

