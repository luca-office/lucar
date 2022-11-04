#' Basic summary from the Logdata of a Participation
#'
#' Takes the log data and the already prepared workflow data from a single participant
#' and returns a single line dataframe with general information on the participation.
#'
#' @param json_data The log data for a single participation in form of a json object
#' @param event_list The prepared data from the workflow of the participant
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
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select_if
get_logdata_summary <- function (json_data, event_list, debug_mode=FALSE) {

  # Initialization of the dataframe row for the general answer data of the participant with its ID
  logdata_summary <- tibble::tibble(id = json_data$surveyInvitation$id) %>%

    # extract basic project and survey info
    dplyr::mutate(project = json_data$project$title, survey = json_data$survey$title) %>%
    # extract basic participation info
    dplyr::mutate(token = json_data$surveyInvitation$token,
           account_participation = json_data$surveyInvitation$isUserAccountParticipation,
           open_participation = json_data$surveyInvitation$isOpenParticipation,
           did_participate = FALSE) %>%
    # summary information for participants who have actually participated
    { if (length(event_list)!=0) {
        dplyr::mutate(., did_participate = as.logical(length(json_data$surveyEvents)>0),
                      project_start = getTime(json_data$surveyInvitation$firstSurveyEventTimestamp),
                      project_end = getTime(json_data$surveyInvitation$lastSurveyEventTimestamp))
      } else {.}
    } %>%
    # Remove ID variable if indicated by Boolean 'hash_ids'
    dplyr::select_if(debug_mode|!grepl("^id$|Id$", names(.)))

  # Checking if invited participant actually participated
  # (to avoid errors by calling non existent variables)
  if (logdata_summary$did_participate) {
    logdata_summary <- logdata_summary %>%
      mutate(salutation = json_data$surveyEvents[[1]]$data$salutation,
             first_name = json_data$surveyEvents[[1]]$data$firstName,
             last_name = json_data$surveyEvents[[1]]$data$lastName)
  }

  # If not debug mode: Add module specific summary information on the event lists
  # (i.e., in debug mode module specific info is not available)
  if (!debug_mode) {
    # list summary values first
    for (module in names(event_list)) {
      logdata_summary[[paste0("module_",module, "_start_time")]] <- as.numeric(event_list[[module]]$project_time[1])
      logdata_summary[[paste0("module_",module, "_total_duration")]] <- as.numeric(event_list[[module]]$module_time[length(event_list[[module]]$module_time)])
    }
    # actual event lists are provided at the end of the tibble
    for (module in names(event_list)) {
      logdata_summary[[paste0("module_",module, "_event_list")]] <- list(event_list[[module]])
    }
  }

  return(logdata_summary)

}


#' Formatting time data
#' @param time Time value in string format that is formatted
#' @param tzone Time zone to be used for the formatting of the time value
#'
getTime <- function(time, tzone="CET"){
  if (is.null(time)){
    print(time)
    return(as.POSIXct(NA))
  } else {
    #print(time)
    return(lubridate::with_tz(lubridate::ymd_hms(time), tzone))
  }
}

