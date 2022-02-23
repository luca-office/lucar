#' Getting the General Participation Information
#'
#' Takes the log data from a single participation and returns a dataframe including a single line with general information of the participation.
#'
#' @param json_data The log data for a single participation in form of a json object
#' @param hash_ids If TRUE the internal hash IDs for the projct elements are included
#'
#' @return A dataframe (consisting of one row) including general information for a single participation
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' participation_data <- get_participation_data(json_data)
#' }
#'
#' @importFrom dplyr tibble
#' @export
get_participation_data <- function (json_data, hash_ids=FALSE){

    # Initialization of the dataframe row for the general answer data of the participation with the participant ID
    participation <- tibble(id = json_data$surveyInvitation$id) %>%
      # extract basic project and survey info
      mutate(project = json_data$project$title, survey = json_data$survey$title) %>%
      # extract basic participation info
      mutate(token = json_data$surveyInvitation$token,
             account_participation = json_data$surveyInvitation$isUserAccountParticipation,
             open_participation = json_data$surveyInvitation$isOpenParticipation,
             did_participate = as.logical(length(json_data$surveyEvents)>0),
             project_start = getTime(json_data$surveyInvitation$firstSurveyEventTimestamp),
             project_end = getTime(json_data$surveyInvitation$lastSurveyEventTimestamp)) %>%
      # Remove ID variable if indicated by boolean 'hash_ids'
      dplyr::select_if(hash_ids|!grepl("^id$|Id$", names(.)))

    # checking if invited particpant actually participated
    if (participation$did_participate) {
      participation <- participation %>%
        mutate(salutation = json_data$surveyEvents[[1]]$data$salutation,
               first_name = json_data$surveyEvents[[1]]$data$firstName,
               last_name <- json_data$surveyEvents[[1]]$data$lastName)
    }

  return(participation)

}


#' Formatting time data
#' @param time Time value in string format that is formatted
#' @param tzone Time zone to be used for the formatting of the time value
#'
getTime <- function(time, tzone="CET"){
  return(lubridate::with_tz(lubridate::ymd_hms(time), tzone))
}

