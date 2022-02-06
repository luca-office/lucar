#' Getting the General Participation Information
#'
#' Takes the log data from a single participation and returns a dataframe including a single line with general information of the participation.
#'
#' @param jsondata The log data for a single participation in form of a json object
#'
#' @return A dataframe (consisting one row) including general information for a single participation
#'
#' @examples
#'
#' @export
get_participation_data <- function (json_data){

    # Initialization of the dataframe row for the general answer data of the participation with the participant ID
    participation <- data.frame(id = json_data$surveyInvitation$id)

    # extract basic project and survey info
    participation$project <- json_data$project$title
    participation$survey <- json_data$survey$title

    # extract basic participation info
    participation$token <- json_data$surveyInvitation$token
    participation$account_participation <- json_data$surveyInvitation$isUserAccountParticipation
    participation$open_participation <- json_data$surveyInvitation$isOpenParticipation
    participation$project_start <- getTime(json_data$surveyInvitation$firstSurveyEventTimestamp)
    participation$project_end <- getTime(json_data$surveyInvitation$lastSurveyEventTimestamp)
    if (length(json_data$surveyEvents)>0) {
      participation$salutation <- json_data$surveyEvents[[1]]$data$salutation
      participation$first_name <- json_data$surveyEvents[[1]]$data$firstName
      participation$last_name <- json_data$surveyEvents[[1]]$data$lastName
    }


  return(participation)

}


# Formatting time data
getTime <- function(time, tzone="CET"){
  return(lubridate::with_tz(lubridate::ymd_hms(time), tzone))
}

