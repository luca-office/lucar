#' Reads all JSON files into a single dataframe
#'
#' JSON files exported from the LUCA office simulation into a dedicated folder
#' are read into a datafrmame.
#' Best is to dowload all zip files into a dedicated folder and unpack them in that same folder.
#' Then provide this folder's path to the function.
#'
#' @param path The path to the folder including all JSON files (files in subfolders are also considered).
#'
#' @return A dataframe including the prepared data from all JSON files
#'
#' @examples
#'
#' @export
read_json <- function (path, unzip = FALSE){

  if (unzip){
    zip_files <- grep("^Luca-Erhebungsdaten-.*\\.zip$", list.files(path), value=TRUE)
    for (zip_file in zip_files){
      unzip(file.path(path, zip_file))
    }
  }

  # Get all JSON files located in the given path (including all subfolders)
  json_files <- grep("\\.json", list.files(path, full.names=TRUE, recursive=TRUE), value=TRUE)

  # Initialization of the object for the survey data
  survey_data <- NULL

  # Looping through all JSON files identified in the given path
  for (json_file in json_files){

    # Import JSON file (including the data from a single participant)
    json_data <- rjson::fromJSON(file= file.path(json_file))

    # extract basic participant info
    id <- json_data$surveyInvitation$id
    token <- json_data$surveyInvitation$token
    account_participation <- json_data$surveyInvitation$isUserAccountParticipation
    open_participation <- json_data$surveyInvitation$isOpenParticipation

    participant <- data.frame(id, token, project_start=NA, project_end=NA,
                              account_participation, open_participation,
                              salutation=NA, first_name=NA, last_name=NA)

    # Checking if participant has registered events/ started the project
    if (length(json_data$surveyEvents)>0){
      participant$project_start <- getTime(json_data$surveyInvitation$firstSurveyEventTimestamp)
      participant$project_end <- getTime(json_data$surveyInvitation$lastSurveyEventTimestamp)
      participant$salutation <- json_data$surveyEvents[[1]]$data$salutation
      participant$first_name <- json_data$surveyEvents[[1]]$data$firstName
      participant$last_name <- json_data$surveyEvents[[1]]$data$lastName

      for (event in json_data$surveyEvents){
        if (event$eventType=="StartScenario"){
          variable <- paste0(event$data$scenarioId,"_start")
          value <- getTime(event$timestamp)
          participant[[variable]] <- value
        }
        if (event$eventType=="AnswerEmail"){
          id_answer_mail <- event$data$createdEmailId
        }
        if (event$eventType=="UpdateEmailText"){
          if (event$data$id==id_answer_mail){
            variable <- paste0(event$data$scenarioId,"_mail_answer")
            value <- event$data$text
            participant[[variable]] <- value
          }
        }
        if (event$eventType=="EndScenario"){
          variable <- paste0(event$data$scenarioId,"_end")
          value <- getTime(event$timestamp)
          participant[[variable]] <- value
        }
      }
    }

    # add extracted data to the overall survey data
    if (is.null(survey_data)){
      survey_data <- participant
    }
    else{
      survey_data <- dplyr::full_join(survey_data, participant)
    }
  }

  if (is.null(survey_data)){
    warning("Neither the folder nor the subfolders of the given path include a JSON file!")
  }
  else {
    # selection of just the IDs of the project parts (scenarios, questionnaires or other elements)
    # they are assumed in survey_data be in the correct order of appearance (they should be)
    project_parts <- grep("........-....-....-....-............_", names(survey_data), value=TRUE)
    project_parts <- substr(1,36)
    project_parts <- project_parts[!duplicated(project_parts)]

    for (i in length(project_parts)){
      names(survey_data) <- gsub(project_parts[i], paste0("part",i), names(survey_data))
    }

  }

  return(survey_data)

}

# Formatting time data
getTime <- function(time, tzone="CET"){
  return(lubridate::with_tz(ymd_hms(time), tzone))
}

