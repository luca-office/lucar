#' Prepares JSON files from a LUCA Office Administration into a List with Several Dataframes
#'
#' JSON files exported from the LUCA office simulation into a dedicated folder
#' are prepared and returned as a list including several result dataframes.
#' Best is to download all zip files into a dedicated folder and unpack them in that same folder.
#' Then provide this folder's path to the function.
#'
#' @param path The path to the folder including all JSON files (files in subfolders are also considered).
#' @param aggregate_events If TRUE, the events with identical task codes and directly following each other will be collated to a single events
#' @param idle_time If not FALSE, it provides the time in seconds when an event is marked as idle - i.e. the participant is not doing anything.
#' @param unzip If true, the function looks for zip archives located in the given path, corresponding to the naming convention for exported data from LUCA Office, and unzips these.
#' @param event_codes Dataframe with the workflow coding that is used to structure the log data
#' @param tool_codes Dataframe with the tool coding that is used to assign each used tool to a common code
#' @param debug_mode If TRUE the results include the internal hash IDs for the project elements are included and a tibble including unknown event types (if there were any). If 'module_specific' is set to TRUE it will be enforced to 'FALSE'.
#'
#' @return A dataframe including the prepared data from all JSON files
#'
#' @examples
#'
#' # Searches in the current working directory and all subdirectories for log data from LUCA office
#' # and prepares the data in a structure suitable for further analyses
#' \dontrun{
#' logdata <- prepare_data()
#' }
#'
#' @importFrom rjson fromJSON
#' @importFrom dplyr tibble
#' @export
prepare_data <- function (path = "./", aggregate_events=FALSE, idle_time=20, unzip = FALSE, event_codes=lucar::event_codes,
                             tool_codes=lucar::tool_codes, debug_mode=FALSE){

  # Setting 'module_specific' workflow preparation to FALSE for debugging mode and TRUE otherwise
  if (debug_mode) {
    module_specific <- FALSE
  } else {
    module_specific <- TRUE
  }

  # unzip files if indicated by function argument
  if (unzip){
    zip_files <- grep("^Luca-Erhebungsdaten-.*\\.zip$", list.files(path), value=TRUE)
    for (zip_file in zip_files){
      unzip(file.path(path, zip_file))
    }
  }

  # Get all JSON files located in the given path (including all subfolders)
  json_files <- grep("\\.json$", list.files(path, full.names=TRUE, recursive=TRUE), value=TRUE)

  # Initialization of objects for the result object
  participation_data <- dplyr::tibble(project=character())
  event_list <- list()
  unknown_events <- dplyr::tibble()
  mail_recipient_codes <- tibble(recipient=character(), code=character())


  # import first JSON to read the basic project data only from this element
  json_data <- rjson::fromJSON(file=file.path(json_files[[1]]))
  # get project modules overview and the corresponding hash IDs
  project_modules <- get_project_modules(json_data, hash_ids=TRUE)
  # get scenario elements overview and their respective event codes
  scenario_elements <- get_scenario_elements(json_data, hash_ids=TRUE)
  # get questionnaire elements overview and the corresponding hash IDs
  questionnaire_elements <- get_questionnaire_elements(json_data, hash_ids=TRUE)
  # get rater overview and the corresponding hash IDs
  rater <- get_rater(json_data, hash_ids=TRUE)



  # Looping through all JSON files identified in the given path
  for (json_file in json_files){

    #TODO Implement Try/Catch for reading the json and checking the format


    # Import JSON file including the data from a single participation
    if (debug_mode) {
      print(json_file)
    }
    json_data <- rjson::fromJSON(file= file.path(json_file))


    # add new list element with the event data, naming it with the ID of the participation
    element_name <- sub('\\..*$', '', basename(json_file))
    event_list[[element_name]] <- get_event_list(json_data, project_modules, scenario_elements, questionnaire_elements, module_specific=module_specific, idle_time=idle_time, mail_recipient_codes=mail_recipient_codes, event_codes, tool_codes, debug_mode=debug_mode)
    # Assigned mail recipient codes are stored in an extra variable and removed from the participant's event list
    mail_recipient_codes <- event_list[[element_name]]$mail_recipient_codes
    event_list[[element_name]]$mail_recipient_codes <- NULL

    # summarize the workflow data if indicated by the corresponding argument
    if (aggregate_events) {
      event_list[[element_name]] <- aggregate_events(event_list[[element_name]])
    }


    # get tibble row including summarized logdata information for the current participant
    logdata_summary <- get_logdata_summary(json_data, event_list[[element_name]], debug_mode)
    # get tibble row including the questionnaire scores for the current participant
    questionnaire_data <- get_questionnaire_data(json_data, project_modules, scenario_elements, questionnaire_elements, rater, debug_mode=debug_mode)

    # add summary results from above to the results from the previous participants
    participation_data <- participation_data %>%
      dplyr::full_join(logdata_summary, by=intersect(names(.), names(logdata_summary))) %>%
      dplyr::full_join(questionnaire_data, by=intersect(names(.), names(questionnaire_data)))


    # in debugging mode: collect incomplete event_codes, e.g. due to unknown events or unmatched id's
    if (debug_mode) {
      incomplete_codes <- nchar(event_list[[element_name]]$event_code)!=10
      unknown_events <- rbind(unknown_events, event_list[[element_name]][incomplete_codes,])
      unknown_events <- unknown_events[!duplicated(unknown_events$event_type),]
    }
  }

  if (is.null(logdata_summary)){
    stop("Neither the folder nor the subfolders of the given path include a JSON file!")
  }

  # add dataframe including unknown events if indicated
  prepared_logdata <- list(participation_data=participation_data,
                           project_modules=get_project_modules(json_data, debug_mode),
                           scenario_elements=get_scenario_elements(json_data, debug_mode),
                           questionnaire_elements=get_questionnaire_elements(json_data, debug_mode),
                           rater=get_rater(json_data, debug_mode),
                           mail_recipients=mail_recipient_codes)
  if (debug_mode) {
    prepared_logdata[["unknown_events"]] <- unknown_events
  }

  return(prepared_logdata)

}


#' Helper function to format timing data
#'
#' @param time string including the time information
#' @param tzone string defining the time zone that will be use to format the time
#'
#' @importFrom lubridate with_tz
#' @importFrom lubridate ymd_hms
getTime <- function(time, tzone="CET"){
  return(with_tz(ymd_hms(time), tzone))
}

