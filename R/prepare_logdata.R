#' Reads all JSON files into a single dataframe
#'
#' JSON files exported from the LUCA office simulation into a dedicated folder
#' are read into a dataframe.
#' Best is to download all zip files into a dedicated folder and unpack them in that same folder.
#' Then provide this folder's path to the function.
#'
#' @param path The path to the folder including all JSON files (files in subfolders are also considered).
#' @param compress_events If TRUE, the events with identical task codes and directly following each other will be collated to a single events
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
#' logdata <- prepare_logdata()
#' }
#'
#' @importFrom rjson fromJSON
#' @importFrom dplyr tibble
#' @export
prepare_logdata <- function (path = "./", compress_events=FALSE, idle_time=20, unzip = FALSE, event_codes=lucar::event_codes,
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

  # Initialization of the objects for the table including the participants_summary
  participant_summary <- NULL
  workflows <- list()
  unknown_events <- dplyr::tibble()

  # Looping through all JSON files identified in the given path
  for (json_file in json_files){

    #TODO Implement Try/Catch for reading the json and checking the format


    # Import JSON file including the data from a single participation
    if (debug_mode) {
      print(json_file)
    }
    json_data <- rjson::fromJSON(file= file.path(json_file))


    # add new list element with the workflow data, naming it with the ID of the  participation
    element_name <- sub('\\..*$', '', basename(json_file))
    workflows[[element_name]] <- get_workflow(json_data, module_specific=module_specific, idle_time=idle_time, event_codes, tool_codes, debug_mode=debug_mode)


    # summarize the workflow data if indicated by the corresponding argument
    if (compress_events) {
      workflows[[element_name]] <- compress_events(workflows[[element_name]])
    }


    # construct new tibble row for the tibble including the summary data on all participants
    new_participant_summary <- get_participant_summary(json_data,  workflows[[element_name]], debug_mode)

    # add new participant data to the already existing one
    if (is.null(participant_summary)){
      participant_summary <- new_participant_summary
    } else {
      participant_summary <- dplyr::full_join(participant_summary, new_participant_summary, by=intersect(names(participant_summary), names(new_participant_summary)))
    }


    # in debugging mode: collect incomplete event_codes, e.g. due to unknown events or unmatched id's
    if (debug_mode) {
      incomplete_codes <- nchar(workflows[[element_name]]$event_code)!=10
      unknown_events <- rbind(unknown_events, workflows[[element_name]][incomplete_codes,])
      unknown_events <- unknown_events[!duplicated(unknown_events$event_type),]
    }
  }

  if (is.null(participant_summary)){
    stop("Neither the folder nor the subfolders of the given path include a JSON file!")
  }

  # add dataframe including unknown events if indicated
  prepared_logdata <- list(participation=participant_summary,
                           workflows=workflows,
                           project_elements=get_project_elements(json_data, debug_mode),
                           project_modules=get_project_modules(json_data, debug_mode))
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

