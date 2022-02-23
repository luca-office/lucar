#' Reads all JSON files into a single dataframe
#'
#' JSON files exported from the LUCA office simulation into a dedicated folder
#' are read into a datafrmame.
#' Best is to dowload all zip files into a dedicated folder and unpack them in that same folder.
#' Then provide this folder's path to the function.
#'
#' @param path The path to the folder including all JSON files (files in subfolders are also considered).
#' @param unzip If true, the function looks for zip archives located in the given path, corresponding to the naming convention for exported data from LUCA Office, and unzips these.
#' @param scenario_specific If TRUE the workflow is split into separate lists for each scenario element
#' @param workflow_codes Dataframe with the workflow coding that is used to structure the log data
#' @param tool_codes Dataframe with the tool coding that is used to assign each used tool to a common code
#' @param debug_mode If TRUE the results include the internal hash IDs for the project elements are included and a tibble including unknown event types (if there were any). If 'scenario_specific' is set to TRUE it will be enforced to 'FALSE'.
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
prepare_logdata <- function (path = "./", unzip = FALSE, scenario_specific=TRUE,
                             workflow_codes=lucar::workflow_coding, tool_codes=lucar::tool_coding,
                             debug_mode=FALSE){

  # Forcing 'scenario_specific' to TRUE for debigging mode
  if (debug_mode & scenario_specific) {
    scenario_specific <- FALSE
    warning("Argument 'scenario_specific' is enforced to FALSE since 'debug_mode' was set TRUE.")
  }

  # unzip files if necessary
  if (unzip){
    zip_files <- grep("^Luca-Erhebungsdaten-.*\\.zip$", list.files(path), value=TRUE)
    for (zip_file in zip_files){
      unzip(file.path(path, zip_file))
    }
  }

  # Get all JSON files located in the given path (including all subfolders)
  json_files <- grep("\\.json", list.files(path, full.names=TRUE, recursive=TRUE), value=TRUE)

  # Initialization of the objects for the prepared participation data
  participation <- NULL
  workflows <- list()
  unknown_events <- tibble()

  # Looping through all JSON files identified in the given path
  for (json_file in json_files){

    #TODO Implement Try/Catch for reading the json and checking the format

    # Import JSON file including the data from a single participation
    json_data <- rjson::fromJSON(file= file.path(json_file))


    # add new list element with the workflow data, naming it with the ID of the  participation
    element_name <- sub('\\..*$', '', basename(json_file))
    workflows[[element_name]] <- get_workflow(json_data, scenario_specific=scenario_specific, workflow_codes, tool_codes, hash_ids=debug_mode)

    # in debugging mode: collects incomplete wf_codes, e.g. due to unknown events or unmatched id's
    if (debug_mode) {
      incomplete_codes <- nchar(workflows[[element_name]]$wf_code)!=10
      unknown_events <- rbind(unknown_events, workflows[[element_name]][incomplete_codes,])
      unknown_events <- unknown_events[!duplicated(unknown_events$event_type),]
    }


    # add new dataframe row for the given participation
    new_participation <- get_participation_data(json_data, debug_mode)
    if (is.null(participation)){
      participation <- new_participation
    } else {
      participation <- rbind(participation, new_participation)
    }

  }

  if (is.null(participation)){
    warning("Neither the folder nor the subfolders of the given path include a JSON file!")
  }

  # add dataframe including unknown events if indicated
  prepared_logdata <- list(participation=participation, workflows=workflows, project_elements=get_project_elements(json_data, debug_mode), project_scenarios=get_project_scenarios(json_data, debug_mode))
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

