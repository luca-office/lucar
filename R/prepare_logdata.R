#' Reads all JSON files into a single dataframe
#'
#' JSON files exported from the LUCA office simulation into a dedicated folder
#' are read into a datafrmame.
#' Best is to dowload all zip files into a dedicated folder and unpack them in that same folder.
#' Then provide this folder's path to the function.
#'
#' @param path The path to the folder including all JSON files (files in subfolders are also considered).
#' @param unzip If true, the function looks for zip archives located in the given path, corresponding to the naming convention for exported data from LUCA Office, and unzips these.
#' @param workflow_codes Dataframe with the workflow coding that is used to structure the log data
#' @param tool_codes Dataframe with the tool coding that is used to assign each used tool to a common code
#' @param include_unknown_events If TRUE the result object includes a tibble including unknown event types (if there were any)
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
prepare_logdata <- function (path = "./", unzip = FALSE, workflow_codes=workflow_coding, tool_codes=tool_coding, include_unknown_events=FALSE){

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


    # add new dataframe row for the given participation
    new_participation <- get_participation_data(json_data)
    if (is.null(participation)){
      participation <- new_participation
    } else {
      participation <- rbind(participation, new_participation)
    }

    # add new list element with the workflow data, naming it with the ID of the  participation
    element_name <- sub('\\..*$', '', basename(json_file))
    workflows[[element_name]] <- get_workflow(json_data, workflow_codes, tool_codes)

    # collects incomplete wf_codes, e.g. due to unknown events or unmatched id's
    incomplete_codes <- nchar(workflows[[element_name]]$wf_code)!=10
    unknown_events <- rbind(unknown_events, workflows[[element_name]][incomplete_codes,])
    unknown_events <- unknown_events[!duplicated(unknown_events$event_type),]

  }

  if (is.null(participation)){
    warning("Neither the folder nor the subfolders of the given path include a JSON file!")
  }

  # add dataframe including unknown events if indicated
  prepared_logdata <- list(participation=participation, workflows=workflows)
  if (include_unknown_events) {
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

