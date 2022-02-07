#' Reads all JSON files into a single dataframe
#'
#' JSON files exported from the LUCA office simulation into a dedicated folder
#' are read into a datafrmame.
#' Best is to dowload all zip files into a dedicated folder and unpack them in that same folder.
#' Then provide this folder's path to the function.
#'
#' @param path The path to the folder including all JSON files (files in subfolders are also considered).
#'
#' @param unzip If true, the function looks for zip archives located in the given path, corresponding to the naming convention for exported data from LUCA Office, and unzips these.
#'
#' @return A dataframe including the prepared data from all JSON files
#'
#' @examples
#'
#' @export
read_json <- function (path = "./", unzip = FALSE){

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
  workflow <- list()

  # Looping through all JSON files identified in the given path
  for (json_file in json_files){

    # Import JSON file including the data from a single participation
    json_data <- rjson::fromJSON(file= file.path(json_file))


    # add new dataframe row for the given participation
    new_participation <- get_participation_data(json_data)
    if (is.null(participation)){
      participation <- new_participation
    } else {
      participation <- rbind(participation, new_participation)
    }

    # add new list element with the workflow data from the given participation
    new_workflow <- get_workflow(json_data)
    workflow <- append(workflow, list(new_workflow))

  }

  if (is.null(participation)){
    warning("Neither the folder nor the subfolders of the given path include a JSON file!")
  }

  return(list(participation=participation, workflow=workflow))

}


# Formatting time data
getTime <- function(time, tzone="CET"){
  return(lubridate::with_tz(lubridate::ymd_hms(time), tzone))
}

