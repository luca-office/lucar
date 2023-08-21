#' Prepares Survey Data from LUCA Office for secondary Analyses
#'
#' The survey data collected via LUCA Office can be downloaded in form of a zip
#' archive that includes JSON data for each participation. `prepare_lsd()`
#' prepares the nested JSON data from the participations in a summarized form
#' for secondary analyses.
#'
#' Besides a table including the relevant information for each participation,
#' the function additionally returns different tables with overviews on the
#' elements used in the scenarios, the form of included questionnaires, and
#' the raters responsible for the scoring.
#'
#'
#' @param path Path to the folder including zip archives downloaded from LUCA
#'   Office (already unzipped JSON files as well as archives or files in
#'   subfolders are also considered).
#' @param aggregate_duplicate_events For `TRUE`, events with identical codes
#'   directly following each other will be collated to a single event.
#' @param idle_time For values larger than 0, events will be marked as idle,
#'   if the event following this one happened more than `ìdle_time` seconds
#'   later. This might be relevant for analyses considering times when
#'   participants were actually not doing anything.
#' @param unzip For `TRUE`, zip archives located in the given path and
#'   following the naming structure from LUCA Office will be unzipped; for
#'   `FALSE`, only JSON files from previously unzipped archives will be
#'   considered.
#' @param event_codes Table with event codes that are used to identify and
#'   differentiate events. The package provides a standard encoding of the
#'   events, however, for very specific analyses it can be adapted as needed.
#' @param tool_codes Table with codes for the different available tools. These
#'   are used similar as the event codes to identify and differentiate events.
#'   The package also provides a standard encoding of the events. For very
#'   specific analyses it can also be adapted as needed.
#' @param debug_mode For `TRUE` the returned tables include internal hash IDs
#'   for the different project elements (to check on possible matching errors).
#'   Further, the event list is not separated according to the different
#'   modules but only a single event list for the complete project, and an
#'   additional table including possibly unrecognized events (missing in the
#'   `event_codes` or `tool_codes`) is returned.
#'
#' @return List including different tables (tibbles) with participation data
#'   and documentation on the administered project.
#'
#' @examples
#'
#' \dontrun{
#' # Prepare survey data given in the current directory
#' survey_data <- prepare_lsd()
#' }
#'
#' @importFrom rjson fromJSON
#' @importFrom dplyr tibble
#' @export
prepare_lsd <- function (path = "./", aggregate_duplicate_events=FALSE, idle_time=10,
                         unzip = TRUE, event_codes=lucar::event_codes,
                         tool_codes=lucar::tool_codes, debug_mode=FALSE) {

  cat("\n")

  # TODO; Check if path leads to a file the prepare that file.exists

  # unzip files if indicated by function argument
  if (unzip){
    # get all zip files that correspond to the naming convention of a LUCA survey data archive
    zip_files <- grep("^Luca-Erhebungsdaten-.*\\.zip$", list.files(path, recursive = TRUE), value=TRUE)
    # temp folder will always be generated, so the check on json files below does not leed to an error
    path_temp <- tempdir()
    if (length(zip_files>0)){
      cat(length(zip_files), "zip archives with survey data found.\n")
      cat("-> Unzipping archives ")
      for (zip_file in zip_files){
        cat(".")
        # extract all zip archives to a "temp" folder in the given path (will be created)
        unzip(file.path(path, zip_file), exdir = path_temp)
      }
      cat(" done!\n\n")
    }
  }

  # Get all JSON files located in the given path (including all subfolders)
  json_files <- c(grep("\\.json$", list.files(path, full.names=TRUE, recursive=TRUE), value=TRUE),
                  grep("\\.json$", list.files(path_temp, full.names=TRUE, recursive=TRUE), value=TRUE))
  # if no folder was provided but the name of an archive, length will be zero, and
  # it will be checked on a json or zip file archive
  if (length(json_files)==0) {
    if (file.exists(path)){
      if (grepl("\\.json$", path)){
        json_files <- path
      } else if (grep("Luca-Erhebungsdaten-.*\\.zip$", path)) {
        cat("1 zip archive with survey data found.\n")
        cat("-> Unzipping archive .")
        unzip(path, exdir = path_temp)
        cat(" done!\n\n")
        json_files <- grep("\\.json$", list.files(path_temp, full.names=TRUE, recursive=TRUE), value=TRUE)
      }
    } else {
      cat("No json files with participation data or zip archives starting with 'Luca-Erhebungsdaten-' found.\n")
      return(dplyr::tibble())
    }
  }
  cat(length(json_files), "files with participation data found.\n")


  # Initialization of objects for the result object
  participation_data <- dplyr::tibble(project=character())
  event_lists <- list()
  unknown_events <- dplyr::tibble()


  # import first JSON to read the basic project data only from this element
  json_data <- rjson::fromJSON(file=file.path(json_files[[1]]))
  # get project modules overview and the corresponding hash IDs
  project_modules <- get_project_modules(json_data)
  # get scenario elements overview and their respective event codes
  scenario_elements <- get_scenario_elements(json_data, project_modules)
  # get questionnaire elements overview and the corresponding hash IDs
  questionnaire_elements <- get_questionnaire_elements(json_data)
  # get rater overview and the corresponding hash IDs
  rater <- get_rater(json_data)

  # console log for start of the data processing
  cat("-> Preparing data ")

  # Looping through all JSON files identified in the given path
  for (json_file in json_files){

    cat(".")
    #TODO Implement Try/Catch for reading the json and checking the format

    # Import JSON file including the data from a single participation
    if (debug_mode) {
      print(json_file)
    }
    json_data <- rjson::fromJSON(file= file.path(json_file))


    # add new list element with the event data, naming it with the ID of the participation
    #element_name <- sub('\\..*$', '', basename(json_file))
    event_lists <- get_event_list(json_data, project_modules, scenario_elements, questionnaire_elements,
                                                 aggregate_duplicate_events=aggregate_duplicate_events,
                                                 idle_time=idle_time, event_codes, tool_codes, debug_mode=debug_mode)
    # Assigned mail recipient codes are stored in an extra variable and removed from the participant's event list
    scenario_elements <- event_lists$scenario_elements
    event_lists$scenario_elements <- NULL


    # get tibble row including summarized logdata information for the current participant
    new_participation_data <- get_logdata_summary(json_data, event_lists, scenario_elements, debug_mode)
    # add tibble row including the questionnaire scores if they exist
    questionnaire_data <- get_questionnaire_data(json_data, project_modules, scenario_elements,
                                                 questionnaire_elements, rater, debug_mode=debug_mode)
    if (nrow(questionnaire_data)>0){
      new_participation_data <- cbind(new_participation_data, questionnaire_data)
    }

    # add summary results from above to the results from the previous participants
    participation_data <- participation_data %>%
      dplyr::full_join(new_participation_data, by=intersect(names(.), names(new_participation_data)))


    # in debugging mode: collect incomplete event_codes, e.g. due to unknown events or unmatched id's
    if (debug_mode) {
      incomplete_codes <- nchar(event_lists$event_code)!=10
      unknown_events <- rbind(unknown_events, event_lists[incomplete_codes,])
      unknown_events <- unknown_events[!duplicated(unknown_events$event_type),]
    }
  }

  cat(" done!\n\n")

  # if not in debug mode remove hash_ids from the reference tables
  project_modules <- project_modules %>%
    dplyr::select_if(debug_mode|!grepl("^id$|_id$", names(.)))
  scenario_elements <- scenario_elements %>%
    dplyr::select_if(debug_mode|!grepl("^id$|_id$", names(.))) %>%
    dplyr::filter(!(usage_type=="UserCreatedEmail" & relevance=="Required")) # remove redundant answer mail elements
  questionnaire_elements <- questionnaire_elements %>%
    dplyr::select_if(debug_mode|!grepl("_no$|_id$", names(.)))
  rater <- rater %>%
    dplyr::select_if(debug_mode|!grepl("_id$", names(.)))

  # preparation of the answer object
  survey_data <- list(participation_data=participation_data,
                      project_modules=project_modules,
                      scenario_elements=scenario_elements,
                      questionnaire_elements=questionnaire_elements,
                      rater=rater)

  # add table including unknown events if indicated
  if (debug_mode) {
    prepared_logdata[["unknown_events"]] <- unknown_events
  }

  # delete temp folder with unzipped survey data
  if (!is.null(path_temp)) {
    unlink(path_temp, recursive=TRUE)
  }


  # console output to provide basic information on the prepared data
  projects <- survey_data$participation_data %>%
    dplyr::distinct(project) %>%
    dplyr::pull(project)
  cat ("\nPrepared data is from the following project:", paste(projects, collapse=", "), "\n")
  if (length(projects)>1) {
    warning("The data is from more than one project. This might lead to errors in the data preparation!\n  Only use this data if you are sure that the modules and structure of the projects are equivalent.")
  }
  surveys <- survey_data$participation_data %>%
    dplyr::distinct(survey)
  cat("Number of included surveys:       ", nrow(surveys), "\n")
  cat("Number of included participations:", nrow(survey_data$participation_data), "\n\n")

  return(survey_data)

}
globalVariables(c("project", "survey"))




###############################################################################
### Helper Functions


#' Format timing data
#'
#' @param time string including the time information
#' @param tzone string defining the time zone that will be use to format the time
#'
#' @return Timing data including the provided time zone definition
#'
#' @importFrom lubridate with_tz
#' @importFrom lubridate ymd_hms
getTime <- function(time, tzone="CET"){
  return(lubridate::with_tz(lubridate::ymd_hms(time), tzone))
}


