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
#' @param aggregate_events For `TRUE`, events with identical codes directly
#'   following each other will be collated to a single event.
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
prepare_lsd <- function (path = "./", aggregate_events=FALSE, idle_time=20,
                         unzip = TRUE, event_codes=lucar::event_codes,
                         tool_codes=lucar::tool_codes, debug_mode=FALSE) {

  cat("\n")

  # Setting 'module_specific' workflow preparation to FALSE for debugging mode and TRUE otherwise
  if (debug_mode) {
    module_specific <- FALSE
  } else {
    module_specific <- TRUE
  }

  # unzip files if indicated by function argument
  if (unzip){
    # get all zip files that correspond to the naming convention of a LUCA survey data archive
    zip_files <- grep("^Luca-Erhebungsdaten-.*\\.zip$", list.files(path), value=TRUE)
    cat(length(zip_files), "zip archives with survey data found.\n")
    if (length(zip_files>0)){
      cat("-> Unzipping archives ")
      path_temp <- file.path(path, "temp")
      for (zip_file in zip_files){
        cat(".")
        # extract all zip archives to a "temp" folder in the given path (will be created)
        unzip(file.path(path, zip_file), exdir = file.path(path, "temp"))
      }
      cat(" done!\n\n")
    } else {
      path_temp <- NULL
    }
  }

  # Get all JSON files located in the given path (including all subfolders)
  json_files <- grep("\\.json$", list.files(path, full.names=TRUE, recursive=TRUE), value=TRUE)
  cat(length(json_files), "files with participation data found.\n")

  # Initialization of objects for the result object
  participation_data <- dplyr::tibble(project=character())
  event_list <- list()
  unknown_events <- dplyr::tibble()


  # import first JSON to read the basic project data only from this element
  json_data <- rjson::fromJSON(file=file.path(json_files[[1]]))
  # get project modules overview and the corresponding hash IDs
  project_modules <- get_project_modules(json_data)
  # get scenario elements overview and their respective event codes
  scenario_elements <- get_scenario_elements(json_data)
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
    element_name <- sub('\\..*$', '', basename(json_file))
    event_list[[element_name]] <- get_event_list(json_data, project_modules, scenario_elements,
                                                 questionnaire_elements, module_specific=module_specific,
                                                 idle_time=idle_time, event_codes, tool_codes,
                                                 debug_mode=debug_mode)
    # Assigned mail recipient codes are stored in an extra variable and removed from the participant's event list
    scenario_elements <- event_list[[element_name]]$scenario_elements
    event_list[[element_name]]$scenario_elements <- NULL

    # summarize the workflow data if indicated by the corresponding argument
    if (aggregate_events) {
      event_list[[element_name]] <- aggregate_events(event_list[[element_name]])
    }


    # get tibble row including summarized logdata information for the current participant
    new_participation_data <- get_logdata_summary(json_data, event_list[[element_name]], debug_mode)
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
      incomplete_codes <- nchar(event_list[[element_name]]$event_code)!=10
      unknown_events <- rbind(unknown_events, event_list[[element_name]][incomplete_codes,])
      unknown_events <- unknown_events[!duplicated(unknown_events$event_type),]
    }
  }

  cat(" done!\n\n")

  # if not in debug mode remove hash_ids from the reference tables
  project_modules <- project_modules %>%
    dplyr::select_if(debug_mode|!grepl("^id$|_id$", names(.)))
  scenario_elements <- scenario_elements %>%
    dplyr::select_if(debug_mode|!grepl("^id$|_id$", names(.)))
  questionnaire_elements <- questionnaire_elements %>%
    dplyr::select_if(debug_mode|!grepl("_no$|_id$", names(.)))
  rater <- rater %>%
    dplyr::select_if(debug_mode|!grepl("_id$", names(.)))

  # preparation of the anser object
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
    cat("Temp folder with unzipped survey data deleted.\n")
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
  return(with_tz(ymd_hms(time), tzone))
}


#' Aggregate event data from a single participation
#'
#' Takes event data from a single participation and returns an aggregated form,
#' where events with identical codes that occur directly after each other are
#' aggregated to a single event. The duration values are correspondingly adjusted,
#' and for each aggregated event an intensity is calculated that describes how
#' many events occurred in the original form.
#'
#' @param event_list A list including the event data
#'
#' @return A list including the aggregated event data
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr select
#'
aggregate_events <- function (event_list) {

  aggregated_event_list <- event_list %>%
    # helper variable to chek if the event_code is the same as the previous one
    dplyr::mutate(previous_event_code=dplyr::lag(event_code)) %>%

    # helper variable to later calculate the intensity (i.e. how often an event occurred)
    dplyr::mutate(event_no=1:n()) %>%

    # only keep those case where the current workflow code is different from the previous
    dplyr::filter(event_code!=previous_event_code) %>%

    # calculation of the activity duration after summarizing evnets with identical workflows codes occurring directly after each other
    dplyr::mutate(event_duration=project_time-dplyr::lag(project_time)) %>%

    # calculation  of the intensity (i.e. how how often the summarized events occurred in the original data set directly after each other)
    dplyr::mutate(intensity=dplyr::lead(event_no)-event_no) %>%

    # preparation of the result data set
    dplyr::select(time, project_time, event_duration, label, event_code, intensity, name, usage_type)

  return(aggregated_event_list)
}
globalVariables(c("event_code", "previous_event_code", "project_time",
                  "event_no", "time", "event_duration", "label", "intensity",
                  "usage_type", "name"))

