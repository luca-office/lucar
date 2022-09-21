#' Save Survey Data from LUCA Office in CSV Files
#'
#' After preparing survey data from LUCA office using `prepare_lsd()`, you
#' can use `write_lsd_csv()` or `write_lsd_csv2()` to save the various tables
#' returned from `prepare_lsd()` in form of CSVs,
#'
#' @param survey_data List including the result object from the function
#'   `prepare_lsd()`.
#' @param export_folder Folder to which the csv files including the workflow data
#'   are exported.
#'
#' @return `write_lsd_*()` returns the input `survey_data' invisibly.
#'
#' @examples
#'
#' # Searches in the current working directory and all subdirectories for log data from LUCA office
#' # and prepares the data, which is then use in write_workflow_data to save the part including the
#' # workflow data as csv
#' \dontrun{
#' survey_data <- prepare_lsd()
#' write_lsd_csv2(survey_data)
#' }
#'
#' @importFrom readr write_excel_csv
#' @export
write_lsd_csv <- function (survey_data, export_folder="lsd_csv"){
  return(write_lsd(survey_data, export_folder, readr::write_excel_csv))
}


#' @importFrom readr write_excel_csv2
#' @rdname write_lsd_csv
#' @export
write_lsd_csv2 <- function (survey_data, export_folder="lsd_csv"){
  return(write_lsd(survey_data, export_folder, readr::write_excel_csv2))
}


#' Save Survey Data from LUCA Office Using a Specified Write Function
#'
#' This function does the heavy lifting for the write functions
#' `write_lsd_csv()` and `write_lsd_csv2()` and allows to flexibly define
#' additional functions to save the survey data.
#'
#' @param survey_data List including the result object from the function
#'   `prepare_lsd()`.
#' @param export_folder Folder to which the csv files including the workflow data
#'   are exported.
#' @param write_function Function which is used to save the different tables
#'   included in the prepared survey data.
#'
#' @return `write_lsd()` returns the input `survey_data' invisibly.
#'
#' @importFrom dplyr tibble
#' @importFrom dplyr %>%
#' @importFrom tidyselect matches
#' @importFrom tidyselect any_of
#' @importFrom tidyr unnest
write_lsd <- function (survey_data, export_folder="lsd_csv", write_function){

  # folder in which the csv files will be written
  dir.create(export_folder)

  # writing information on the project modules
  write_function(survey_data$project_modules,
                         file=file.path(export_folder, paste0("project_modules.csv")))

  # writing information on the scenario elements
  write_function(survey_data$scenario_elements,
                         file=file.path(export_folder, paste0("scenario_elements.csv")))

  # writing information on the questionnaire elements
  write_function(survey_data$questionnaire_elements,
                         file=file.path(export_folder, paste0("questionnaire_elements.csv")))

  # writing information on the assigned rater
  write_function(survey_data$rater,
                         file=file.path(export_folder, paste0("rater.csv")))

  # writing participation data (excluding event data)
  survey_data$participation_data %>%
    dplyr::select(!tidyselect::matches(paste0("^module_.._event"))) %>%
    write_function(file=file.path(export_folder, paste0("participation.csv")))

  # writing event data

  # create subfolder for the event data
  event_list_folder <- file.path(export_folder, "event_lists")
  dir.create(event_list_folder)

  for (participation_token in survey_data$participation_data$token) {
    # create subfolder for each participation
    dir.create(file.path(event_list_folder, participation_token))

    for (module in survey_data$project_modules$code) {
      survey_data$participation_data %>%
        dplyr::filter(token==participation_token) %>%
        dplyr::select(tidyselect::matches(paste0("^module_",module, "_event"))) %>%
        tidyr::unnest(cols=tidyselect::any_of(names(.))) %>%
        # event list for each participation and each project module is written in a separate csv
        write_function(file=file.path(event_list_folder, participation_token, paste0("module_", module, ".csv")))
    }
  }
  return(invisible(survey_data))
}
globalVariables(c("token"))

