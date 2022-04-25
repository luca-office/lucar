#' Write LUCA Office logdata data to csv files
#'
#' After preparing (JSON) log data from LUCA office using  `prepare_data()`, you can
#' use `write_logdata_csv()` or `write_logdata_csv2()` to write the workflow data, the
#' participation data including aggregated information on the person level as well as
#' two tables including information on the files existing in the scenarios and the
#' names of the scenarios themselves.
#'
#' @param logdata A list including prepared data from the function `prepare_logdata()`.
#' @param folder A folder to which the csv files including the workflow data are exported.
#'
#' @return A list including the prepared data from all JSON files
#'
#' @examples
#'
#' # Searches in the current working directory and all subdirectories for log data from LUCA office
#' # and prepares the data, which is then use in write_workflow_data to save the part including the
#' # workflow data as csv
#' \dontrun{
#' logdata <- prepare_logdata()
#' write_workflow_csv(logdata)
#' }
#'
#' @importFrom rjson fromJSON
#' @importFrom dplyr tibble
#' @importFrom readr write_excel_csv
#' @export
write_logdata_csv <- function (logdata, folder="logdata"){

  # In this folder the logdata will be written
  dir.create(folder)

  # writing participation data
  readr::write_excel_csv(logdata$participation,
                         file=file.path(folder, paste0("participation.csv")))

  # writing information on the project elements
  readr::write_excel_csv(logdata$project_elements,
                         file=file.path(folder, paste0("project_elements.csv")))

  # writing information on the project scenarios
  readr::write_excel_csv(logdata$project_scenarios,
                         file=file.path(folder, paste0("project_scenarios.csv")))

  # writing the workflow data
  wf_folder <- file.path(folder, "workflows")
  dir.create(wf_folder)
  for (participant in names(logdata$workflows)) {
    dir.create(file.path(wf_folder, participant))

    for (scenario in names(logdata$workflows[[participant]])) {
      readr::write_excel_csv(logdata$workflows[[participant]][[scenario]],
                 file=file.path(wf_folder, participant, paste0("scenario_", scenario, ".csv")))
    }
  }
  return(invisible(logdata))
}

#' @importFrom rjson fromJSON
#' @importFrom dplyr tibble
#' @importFrom readr write_excel_csv2
#' @rdname write_logdata_csv
#' @export
write_logdata_csv2 <- function (logdata, folder="logdata"){

  # In this folder the logdata will be written
  dir.create(folder)

  # writing participation data
  readr::write_excel_csv2(logdata$participation,
                         file=file.path(folder, paste0("participations_summary.csv")))

  # writing information on the project elements
  readr::write_excel_csv2(logdata$project_elements,
                         file=file.path(folder, paste0("project_elements.csv")))

  # writing information on the project scenarios
  readr::write_excel_csv2(logdata$project_scenarios,
                         file=file.path(folder, paste0("project_modules.csv")))

  # writing the workflow data
  wf_folder <- file.path(folder, "workflows")
  dir.create(wf_folder)
  for (participant in names(logdata$workflows)) {

    for (module in names(logdata$workflows[[participant]])) {
      readr::write_excel_csv2(logdata$workflows[[participant]][[module]],
                             file=file.path(wf_folder, paste0(participant, "_", module, ".csv")))
    }
  }
  return(invisible(logdata))
}
