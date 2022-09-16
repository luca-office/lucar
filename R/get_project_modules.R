#' Getting the project module IDs and their titles
#'
#' Takes the log data from a single participation and returns a table with the IDs and names of all modules in the project
#'
#' @param json_data Nested list including the log data for a single participation
#' @param hash_ids If TRUE, the returned dataframe includes the hash IDs of the modules
#'
#' @return A dataframe including information on the project modules: module code, module title, hash ID of the module, and hash ID of the sample company used in the modules
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' project_modules <- get_project_modules(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_depth
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @importFrom dplyr coalesce
#' @importFrom stringr str_pad
#' @export
get_project_modules <- function (json_data, hash_ids=FALSE) {

  # tibble with info on the project files that are categorized according to their relevance
  modules <- json_data$project$projectModules %>%
    purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
    dplyr::bind_rows() %>%   # format list as dataframe
    dplyr::arrange(position) %>%  # order modules according to their position
    dplyr::mutate(code = stringr::str_pad(1:n(), width=2, side="left", pad="0")) %>% # setting the running event codes for each module
    dplyr::mutate(module_id=dplyr::coalesce(.$questionnaireId, .$scenarioId)) %>%
    dplyr::mutate(type=dplyr::case_when(!is.na(scenarioId) ~ "scenario",
                                               !is.na(questionnaireId) ~ "questionnaire")) %>%
    dplyr::select(code, type, title, module_id, sample_company_id=sampleCompanyId) %>% # select only relevant variables
    dplyr::select_if(hash_ids|!grepl("^id$|_id$", names(.))) # removing hash IDs if indicated by boolean argument 'hash_ids'
  return(modules)
}
