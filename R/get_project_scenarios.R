#' Getting the project scenario IDs and their titles
#'
#' Takes the log data from a single participation and returns a table with the IDs and names of all scenarios in the project
#'
#' @param json_data Nested list including the log data for a single participation
#' @param hash_ids If TRUE, the returned dataframe includes the hash IDs of the scenarios
#'
#' @return A dataframe including information on the project scenarios: scenario workflow code, scenario title, hash ID of the scenario, and hash ID of the sample company used in the scenario
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' project_scenarios <- get_project_scenarios(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_depth
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr n
#' @export
get_project_scenarios <- function (json_data, hash_ids=FALSE) {

  # tibble with info on the project files that are categorized according to their relevance
  scenarios <- json_data$project$projectModules %>%
    purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
    dplyr::bind_rows() %>%   # format list as dataframe
    dplyr::mutate(code = str_pad(1:n(), width=2, side="left", pad="0")) %>% # setting the running workflow codes for each scenario
    dplyr::select(code, title, scenario_id=scenarioId, sample_company_id=sampleCompanyId) %>% # select only relevant variables
    dplyr::select_if(hash_ids|!grepl("^id$|_id$", names(.))) # removing hash IDs if indicated by boolean argument 'hash_ids'
  return(scenarios)
}
