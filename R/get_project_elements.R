#' Getting the scenario elements and its respective codes for the data
#'
#' Takes the log data from a single participation and returns a list with the names of all project elements and the respective workflow codes.
#' These might prepared onboarding steps, emails, excel sheets, pdf, events and other elements.
#'
#' @param json_data The log data for a single participation in form of a nested list
#'
#' @return A dataframe including the scenario elements, their workflow codes and other relevant information
#'
#' @examples
#'
#' @export
get_scenario_elements <- function (json_data, basic_wf_codes=basic_wf_codes) {

  # prepare the nested list with the info on the scenario files as a tibble
  scenario_data <- json_data$files %>%
    purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
    bind_rows() %>%   # format list as dataframe
    mutate(doc_type=sub(pattern = "(.*)\\.", replacement = "", name)) %>%  # extract file extension from file name
    mutate(wf_code=NA) %>% # initialization of the variable for the workflow codes
    select(id, name, wf_code, usage_type=usageType, relevance, doc_type)  # select only relevant variables


  # construction of the workflow code
  scenario_data$wf_code <- paste0(recode(substr(scenario_data$relevance, 1, 1), "I"=0, "P"=1, "R"=2),
                                  stringr::str_pad(1:nrow(scenario_data), width=3, side="left", pad="0"))

  scenario_elements <- scenario_data


  return(scenario_elements)
}
