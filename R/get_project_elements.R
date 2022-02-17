#' Getting the scenario elements and its respective codes for the data
#'
#' Takes the log data from a single participation and returns a list with the names of all project elements and the respective workflow codes.
#' These might prepared onboarding steps, emails, excel sheets, pdf, events and other elements.
#'
#' @param json_data Nested list including the log data for a single participation
#'
#' @return A dataframe including the scenario elements, their workflow codes and other relevant information
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' project_elements <- get_project_elements(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_depth
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export
get_project_elements <- function (json_data) {

  # tibble with info on the project files that are categorized according to their relevance
  categorized_files <- json_data$files %>%
    purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
    dplyr::bind_rows() %>%   # format list as dataframe
    dplyr::mutate(doc_type=sub(pattern = "(.*)\\.", replacement = "", name)) %>%  # extract file extension from file name
    dplyr::mutate(wf_code=construct_wf_code(relevance)) %>% # setting the running workflow codes for each element of this type
    dplyr::select(id, name, wf_code, usage_type=usageType, relevance, doc_type)  # select only relevant variables


  # tibble with info on the project emails that are categorized according to their relevance
  emails <- json_data$emails %>%
    lapply(function(x) x[names(x)!="ccRecipients"]) %>% # removing ccRecipients since it has different types depending on its content
    purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
    dplyr::bind_rows() %>%   # format list as dataframe
    dplyr::mutate(wf_code=construct_wf_code(relevance)) %>% # seting the running workflow codes for each element of this type
    dplyr::mutate(name=subject, usage_type="Email", relevance, doc_type="mail") %>% # renaming variables according to the general format
    dplyr::select(id, name, wf_code, usage_type, relevance, doc_type)  # select only relevant variables


  # combining the different elements in a common table
  project_elements <- rbind(categorized_files, emails)

  return(project_elements)
}

#' Helper function to assign the running workflow code for elements of the same type
#'
#' @param relevance string indicating the relevance of a given LUCA office element
#'
#' @importFrom dplyr recode
#' @importFrom stringr str_pad
construct_wf_code <- function(relevance){
  wf_code <- paste0(recode(substr(relevance, 1, 1), "I"=0, "P"=1, "R"=2),
         str_pad(1:length(relevance), width=3, side="left", pad="0"))
  return (wf_code)
}
