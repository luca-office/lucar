#' Getting the scenario elements from a project
#'
#' Takes the log data from a single participation and returns a table with the names of all scenario elements and the respective event codes.
#' These might be emails, excel sheets, pdf, and other elements.
#'
#' @param json_data Nested list including the log data for a single participation
#'
#' @return A dataframe including all elements existing in the scenarios of a project, their event codes and other relevant information
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
get_scenario_elements <- function (json_data) {

  # tibble with info on the project emails that are categorized according to their relevance
  emails <- json_data$emails %>%
    {
      if (length(.)==0) {
        dplyr::tibble()
      } else {
        lapply(., function(x) x[names(x)!="ccRecipients"]) %>% # removing ccRecipients since it has different types depending on its content
        purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
        dplyr::bind_rows() %>%   # format list as dataframe
        dplyr::mutate(binary_file_id=NA, spreadsheet_id=NA, name=subject, usage_type="Email", relevance, doc_type="mail") %>% # renaming variables according to the general format
        dplyr::select(id, binary_file_id, spreadsheet_id, name, usage_type, relevance, doc_type)  # select only relevant variables
      }
    }


  # tibble with info on the project files that are categorized according to their relevance
  files <- json_data$files %>%
    {
      if (length(.)==0) {
        dplyr::tibble()
      } else {
        purrr::map_depth(., 2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
        dplyr::bind_rows() %>%   # format list as dataframe
        dplyr::mutate(doc_type=sub(pattern = "(.*)\\.", replacement = "", name)) %>%  # extract file extension from file name
        dplyr::filter(!duplicated(binaryFileId, incomparables = NA)) %>%
        dplyr::select(id, binary_file_id=binaryFileId, spreadsheet_id=spreadsheetId, name, usage_type=usageType, relevance, doc_type)  # select only relevant variables      }
      }
    }

  # combining the elements in a single table
  scenario_elements <- rbind(emails, files) %>%
    {
      if (length(.)>0) {
        # setting the running workflow codes for each project element
        dplyr::mutate(., element_code=construct_element_code(relevance), .before = 1)
      } else {
        dplyr::tibble()
      }
    }

  return(scenario_elements)
}
globalVariables(c("subject", "relevance", "id", "binary_file_id", "spreadsheet_id",
                  "name", "doc_type", "binaryFileId", "spreadsheetId", "usageType"))


#' Helper function to assign the running event code for elements of the same type
#'
#' @param relevance string indicating the relevance of a given LUCA office element
#'
#' @importFrom dplyr recode
#' @importFrom stringr str_pad
construct_element_code <- function(relevance){
  event_code <- paste0(recode(substr(relevance, 1, 1), "I"=0, "P"=1, "R"=2),
         stringr::str_pad(1:length(relevance), width=3, side="left", pad="0"))
  return (event_code)
}
