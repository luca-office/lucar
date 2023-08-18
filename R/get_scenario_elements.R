#' Getting the scenario elements from a project
#'
#' Takes the log data from a single participation and returns a table with the names of all scenario elements and the respective event codes.
#' These might be emails, excel sheets, pdf, and other elements.
#'
#' @param json_data Nested list including the log data for a single participation
#' @param project_modules Table of project elements, their ids and other info
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
#' @importFrom dplyr left_join
#' @importFrom dplyr case_when
#' @importFrom tibble add_row
#' @importFrom dplyr bind_rows
#' @export
get_scenario_elements <- function (json_data, project_modules) {

  # tibble with info on the project emails that are categorized according to their relevance
  emails <- if(length(json_data$emails) == 0) {
    dplyr::tibble()
  } else {
    json_data$emails %>%
      lapply(function(x) x[names(x) != "ccRecipients"]) %>% # removing ccRecipients since it has different types depending on its content
      purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
      dplyr::bind_rows() %>%   # format list as dataframe
      dplyr::mutate(
        binary_file_id = NA,
        spreadsheet_id = NA,
        sample_company_id = NA,
        scenario_id = NA,
        name = subject,
        usage_type = "Email",
        relevance = relevance,
        doc_type = "mail"
      ) %>%
      dplyr::select(id, binary_file_id, spreadsheet_id, sample_company_id, scenario_id, name, usage_type, relevance, doc_type)  # select only relevant variables
  }


  for (scenario in json_data$scenarios) {
    # add special type for introductory emails and add scenario_id
    emails$usage_type[emails$id==scenario$introductionEmailId] <- paste0("IntroductionEmail_Scenario",
                                                                         project_modules$code[project_modules$module_id==scenario$id])
    emails$scenario_id[emails$id==scenario$introductionEmailId] <- scenario$id
    # special row for the completion email address of a scenario
    emails <- emails %>%
      tibble::add_row(scenario_id = scenario$id,
                      name = scenario$completionEmailAddress[],
                      usage_type = paste0("CompletionEmailAddress_Scenario", project_modules$code[project_modules$module_id==scenario$id]),
                      doc_type = "string" )
  }


  # get sample company to include the corresponding titles in the companies file archive
  sample_companies <- get_sample_companies(json_data) %>%
    select(sampleCompanyId=id, sample_company_title=title)

  directories <-json_data$directories %>%
    purrr::map_depth(2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
    dplyr::bind_rows() %>%   # format list as dataframe
    dplyr::left_join(sample_companies, by="sampleCompanyId", na_matches="never") %>%
    dplyr::mutate(name=dplyr::case_when(
      name=="" ~ paste0("Company Directory for ", sample_company_title),
      TRUE ~ as.character(name)
    )) %>%   # remove directories for sample company (unknown what these are for)
    dplyr::mutate(binary_file_id=NA, spreadsheet_id=NA, usage_type="FileDirectory", relevance="Unknown", doc_type="directory") %>% # renaming variables according to the general format
    dplyr::select(id, binary_file_id, spreadsheet_id, sample_company_id=sampleCompanyId, name, usage_type, relevance, doc_type) %>%   # select only relevant variables
    tibble::add_row(name="Inbox", usage_type="MailFolder", relevance="Unknown", doc_type="directory") %>% # add system mail folder
    tibble::add_row(name="Draft", usage_type="MailFolder", relevance="Unknown", doc_type="directory") %>% # add system mail folder
    tibble::add_row(name="Sent", usage_type="MailFolder", relevance="Unknown", doc_type="directory") # add system mail folder


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
        dplyr::mutate(sample_company_id=NA) %>%  #  add id variable to be congruent with directories structure
        dplyr::select(id, binary_file_id=binaryFileId, spreadsheet_id=spreadsheetId, sample_company_id, name, usage_type=usageType, relevance, doc_type)  # select only relevant variables      }
      }
    }

  # tibble with info on the complete project files including those that are categorized according to their relevance
  binary_files <- json_data$binaryFiles %>%
    {
      if (length(.)==0) {
        dplyr::tibble()
      } else {
        purrr::map_depth(., 2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
          dplyr::bind_rows() %>%   # format list as dataframe
          dplyr::mutate(doc_type=sub(pattern = "(.*)\\.", replacement = "", filename)) %>%  # extract file extension from file name
          dplyr::mutate(binary_file_id=id) %>%  #  the given id variable corresponds to the binary file id given for the elements under 'files'
          dplyr::mutate(id=NA) %>%  #  set id to NA to avoid redundancy
          dplyr::mutate(relevance="Unknown") %>%  #  set id to NA to avoid redundancy
          dplyr::mutate(spreadsheet_id=NA, sample_company_id=NA, usage_type=NA) %>%  #  add id variables to be congruent with the general structure
          dplyr::select(id, binary_file_id, spreadsheet_id, sample_company_id, name=filename, usage_type, relevance, doc_type)  # select only relevant variables      }
      }
    }

  # tibble with info on the reference book chapters
  book_chapters <- json_data$referenceBookChapters %>%
    {
      if (length(.)==0) {
        dplyr::tibble()
      } else {
        purrr::map_depth(., 2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
          dplyr::bind_rows() %>%   # format list as dataframe
          dplyr::mutate(doc_type="book_chapter") %>%  # extract file extension from file name
          dplyr::mutate(relevance="Unknown") %>%  #  set id to NA to avoid redundancy
          dplyr::mutate(binary_file_id=NA, spreadsheet_id=NA, sample_company_id=NA, usage_type=NA) %>%  #  add id variables to be congruent with the general structure
          dplyr::select(id, binary_file_id, spreadsheet_id, sample_company_id, name=title, usage_type, relevance, doc_type)  # select only relevant variables      }
      }
    }

  # tibble with info on the reference book articles
  book_articles <- json_data$referenceBookArticles %>%
    {
      if (length(.)==0) {
        dplyr::tibble()
      } else {
        purrr::map_depth(., 2, ~ replace(.x, is.null(.x), NA)) %>% # replacing NULL elements by NA
          dplyr::bind_rows() %>%   # format list as dataframe
          dplyr::mutate(doc_type="book_articles") %>%  # extract file extension from file name
          dplyr::mutate(relevance="Unknown") %>%  #  set id to NA to avoid redundancy
          dplyr::mutate(binary_file_id=NA, spreadsheet_id=NA, sample_company_id=NA, usage_type=NA) %>%  #  add id variables to be congruent with the general structure
          dplyr::select(id, binary_file_id, spreadsheet_id, sample_company_id, name=title, usage_type, relevance, doc_type)  # select only relevant variables      }
      }
    }


  # combining the elements in a single table
  scenario_elements <- dplyr::bind_rows(emails, directories, files, binary_files, book_chapters, book_articles) %>%
    {
      if (length(.)>0) {
        # Remove duplicates included in 'files' and 'binary_files' (elements from 'files' will be kept)
        dplyr::filter(., !duplicated(binary_file_id, incomparables = NA)) %>%
        # setting the running workflow codes for each project element
        dplyr::mutate(element_code=construct_element_code(relevance), .before = 1)
      } else {
        dplyr::tibble()
      }
    }

  return(scenario_elements)
}
globalVariables(c("subject", "relevance", "id", "binary_file_id", "spreadsheet_id",
                  "name", "doc_type", "binaryFileId", "spreadsheetId", "usageType",
                  "sample_company_id", "filename"))


#' Helper function to assign the running event code for elements of the same type
#'
#' @param relevance string indicating the relevance of a given LUCA office element
#'
#' @importFrom dplyr recode
#' @importFrom stringr str_pad
construct_element_code <- function(relevance){
  event_code <- paste0(recode(substr(relevance, 1, 1), "I"=0, "P"=1, "R"=2, "U"=9),
         stringr::str_pad(1:length(relevance), width=3, side="left", pad="0"))
  return (event_code)
}
