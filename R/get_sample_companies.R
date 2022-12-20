#' Getting the sample companies, their titles, and description
#'
#' Takes the log data from a single participation and returns a table with the sample company information
#'
#' @param json_data Nested list including the log data for a single participation
#'
#' @return A dataframe including information on the sample companies: their respective (hash) `id`, `title`, and `description`
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' project_modules <- get_sample_companies(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
get_sample_companies <- function (json_data) {

  # tibble with info on the project files that are categorized according to their relevance
  sample_companies <- json_data$sampleCompanies %>%
    lapply(function(x) x[1:3]) %>%  # remove all sample company elements except for the first three
    dplyr::bind_rows()   # format list as dataframe
  return(sample_companies)
}
globalVariables(c())
