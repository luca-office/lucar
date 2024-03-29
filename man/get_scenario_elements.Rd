% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_scenario_elements.R
\name{get_scenario_elements}
\alias{get_scenario_elements}
\title{Getting the scenario elements from a project}
\usage{
get_scenario_elements(json_data, project_modules)
}
\arguments{
\item{json_data}{Nested list including the log data for a single participation}

\item{project_modules}{Table of project elements, their ids and other info}
}
\value{
A dataframe including all elements existing in the scenarios of a project, their event codes and other relevant information
}
\description{
Takes the log data from a single participation and returns a table with the names of all scenario elements and the respective event codes.
These might be emails, excel sheets, pdf, and other elements.
}
\examples{
\dontrun{
json_file = "participation_logdata.json"
json_data <- rjson::fromJSON(json_file)
project_elements <- get_project_elements(json_data)
}

}
