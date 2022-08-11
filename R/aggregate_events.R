#' Aggregates the event data from a single participation
#'
#' Takes the event data from a single participation and returns an aggregated form,
#' where events with identical codes that occur directly after each other are
#' aggregated to a single event. The duration values are correspondingly adjusted,
#' and for each aggregated event an intensity is calculated that describes how
#' many events occurred in the original form.
#'
#' @param workflow A list including the event data
#'
#' @return A list including the compressed event data
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' workflow <- get_workflow(json_data)
#' compressed_workflow <- aggregate_events(workflow)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr select
aggregate_events <- function (workflow) {

  aggregated_workflow <- workflow %>%
    # helper variable to chek if the event_code is the same as the previous one
    dplyr::mutate(previous_event_code=dplyr::lag(event_code)) %>%

    # helper variable to later calculate the intensity (i.e. how often an event occurred)
    dplyr::mutate(event_no=1:n()) %>%

    # only keep those case where the current workflow code is different from the previous
    dplyr::filter(event_code!=previous_event_code) %>%

    # calculation of the activity duration after summarizing evnets with identical workflows codes occurring directly after each other
    dplyr::mutate(event_duration=project_time-dplyr::lag(project_time)) %>%

    # calculation  of the intensity (i.e. how how often the summarized events occurred in the original data set directly after each other)
    dplyr::mutate(intensity=dplyr::lead(event_no)-event_no) %>%

    # preparation of the result data set
    dplyr::select(time, project_time, event_duration, label, event_code, intensity, name, usage_type)

  return(aggregated_workflow)
}
