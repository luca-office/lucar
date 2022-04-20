#' Summarizing the workflow data from a single participation
#'
#' Takes the workflow data from a single participation and returns a summarized form,
#' where events with identical workflow codes that occur directly after each other are
#' summarized to a single activity. The durations are correspondingly returned on the
#' activity level, and for each task an intensity is calculated that describes how
#' many events occurred in the original datasets within a given activity.
#'
#' @param workflow A list including the workflow data
#'
#' @return A list including the summarized workflow data
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' workflow <- get_workflow(json_data)
#' sum_workflow <- summarize(workflow)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr select
#' @export
summarize_workflow <- function (workflow) {

  sum_workflow <- workflow %>%
    # helper variable to chek if the wf_code is the same as the previous one
    dplyr::mutate(previous_wf_code=dplyr::lag(wf_code)) %>%

    # helper variable to later calculate the intensity (i.e. how often an event occurred)
    dplyr::mutate(event_no=1:n()) %>%

    # only keep those case where the current workflow code is different from the previous
    dplyr::filter(wf_code!=previous_wf_code) %>%

    # calculation of the activity duration after summarizing evnets with identical workflows codes occurring directly after each other
    dplyr::mutate(activity_duration=project_time-dplyr::lag(project_time)) %>%

    # calculation  of the intensity (i.e. how how often the summarized events occurred in the original data set directly after each other)
    dplyr::mutate(intensity=dplyr::lead(event_no)-event_no) %>%

    # preparation of the result data set
    dplyr::select(time, project_time, activity_duration, label, wf_code, intensity, name, usage_type)

  return(sum_workflow)
}
