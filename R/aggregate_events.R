#' Aggregate Event Data
#'
#' Takes the event data from a LUCA survey data object and aggregates them,
#' either according to parts of the name, by a time interval, or by
#' both.
#'
#' In the first case `substr_start` marks the starting element of the substring
#' of the event name that is considered as relevant and `substr_stop` marks the
#' corresponding last character.
#' In the second case `interval` defines the number of seconds that will be
#' used to split the maximum module time into intervals; each interval will
#' then be assigned the event code that lasted the longest within the given
#' interval. As a result the event lists of all participants will be of equal
#' length.
#' Both variants can also be combined.
#'
#' @param survey_data LUCA survey data object
#' @param substr_start First character element to be considered in the name
#'   of the event
#' @param substr_stop Last character element to be considered in the name
#'   of the event
#' @param interval Number of seconds for each considered interval
#'
#' @return A list including the aggregated event data
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom purrrlyr invoke_rows
#' @importFrom tidyr unnest_wider
#' @importFrom tidyselect all_of
#' @export
aggregate_events <- function (survey_data, substr_start=1, substr_stop=10, interval=0) {

  # get the names of the columns including the event lists
  module_event_lists <- names(survey_data$participation_data)[grepl("module_.._event_list", names(survey_data$participation_data))]

  if (interval>0){
    # get the maximum length of each module


  }

  # create result object for the survey data including aggregated event lists
  aggregated_survey_data <- survey_data

  # For each columns with module event lists call for each element (i.e. each participation)
  # the function to do the actual aggregation
  for (event_lists in module_event_lists) {
    aggregated_survey_data$participation_data[event_lists] <- aggregated_survey_data$participation_data[event_lists] %>%
      purrrlyr::invoke_rows(.f=aggregate_events_internal, substr_start=substr_start, substr_stop=substr_stop, interval=interval) %>%
      dplyr::select(.out)
  }

  return(aggregated_survey_data)
}
globalVariables(c(".out"))



#' Aggregate event data from a single participation
#'
#' Takes event data from a single participation (event codes, times, and durations)
#' and returns an aggregatd form of the three vectors within a table.
#'
#' @param ... List of arguments (in fact columns) of the event data from a single participation
#' @param substr_start First character element to be considered in the name
#'   of the event
#' @param substr_stop Last character element to be considered in the name
#'   of the event
#' @param interval Number of seconds for each considered interval
#' @return A list including the aggregated event data
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr select
#'
aggregate_events_internal <- function (..., substr_start, substr_stop, interval) {

  # Retrieve event data from the function argument
  event_list <- list(...)[[1]]

  # Testing that events exist
  if (!is.null(event_list)){

    #aggregate events with identical ids ocurring directly after each other
    aggregated_event_list <- event_list %>%

      # shorten event code names as indicated by the substring arguments
      dplyr::mutate(code=substr(code, substr_start, substr_stop)) %>%

      # helper variable to check if the event_code is the same as the previous one
      dplyr::mutate(previous_code=dplyr::lag(code, default="XXXXXXXXXX")) %>%

      # helper variable to later calculate the intensity (i.e. how often an event occurred)
      # previously aggregated events are considered by adding up the overall intensiry
      dplyr::mutate(intensity_sum=get_intensity_sum(intensity)) %>%

      # only keep those cases where the current workflow code is different from the previous
      dplyr::filter(code!=previous_code) %>%

      # calculation of the activity duration after summarizing evnets with identical workflows codes occurring directly after each other
      dplyr::mutate(duration=dplyr::lead(project_time)-project_time) %>%

      # calculation  of the intensity (i.e. how how often the summarized events occurred in the original data set directly after each other)
      dplyr::mutate(intensity=dplyr::lead(intensity_sum)-intensity_sum) %>%

      # preparation of the result data set
      dplyr::select(time, code, duration, intensity, label, data, project_time, module_time)

    if (interval>0){
      # the aggregated event list will be reformatted to an event list with constant time intervals
      n_intervals <- as.integer(ceiling(max(event_list$module_time)/interval))
      interval_list <- tibble(interval_start=NA_integer_, interval_end=NA_integer_, code=NA_character_, duration=NA,
                              intensity=NA_integer_, n_events=NA_integer_, events=NA_character_)
      for (start_time in seq(0, (n_intervals-1)*interval, by=interval)){
        # filter relevant events
        interval_events <- aggregated_event_list %>%
          filter( (module_time>=start_time & module_time<(start_time+interval)) |
                    (module_time+duration > start_time & module_time<(start_time+interval)) ) %>%
        # correct duration only considering the time within the interval
          mutate(new_duration=duration + (module_time<start_time)*(module_time-start_time) +
                                         (((module_time+duration)>(start_time+interval)) * (start_time+interval-module_time-duration)) )
        # Event with longest duration
        longest_event <- interval_events %>%
          slice(which.max(new_duration))

        # add new row to the event list organized in intervals
        interval_list <- interval_list %>%
          tibble::add_row(interval_start=start_time, interval_end=start_time+interval,
                          code=longest_event$code, duration=longest_event$new_duration,
                          intensity=sum(interval_events$intensity, na.rm = TRUE),
                          n_events=nrow(interval_events), events=paste0(interval_events$code, collapse=";"))
      }
      # remove first line that was needed for initialization of the tibble
      interval_list <- interval_list %>%
        filter(!is.na(interval_start))
      # replace aggregated event list with the new interval list
      aggregated_event_list <- interval_list
    }

  }


  return(aggregated_event_list)
}
globalVariables(c(".out", "event_code", "event_duration", "previous_event_code",
                  "intensity_sum", "module_time", "new_duration", "interval_start"))


#' Aggregate intensity values in a new vector
#'
#' The aggregated vector includes at each position the sum of all intensity
#' values provided until that positions in the given intensity vector
#'
#' @param intensity Vector including intensity values
#'
get_intensity_sum <- function (intensity) {
  intensity_sum <- intensity
  for (i in 2:length(intensity)){
    intensity_sum[i] <- intensity_sum[i-1]+intensity[i]
  }
  return(intensity_sum)
}
