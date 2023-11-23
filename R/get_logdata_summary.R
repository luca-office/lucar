#' Basic summary from the Logdata of a Participation
#'
#' Takes the log data and the already prepared workflow data from a single participant
#' and returns a single line dataframe with general information on the participation.
#'
#' @param json_data The log data for a single participation in form of a json object
#' @param event_lists The prepared data from the workflows of the participant
#' @param scenario_elements Table of scenario elements, their ids and other info
#' @param debug_mode If TRUE the internal hash IDs for the project elements are included and no module specific data is returned.
#'
#' @return A dataframe (consisting of one row) including general information for a single participation
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' workflow <- get_workflow(json_data, module_specific=TRUE)
#' participation_data <- get_participation_data(json_data, workflow)
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select_if
#' @importFrom dplyr filter
#' @importFrom dplyr slice
#' @importFrom dplyr arrange
#' @importFrom stringr str_extract
get_logdata_summary <- function (json_data, event_lists, scenario_elements, debug_mode=FALSE) {

  # Initialization of the dataframe row for the general answer data of the participant with its ID
  logdata_summary <- tibble::tibble(id = json_data$surveyInvitation$id) %>%

    # extract basic project and survey info
    dplyr::mutate(project = json_data$project$title, survey = json_data$survey$title) %>%
    # extract basic participation info
    dplyr::mutate(token = json_data$surveyInvitation$token,
           account_participation = json_data$surveyInvitation$isUserAccountParticipation,
           open_participation = json_data$surveyInvitation$isOpenParticipation,
           did_participate = FALSE) %>%
    # summary information for participants who have actually participated
    { if (length(event_lists)!=0) {
        dplyr::mutate(., did_participate = as.logical(length(json_data$surveyEvents)>0),
                      project_start = getTime(json_data$surveyInvitation$firstSurveyEventTimestamp),
                      project_end = getTime(json_data$surveyInvitation$lastSurveyEventTimestamp))
      } else {.}
    } %>%
    # Remove ID variable if indicated by Boolean 'hash_ids'
    dplyr::select_if(debug_mode|!grepl("^id$|Id$", names(.)))

  # Checking if invited participant actually participated
  # (to avoid errors by calling non existent variables)
  if (logdata_summary$did_participate) {
    logdata_summary <- logdata_summary %>%
      mutate(salutation = json_data$surveyEvents[[1]]$data$salutation,
             first_name = json_data$surveyEvents[[1]]$data$firstName,
             last_name = json_data$surveyEvents[[1]]$data$lastName)
  }


  # If not debug mode: Add module specific summary information on the event lists
  # (i.e., in debug mode module specific info is not available)
  if (!debug_mode) {
    for (module in names(event_lists)) {

      module_events <- event_lists[[module]]

      ######
      # Extraction of the content of the answer email
      #####

      # Take the id of the scenario given in the scenario start event
      scenario_id <- module_events$data[grepl("STR_SCN$", module_events$code)]

      # Use the id to get the completion email address for the scenario given in the `scenario_elements`
      completion_email_address <- scenario_elements$name[grepl("^CompletionEmailAddress", scenario_elements$usage_type)&
                                                           (scenario_elements$scenario_id==scenario_id)]

      # Extracting the content of the answer email
      # only take the text from the last sent email if the scenario was ended by the participant
      if (any(module_events$data[grepl("END_SCN$", module_events$code)]=="ByParticipant")){
        answer_sent <- module_events %>%
          dplyr::filter(grepl("^T01SND", code)) %>%
          dplyr::arrange(time) %>%
          dplyr::slice(n()) %>%
          dplyr::pull(data)

      # otherwise return `NA`
      } else {
        answer_sent <- NA
      }


      # If no completion email address exists (e.g., because an old data format was used
      if (length(completion_email_address)==0){
        # always set answer_last_edit to `NA`
        answer_last_edit <- NA

      # If completion email address is given
      } else {

        # look for all email address fields in the module events that include the completion mail address after `to` in the data field.
        completion_email_header_update_events <- module_events %>%
          dplyr::filter(grepl("^T01UPD", code)) %>% # filter to all email header updates
          dplyr::filter(stringr::str_extract(data, "^To: ([^;]+);") == paste0("To: ", completion_email_address, ";")) # filter to all that include the completion email address

        # get the element codes of all mails that include the completion mail address
        completion_email_element_code <- substr(completion_email_header_update_events$code, 7, 11)

        # If completion email was sent, set answer_last_edit to the same content
        if (length(answer_sent)>1){
          answer_last_edit <- answer_sent

        # If no completion email was sent
        # set the text of answer_last_edit to the text of the last edited email that included the completion email address
        } else {

          # get the last edit of all emails with the above resulting ids.
          answer_last_edit <- module_events %>%
            dplyr::filter(grepl("^T01EDT", code) & substr(code, 7, nchar(code)) %in% completion_email_element_code) %>% #select all edit events for the given codes
            dplyr::arrange (time) %>% #sort rows by time (for safety purposes - should be sorted already in that way)
            dplyr::slice(n()) %>% #select the last row/event
            dplyr::pull(data) # get the content of the data column

          ### In very rare cases, there might be an error if the `to`field is edited again after the editing of the email

          ### I might be that the participant copies the answer from the clipboard and does not edit it before sending. In that case the
          ### `answer_last_edit` is different from `answer_sent`! (See example event list below.)

          #> tail(module_events)
          ## A tibble: 6 x 8
          #time                code       duration       intensity label                data                                   project_time module_time
          #<dttm>              <chr>      <drtn>             <int> <chr>                <chr>                                  <drtn>       <drtn>
          #1 2022-01-17 13:01:48 T02DIR9037 2.1620002 secs         1 Email Explorer       "Draft"                                4278.408 se~ 3330.827 s~
          #2 2022-01-17 13:01:50 T01EDT0066 4.1030002 secs         5 Edit Email           ">\nHallo Frau Flocke,\n >\n >wie wir~ 4280.570 se~ 3332.989 s~
          #3 2022-01-17 13:01:54 T05PST0000 1.7399998 secs         1 Paste From Clipboard ""                                     4284.673 se~ 3337.092 s~
          #4 2022-01-17 13:01:56 T01SND0066 0.0150001 secs         1 Send Email           "Sehr geehrter Herr Boeschek, \n\nPow~ 4286.413 se~ 3338.832 s~
          #5 2022-01-17 13:01:56 T02DIR9038 1.1480000 secs         1 Email Explorer       "Sent"                                 4286.428 se~ 3338.847 s~
          #6 2022-01-17 13:01:57 M02END_SCN 0.0000000 secs         1 Scenario End         "ByParticipant"                        4287.576 se~ 3339.995 s~

        }
      }



      logdata_summary[[paste0("module_",module, "_start_time")]] <- as.numeric(module_events$project_time[1])
      logdata_summary[[paste0("module_",module, "_total_duration")]] <- as.numeric(module_events$module_time[length(module_events$module_time)])
      logdata_summary[[paste0("module_",module, "_answer_sent")]] <- answer_sent
      logdata_summary[[paste0("module_",module, "_answer_last_edit")]] <- answer_last_edit
      logdata_summary[[paste0("module_",module, "_event_list")]] <- list(module_events)
    }
  }

  return(logdata_summary)

}


#' Formatting time data
#' @param time Time value in string format that is formatted
#' @param tzone Time zone to be used for the formatting of the time value
#'
getTime <- function(time, tzone="CET"){
  if (is.null(time)){
    print(time)
    return(as.POSIXct(NA))
  } else {
    #print(time)
    return(lubridate::with_tz(lubridate::ymd_hms(time), tzone))
  }
}

