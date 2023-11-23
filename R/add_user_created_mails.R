#' Adding User Created Mails to the Scenario Elements
#'
#' Takes the event data from a single participant and returns a table with mail
#' recipients the participant was writing a mail and 4-digit codes for each
#' recipient.
#'
#' @param scenario_elements Table including currently existing scenario elements and their codes
#' @param participant_events Table including minimally pre-processed log data for a single participation
#'
#' @return Table including a completed list of scenario elements
#'
#' @importFrom dplyr %>%
add_user_created_mails <- function (scenario_elements, participant_events) {

  # Get table with all email header update events (showing the email recipient)
  email_header_update_events <- participant_events %>%
    dplyr::filter(eventType=="UpdateEmail") %>%
    tidyr::unnest_wider(data) %>%
    dplyr::arrange(plyr::desc(timestamp))


  # if no corresponding email events exist return scenario_elements as is
  if (nrow(email_header_update_events)==0) {
    return(scenario_elements)

  # if email events exist prepare changed scenario_elements table
  } else {

    # Determine the completion email addresses
    completion_email_elements <- scenario_elements %>%
      filter(startsWith(usage_type, "CompletionEmail"))

    # Determine the next available code number
    last_code <- max(as.numeric(substr(scenario_elements$element_code,2,4)), na.rm = TRUE)


    completed_scenario_elements <- email_header_update_events %>%
      dplyr::distinct(., id, .keep_all=TRUE) %>% # keep only one row for emails with equal ids
      dplyr::filter(to!="") %>% # remove all rows with empty `to` column
      dplyr::select(id, name=to) %>% # reduce to the two relevant columns
      dplyr::mutate(usage_type="UserCreatedEmail", doc_type="mail", # complete columns for UserCreatedEmail
                    relevance=NA_character_,
                    element_code=NA_character_) %>%
      left_join(completion_email_elements %>% select(name, completion_code = element_code), by = "name") %>%
      mutate(element_code = ifelse(usage_type == "UserCreatedEmail" & is.na(element_code), completion_code, element_code)) %>%
      mutate(element_code = as.character(element_code)) %>%
      select(-completion_code) %>%
      dplyr::full_join(scenario_elements, by=c("element_code", "id", "name", "usage_type", "relevance", "doc_type")) %>% # join the currently given scenario elements
      mutate(relevance = ifelse(is.na(relevance) & !is.na(element_code), "Required", relevance)) %>% # all new mail elements that already have an element code from a completion mail address are required
      mutate(relevance = ifelse(is.na(relevance) & is.na(element_code), "Irrelevant", relevance)) %>% # all new mail elements that are not completion mails are irrelevant
      mutate(element_code = ifelse(is.na(element_code),
                                   stringr::str_pad(cumsum(is.na(element_code)) + last_code, 4, pad = "0"),
                                   element_code))
  }

  return(completed_scenario_elements)
}
globalVariables(c("eventType", "data", "timestamp", "id", "to", "code", "completion_code"))
