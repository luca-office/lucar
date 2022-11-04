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

  # Complete participant's recipient list
  completed_scenario_elements <- participant_events %>%
    dplyr::filter(eventType=="UpdateEmail") %>%
    tidyr::unnest_wider(data) %>%
    dplyr::arrange(plyr::desc(timestamp)) %>%
    # Skip the following steps if no emails are present and questionnaire_elements list as is
    {
      if (nrow(.)==0) {
        scenario_elements
      } else {
        dplyr::distinct(., id, .keep_all=TRUE) %>%
        dplyr::filter(to!="") %>%
        dplyr::select(id, name=to) %>%
        dplyr::mutate(usage_type="UserCreatedEmail", doc_type="mail", relevance="Irrelevant") %>%
        dplyr::mutate(element_code=stringr::str_pad(dplyr::row_number()+nrow(scenario_elements), 4, pad="0")) %>%
        dplyr::full_join(scenario_elements, by=c("element_code", "id", "name", "usage_type", "relevance", "doc_type")) %>%
        dplyr::arrange(substr(element_code,2,4))
      }
    }

  return(completed_scenario_elements)
}
globalVariables(c("eventType", "data", "timestamp", "id", "to", "code"))
