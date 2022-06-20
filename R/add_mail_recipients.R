#' Getting the project module IDs and their titles
#'
#' Takes the event data from a single participant and returns a table with mail recipients the participant was writing a mail and 4-digit codes
#' for each recipient
#'
#' @param participant_events Tibble including minimally pre-processed log data for a single participation
#' @param mail_recipient_codes Tibble including previously assigned mail recipients and their codes
#'
#' @return A tibble including a completed list of mail recipients used by the participants and their codes for generating
#' more generalized event codes
#'
#'
#' @importFrom dplyr %>%
add_mail_recipients <- function (participant_events, mail_recipient_codes=tibble(recipient=character(), code=character())) {

  # Complete participant's recipient list
  completed_mail_recipient_codes <- participant_events %>%
    dplyr::filter(eventType=="UpdateEmail") %>%
    tidyr::unnest_wider(data) %>%
    dplyr::arrange(desc(timestamp)) %>%
    dplyr::distinct(id, .keep_all=TRUE) %>%
    dplyr::distinct(to) %>%
    dplyr::filter(to!="") %>%
    dplyr::select(recipient=to) %>%
    dplyr::full_join(mail_recipient_codes, by="recipient") %>%
    dplyr::arrange(code)

  # Check whether the table is still empty to avoid errors in coalesce()
    if (nrow(completed_mail_recipient_codes)>0){
      completed_mail_recipient_codes <- completed_mail_recipient_codes %>%
        dplyr::mutate(code=coalesce(code, stringr::str_pad(1:n(), width=2, side="left", pad="0")))
    }

  return(completed_mail_recipient_codes)
}
