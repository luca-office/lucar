#' Answer Data of the Questionnaires from a Single Participation
#'
#' Takes the log data and the already prepared workflow data from a single participant
#' and returns a single line dataframe with the answer data of included questionnaires.
#'
#' @param json_data The log data for a single participation in form of a json object
#' @param project_modules Dataframe including the running numbers and ids for the project modules
#' @param scenario_elements Dataframe including the codes for all existing scenario elements, which are used in the event list.
#' @param questionnaire_elements Dataframe including the codes for all questionnaire elements, which are used in the event list as well as in the summary results.
#' @param rater Dataframe including the running numbers and ids for the rater in the project
#' @param debug_mode If TRUE the internal hash IDs for the project elements are included and no module specific data is returned.
#'
#' @return A dataframe (consisting of one row) including general information for a single participation
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' workflow <- get_workflow(json_data, module_specific=TRUE)
#' questionnaire_data <- get_questionnaire_data(json_data, workflow)
#' }
#'
#' @importFrom dplyr tibble
get_questionnaire_data <- function (json_data, project_modules, scenario_elements, questionnaire_elements, rater, debug_mode=FALSE){

  # initialization of result object
  qst_data <- dplyr::tibble()

  # selection of the object including the scoring for the project's questionnaires
  questionnaire_list <- json_data$scoring$questionnaireScorings

  for (qst in questionnaire_list) {
    qst_code <- plyr::mapvalues(qst$questionnaireId, project_modules$module_id, project_modules$code, warn_missing = FALSE)

    for (rating in qst$ratings){

      # setting description of current rater
      rater_no <- plyr::mapvalues(rating$rater$id, rater$rater_id, rater$rater_no, warn_missing = FALSE)
      rater_description <- paste0("rater", rater_no)
      if (rating$isFinalScore) {
        rater_description <- paste0(rater_description, "_final")
      }

      # preparation of the scores from the current rater
      answer_data <-
        # transforming nested list into a iibble
        dplyr::as_tibble(data.frame(matrix(unlist(rating$questions, recursive=FALSE), nrow=length(rating$questions), byrow=TRUE))) %>%
        # retrieving original column names
        dplyr::rename_with(~names(rating$questions[[1]])) %>%
        # unnest columns provided in lists
        dplyr::mutate(across(c(id, score), ~sapply(.x, function(x) x[[1]]))) %>%
        # get question id as given in the questionnaire_element overview and add rater description
        dplyr::mutate(question_id=paste0("M", qst_code, "Q",
                                         plyr::mapvalues(id, questionnaire_elements$question_id, questionnaire_elements$question_no, warn_missing = FALSE),
                                         "_", rater_description)) %>%
        # select final set of variables
        dplyr::select(question_id, score) %>%

        tidyr::pivot_longer(cols = -question_id, names_to = 'col_name') %>%
        tidyr::pivot_wider(names_from = question_id, values_from = value) %>%

        # remove variable with old column name
        dplyr::select(-col_name)


      qst_data <- dplyr::full_join(qst_data, answer_data, by=intersect(names(qst_data), names(answer_data)))

    }
  }

  return(qst_data)

}
globalVariables(c("id", "score", "question_id", "value", "col_name"))
