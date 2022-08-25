#' Getting the questionnaire elements from a project
#'
#' Takes the log data from a single participation and returns a table with the names of all questionnaire elements and the respective event codes.
#'
#' @param json_data Nested list including the log data for a single participation
#' @param hash_ids If TRUE the internal hash IDs for the questionnaire elements are included
#'
#' @return A dataframe including all questionnaire elements, their event codes and other relevant information
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' questionnaire_elements <- get_questionnaire_elements(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @export
get_questionnaire_elements <- function (json_data, hash_ids=FALSE) {

  questionnaire_elements <- json_data$questionnaires %>%

    # unlist questions in all questionnaires into rows
    dplyr::bind_rows() %>%

    # Unnest all questions
    tidyr::unnest_wider(questions, names_sep="_") %>%

    # add running id for questionnaires and questions within questionnaires
    group_by(id) %>%
    dplyr::mutate(questionnaire_no=cur_group_id()) %>%
    dplyr::mutate(question_no=row_number()) %>%

    # Unnest all answers
    tidyr::unnest_longer(questions_answers) %>%
    tidyr::unnest_wider(questions_answers, names_sep="_") %>%
    tidyr::unnest_longer(questions_freetextQuestionCodingCriteria) %>%
    tidyr::unnest_wider(questions_freetextQuestionCodingCriteria, names_sep="_") %>%

    # Order (questions and) and answers according to their position in the questionnaire
    # TODO: replace question_no by questions_position as soon as variable available
    arrange(questionnaire_no, question_no, questions_answers_position) %>%


    # add running id for answers
    group_by(questions_id) %>%
    mutate(answer_no=row_number()) %>%

    # add complete codes
    mutate(answer_code=paste0("Q", stringr::str_pad(question_no, 3, pad = "0"), "A", stringr::str_pad(answer_no, 2, pad = "0"))) %>%

    # select and name final set of variables
    dplyr::select(questionnaire_no, question_no, answer_no, answer_code,
                  questionnaire_id=id, questionnaire_title=title, questionnaire_description=description,
                  questionnaire_type=questionnaireType, questionnaire_maxDurationInSeconds=maxDurationInSeconds,
                  question_id=questions_id, question_text=questions_text, question_type=questions_questionType,
                  question_isAdditionalFreeTextAnswerEnabled=questions_isAdditionalFreeTextAnswerEnabled,
                  answer_id=questions_answers_id, answer_choiceText=questions_answers_text,
                  answer_isCorrect=questions_answers_isCorrect, answer_position=questions_answers_position,
                  answer_freeText_codingCriteria_id=questions_freetextQuestionCodingCriteria_id,
                  answer_freeText_codingCriteria_description=questions_freetextQuestionCodingCriteria_description,
                  answer_freeText_codingCriteria_score=questions_freetextQuestionCodingCriteria_score) %>%

    # remove hash IDs if 'hash_ids' is set to `FALSE`
    dplyr::select_if(hash_ids|!grepl("_no$|_id$", names(.)))


  return(questionnaire_elements)
}
