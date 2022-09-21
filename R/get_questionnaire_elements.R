#' Getting the questionnaire elements from a project
#'
#' Takes the log data from a single participation and returns a table with the
#' names of all questionnaire elements and runtime survey elements (which are
#' technically questionnaires as well) and the respective event codes.
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
#' @importFrom dplyr coalesce
get_questionnaire_elements <- function (json_data, hash_ids=FALSE) {

  questionnaire_elements <- json_data$questionnaires %>%
    append(json_data$runtimeSurveys) %>%
    { if (length(.)==0) {
      NULL
    } else {

      # unlist questions in all questionnaires into rows
      dplyr::bind_rows(.) %>%

      # Unnest all questions
      tidyr::unnest_wider(questions, names_sep="_") %>%

      # add running id for questionnaires and questions within questionnaires
      dplyr::group_by(id) %>%
      dplyr::mutate(questionnaire_no=dplyr::cur_group_id()) %>%
      dplyr::mutate(question_no=stringr::str_pad(dplyr::row_number(), 3, pad = "0")) %>%

      # Unnest all answers
      tidyr::unnest_longer(questions_answers) %>%
      tidyr::unnest_wider(questions_answers, names_sep="_") %>%
      tidyr::unnest_longer(questions_freetextQuestionCodingCriteria) %>%
      tidyr::unnest_wider(questions_freetextQuestionCodingCriteria, names_sep="_") %>%

      # Order (questions and) and answers according to their position in the questionnaire
      # TODO: replace question_no by questions_position as soon as variable available
      plyr::arrange(questionnaire_no, question_no, questions_answers_position) %>%


      # add running id for answers
      dplyr::group_by(questions_id) %>%
      dplyr::mutate(answer_no=stringr::str_pad(dplyr::row_number(), 2, pad = "0")) %>%

      # add complete codes
      mutate(answer_code=paste0("Q", question_no, "A", answer_no)) %>%

      # set answer codes for open answers to 00 at the end
#      mutate(answer_code=paste0("Q", stringr::str_pad(question_no, 3, pad = "0"), "A", stringr::str_pad(answer_no, 2, pad = "0"))) %>%


      # merge answer category ids and descriptions for closed and open responses
      mutate(answer_category_id=dplyr::coalesce(questions_answers_id, questions_freetextQuestionCodingCriteria_id)) %>%
      mutate(answer_category_description=dplyr::coalesce(questions_answers_text, questions_freetextQuestionCodingCriteria_description)) %>%

      # select and name final set of variables
      dplyr::select(questionnaire_no, question_no, answer_no, answer_code,
                    questionnaire_id=id, questionnaire_title=title, questionnaire_description=description,
                    questionnaire_type=questionnaireType, questionnaire_maxDurationInSeconds=maxDurationInSeconds,
                    question_id=questions_id, question_text=questions_text, question_type=questions_questionType,
                    question_isAdditionalFreeTextAnswerEnabled=questions_isAdditionalFreeTextAnswerEnabled,
                    answer_category_id, answer_category_description,
                    answer_closed_category_isCorrect=questions_answers_isCorrect, answer_position=questions_answers_position,
                    answer_freeText_category_score=questions_freetextQuestionCodingCriteria_score) %>%

      # remove hash IDs if 'hash_ids' is set to `FALSE`
      dplyr::ungroup() %>%
      dplyr::select_if(hash_ids|!grepl("_no$|_id$", names(.)))

      }
    }

  return(questionnaire_elements)
}
globalVariables(c("questions", "id", "questions_answers", "questions_freetextQuestionCodingCriteria",
                  "questionnaire_no", "question_no", "questions_answers_position", "questions_id",
                  "answer_no", "questions_answers_id", "questions_freetextQuestionCodingCriteria_id",
                  "questions_answers_text", "questions_freetextQuestionCodingCriteria_description",
                  "answer_code", "title", "description", "questionnaireType", "maxDurationInSeconds",
                  "questions_text", "questions_questionType",
                  "questions_isAdditionalFreeTextAnswerEnabled", "answer_category_id",
                  "answer_category_description", "questions_answers_isCorrect",
                  "questions_freetextQuestionCodingCriteria_score"))
