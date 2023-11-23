#' Getting the questionnaire elements from a project
#'
#' Takes the log data from a single participation and returns a table with the
#' names of all questionnaire elements and runtime survey elements (which are
#' technically questionnaires as well) and the respective event codes.
#'
#' @param json_data Nested list including the log data for a single participation
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
get_questionnaire_elements <- function (json_data) {

  needed_variables <- data.frame(message=NA_character_, content=NA_character_, value=NA_character_, text=NA_character_, mimeType=NA_character_,
                                 spreadsheetTitle=NA_character_, binaryFileTitle=NA_character_, startCellName=NA_character_,
                                 endCellName=NA_character_, cellName=NA_character_, to=NA_character_, cc=NA_character_, subject=NA_character_,
                                 tool=NA_character_, directory=NA_character_, endType=NA_character_, answerPosition=NA_character_,
                                 spreadsheetId=NA_character_, occurred=NA_character_, interventionId=NA_character_,
                                 directoryId=NA_character_, textDocumentTitle=NA_character_, tableType=NA_character_, columnName=NA_character_,
                                 rowId=NA_character_, tableName=NA_character_, scenarioId=NA_character_, questionnaireId=NA_character_,
                                 binaryFileId=NA_character_, fileId=NA_character_, emailId=NA_character_,
                                 questions_freetextQuestionCodingCriteria_id=NA_character_, questions_freetextQuestionCodingCriteria_description=NA_character_,
                                 questions_freetextQuestionCodingCriteria_score=NA_character_, maxDurationInSeconds=NA_character_,
                                 questions_freeTextAnswer=NA_character_, questions_answers_id=NA_character_)


  questionnaire_data <- json_data$questionnaires %>%
    append(json_data$runtimeSurveys)

  # return empty tibble if no questionnaire data exists
  if (length(questionnaire_data) == 0) {
    return (dplyr::tibble())
  }

  # otherwise prepare tibble with questionnaire elements
  questionnaire_elements <- questionnaire_data %>%

    # unlist questions in all questionnaires into rows
    dplyr::bind_rows(.) %>%
    # Unnest all questions
    tidyr::unnest_wider(questions, names_sep = "_") %>%
    # add running id for questionnaires and questions within questionnaires
    dplyr::group_by(id) %>%
    dplyr::mutate(questionnaire_no = stringr::str_pad(dplyr::cur_group_id(), 2, pad = "0")) %>%
    dplyr::mutate(question_no = stringr::str_pad(dplyr::row_number(), 2, pad = "0")) %>%
    # merge answer fields
    dplyr::mutate(questions_answers = coalesce(questions_answers, questions_freeTextAnswer)) %>%
    # Unnest answers
    tidyr::unnest_longer(questions_answers) %>%
    tidyr::unnest_wider(questions_answers, names_sep = "__") %>%
    # add dummy variables for questionnaire data that was not generated for this participant
    tibble::add_column(!!!needed_variables[!names(needed_variables) %in% names(.)]) %>%
    # remove not needed rows for free text questions in Runtime Surveys
    dplyr::filter(is.na(questions_answers_id) | questions_answers_id == "" | questions_answers_id == "text") %>%
    {
      # Check if all elements in questions_freetextQuestionCodingCriteria are NULL
      all_null <-
        all(sapply(.$questions_freetextQuestionCodingCriteria, is.null))
      if (!all_null) {
        tidyr::unnest_longer(., questions_freetextQuestionCodingCriteria) %>%
          tidyr::unnest_wider(questions_freetextQuestionCodingCriteria, names_sep =
                                "_") %>%
          # merge answer category ids and descriptions for closed and open responses
          mutate(
            answer_category_id = dplyr::coalesce(
              questions_answers__id,
              questions_freetextQuestionCodingCriteria_id
            )
          ) %>%
          mutate(
            answer_category_description = dplyr::coalesce(
              questions_answers__text,
              questions_freetextQuestionCodingCriteria_description,
              questions_answers__1
            )
          )
      } else {
        # create empty dummy variables
        mutate(., answer_category_id = NA) %>%
          mutate(answer_category_description = NA) %>%
          mutate(questions_freetextQuestionCodingCriteria_score =
                   NA) %>%
          mutate(maxDurationInSeconds = NA)
      }
    } %>%
    # Order (questions and) and answers according to their position in the questionnaire
    # TODO: replace question_no by questions_position as soon as variable available
    plyr::arrange(questionnaire_no,
                  questions_position,
                  questions_answers__position) %>%
    # add running id for answers
    dplyr::group_by(questions_id) %>%
    dplyr::mutate(answer_no = stringr::str_pad(dplyr::row_number(), 2, pad = "0")) %>%
    # add complete codes
    mutate(answer_code = paste0("Q", questionnaire_no, "Q", question_no, "A", answer_no)) %>%
    # select and name final set of variables
    dplyr::select(questionnaire_no, question_no, answer_no, answer_code,
                  questionnaire_id=id, questionnaire_title=title, questionnaire_description=description,
                  questionnaire_type=questionnaireType, questionnaire_maxDurationInSeconds=maxDurationInSeconds,
                  question_id=questions_id, question_text=questions_text, question_type=questions_questionType,
                  question_isAdditionalFreeTextAnswerEnabled=questions_isAdditionalFreeTextAnswerEnabled,
                  answer_category_id, answer_category_description,
                  answer_closed_category_isCorrect=questions_answers__isCorrect, answer_position=questions_answers__position,
                  answer_freeText_category_score=questions_freetextQuestionCodingCriteria_score) %>%
    dplyr::ungroup()

  return(questionnaire_elements)
}

globalVariables(c("questions", "id", "questions_answers", "questions_freetextQuestionCodingCriteria",
                  "questionnaire_no", "question_no", "questions_answers_id", "questions_answers_position",
                  "questions_id", "answer_no", "questions_answers__id", "questions_answers__text",
                  "questions_answers__1", "questions_answers__position", "questions_answers__isCorrect",
                  "questions_freeTextAnswer", "questions_freetextQuestionCodingCriteria_id",
                  "questions_answers_text", "questions_freetextQuestionCodingCriteria_description",
                  "answer_code", "title", "description", "questionnaireType", "maxDurationInSeconds",
                  "questions_text", "questions_questionType",
                  "questions_isAdditionalFreeTextAnswerEnabled", "answer_category_id",
                  "answer_category_description", "questions_answers_isCorrect",
                  "questions_freetextQuestionCodingCriteria_score", "questions_position"))
