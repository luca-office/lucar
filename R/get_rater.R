#' Getting the list of raters assigned in this project
#'
#' Takes the log data from a single participation and returns a table with the names of all raters and their assigned rater number.
#'
#' @param json_data Nested list including the log data for a single participation
#' @param hash_ids If TRUE the internal hash IDs for the questionnaire elements are included
#'
#' @return A dataframe including all rater, their id number, names, emails, and organizations
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(json_file)
#' rater <- get_rater(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
get_rater <- function (json_data, hash_ids=FALSE) {

  rater_qst <- json_data$scoring$questionnaireScoring %>%
    { if (length(.)==0 || !exists("ratings", where = .[[1]])) {
      dplyr::tibble(ratings_rater_id=character())
    } else {
      # unlist questions in all questionnaires into rows
      dplyr::bind_rows(.) %>%
      # Unnest all questions
      tidyr::unnest_wider(ratings, names_sep="_") %>%
      tidyr::unnest_wider(ratings_rater, names_sep="_")
      }
    }

  rater_sc <- json_data$scoring$scenarioScoring %>%
    { if (length(.)==0 || !exists("ratings", where = .[[1]])) {
      dplyr::tibble(ratings_rater_id=character())
    } else {
      # unlist questions in all questionnaires into rows
      dplyr::bind_rows(.) %>%
      # Unnest all questions
      tidyr::unnest_wider(ratings, names_sep="_") %>%
      tidyr::unnest_wider(ratings_rater, names_sep="_")
      }
    }

  rater <-
    # join the rater from scenarios and questionnaires
    dplyr::full_join(rater_qst, rater_sc, by=intersect(names(rater_qst), names(rater_sc))) %>%
    {
      if (nrow(.)==0){
        dplyr::tibble()
      } else {
        # only keep each rater once in the table
        dplyr::distinct(., ratings_rater_id, .keep_all= TRUE) %>%
        # set rater number
        dplyr::mutate(rater_no=stringr::str_pad(dplyr::row_number(), 2, pad="0")) %>%
        # select and name final set of variables
        dplyr::select(rater_no, rater_id=ratings_rater_id, email=ratings_rater_email, first_name=ratings_rater_firstName,
                      last_name=ratings_rater_lastName, ratings_rater_organization)
      }
    }


  return(rater)
}
globalVariables(c("ratings", "ratings_rater", "ratings_rater_id", "rater_no",
                  "ratings_rater_email", "ratings_rater_firstName",
                  "ratings_rater_lastName", "ratings_rater_organization"))
