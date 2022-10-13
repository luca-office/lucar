#' Getting the workflow data from a single participation
#'
#' Takes the log data from a single participation and returns a list describing the workflow.
#'
#' @param json_data The log data for a single participation in form of a nested list
#' @param project_modules Dataframe including the running numbers and ids for the project modules
#' @param scenario_elements Dataframe including the codes for all existing scenario elements, which are used in the event list.
#' @param questionnaire_elements Dataframe including the codes for all questionnaire elements, which are used in the event list as well as in the summary results.
#' @param module_specific If TRUE the event list is split into separate lists for each module element
#' @param aggregate_duplicate_events If TRUE the duplicate events occurring directly after each other are aggregated.
#' @param idle_time Numeric describing after how many seconds an event is considered as idle.
#' @param event_codes Dataframe with the event codes that are used to structure the log data
#' @param tool_codes Dataframe with the tool codes that are used to assign each used tool to a common code
#' @param debug_mode If TRUE the internal hash IDs for the project elements and additional lower level data are included
#'
#' @return A list including the workflow data
#'
#' @examples
#' \dontrun{
#' json_file = "participation_logdata.json"
#' json_data <- rjson::fromJSON(file=json_file)
#' workflow <- get_workflow(json_data)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr rename_with
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom lubridate as_datetime
#' @importFrom dplyr filter
#' @importFrom plyr mapvalues
#' @importFrom rlang syms
#' @importFrom dplyr slice
#' @importFrom dplyr arrange
#' @importFrom tibble add_column
#' @importFrom dplyr coalesce
get_event_list <- function (json_data, project_modules, scenario_elements,
                            questionnaire_elements, module_specific=TRUE,
                            aggregate_duplicate_events=FALSE, idle_time=20,
                            event_codes=lucar::event_codes, tool_codes=lucar::tool_codes, debug_mode=FALSE) {

  # return only the element with the current list of questionnaire elements if no events were recorded
  if (length(json_data$surveyEvents)==0) {
    return(list(scenario_elements=scenario_elements))
  }

  # formatting the events into a tibble
  participant_events <-
    # transforming nested list into a iibble
    dplyr::as_tibble(data.frame(matrix(unlist(json_data$surveyEvents, recursive=FALSE), nrow=length(json_data$surveyEvents), byrow=TRUE))) %>%
    # retrieving original column names
    dplyr::rename_with(~names(json_data$surveyEvents[[1]])) %>%
    # unnest columns provided in lists
    dplyr::mutate(across(c(timestamp, eventType, index), ~sapply(.x, function(x) x[[1]]))) %>%
    # rename column
    dplyr::mutate(event_type=eventType)

  # get all mail recipients the participant(s) were entering in any email and add them to the list of scenario elements
  scenario_elements <- add_user_created_mails(scenario_elements, participant_events)

  # Construction of a helper dataframe that includes all variables used from the nested JSON data structure
  # it is needed to add missing variables to the temporary dataframe of the event list (e.g. if some events did not occur for the given participant)
  # without adding the variables the values for the data column prepared later would not run through
  needed_variables <- data.frame(message=NA_character_, content=NA_character_, value=NA_character_, text=NA_character_, mimeType=NA_character_,
                                 spreadsheetTitle=NA_character_, binaryFileTitle=NA_character_, startCellName=NA_character_,
                                 endCellName=NA_character_, cellName=NA_character_, to=NA_character_, cc=NA_character_, subject=NA_character_,
                                 tool=NA_character_, directory=NA_character_, endType=NA_character_, answerPosition=NA_character_,
                                 spreadsheetId=NA_character_, occurred=NA_character_, interventionId=NA_character_,
                                 directoryId=NA_character_, textDocumentTitle=NA_character_, tableType=NA_character_, columnName=NA_character_,
                                 rowId=NA_character_, tableName=NA_character_, scenarioId=NA_character_, questionnaireId=NA_character_,
                                 binaryFileId=NA_character_, fileId=NA_character_, emailId=NA_character_, questionId=NA_character_,
                                 answerId=NA_character_, id=NA_character_)


  event_list <-
    # match events with the basic_event_codes
    dplyr::left_join(participant_events, event_codes, by="event_type") %>%
    # exclude irrelevant variables
    dplyr::select(invitation_id=invitationId, survey_id=surveyId, timestamp, label, event_type, event_code, data=data, index=index) %>%
    # unnest columns with ids
    dplyr::mutate(invitation_id=unlist(invitation_id), survey_id=unlist(survey_id)) %>%
    # format time variable
    dplyr::mutate(time=lubridate::as_datetime(timestamp)) %>%

    dplyr::mutate(data2=data) %>%


    # unnest all variables included in the list variable data
    tidyr::unnest_wider(data) %>%
    # add dummy variables for event data that was not generated for this participant
    tibble::add_column(!!!needed_variables[!names(needed_variables) %in% names(.)]) %>%
    # unnest additional columns provided in lists (only done here to make sure they exist)
    dplyr::mutate(across(c(value), ~sapply(.x, function(x) x[[1]]))) %>%
    # renaming ID variables according to naming conventions
    dplyr::rename(binary_file_id=binaryFileId, spreadsheet_id=spreadsheetId, file_id=fileId, email_id=emailId) %>%

    # Combine the questionnaire and scenario id to a single module_id variable
    dplyr::mutate(module_id=dplyr::coalesce(.$scenarioId, .$questionnaireId)) %>%

    # The following steps are only conducted for a not empty list of scenario elements (i.e. not only questionnaires were defined but also scenarios)
    { if (length(scenario_elements)>0) {
        # match event ids with the ids of the scenario elements (if scenario element)
        dplyr::left_join(., select(scenario_elements,-c("binary_file_id","spreadsheet_id")), by="id", na_matches="never") %>%
        dplyr::left_join(select(scenario_elements,-c("id","spreadsheet_id")), by="binary_file_id", na_matches="never") %>%
        dplyr::left_join(select(scenario_elements,-c("id","binary_file_id")), by="spreadsheet_id", na_matches="never") %>%
        dplyr::left_join(select(scenario_elements,-c("id","spreadsheet_id")), by=c("file_id"="binary_file_id"), na_matches="never") %>%
        dplyr::left_join(select(scenario_elements,-c("binary_file_id", "spreadsheet_id")), by=c("email_id"="id"), na_matches="never") %>%

        # merge relevant variables (replacing NAs with values included due to subsequent joins above) and replace NAs by empty string
        dplyr::mutate(name=dplyr::coalesce(!!!syms(grep("^name",names(.), value=TRUE))),
                      usage_type=dplyr::coalesce(!!!syms(grep("^usage_type",names(.), value=TRUE))),
                      element_code=dplyr::coalesce(!!!syms(grep("^element_code",names(.), value=TRUE))),
                      element_code=replace(element_code, is.na(element_code), "")) %>%

        # join basic event codes with individual project element code
        dplyr::mutate(event_code=paste0(event_code, element_code))
      } else {.}
    } %>%

    # Order events according to their time stamps (this step might be unnecessary once the log data generation is corrected in LUCA office)
    arrange(time) %>%

    # calculate variables for project run time and event durations
    dplyr::mutate(project_time = time-time[1], event_duration = dplyr::lead(time)-time) %>%

    # exclude cases with a wf code of "#" (see basic table with event codes)
    dplyr::filter(event_code!="#") %>%


    # if no tool value is provided impute the value from mimeType
    dplyr::mutate(tool=replace(tool, is.na(tool)&!is.na(mimeType), mimeType[is.na(tool)&!is.na(mimeType)])) %>%
    # replace the imputed mimeType values acording to the values used in the tool variable
    dplyr::mutate(tool=recode(tool, ApplicationPdf="PdfViewer", ImageJpeg="ImageViewer", Spreadsheet="SpreadsheetEditor", VideoMp4="VideoPlayer")) %>%
    # integrate corresponding tool id into all event_codes were necessary (starting with "T##")
    dplyr::mutate(event_code=replace(event_code, grepl("^T##", event_code),
                                  paste0("T",
                                         plyr::mapvalues(tool, tool_codes$tool, tool_codes$code, warn_missing = FALSE)[grepl("^T##", event_code)],
                                         substr(event_code[grepl("^T##", event_code)], 4, 10) ))) %>%

    # integrate module id into the event_codes were necessary
    dplyr::mutate(event_code=replace(event_code, grepl("^M##", event_code),
                                        paste0("M",
                                               plyr::mapvalues(module_id, project_modules$module_id, project_modules$code, warn_missing = FALSE)[grepl("^M##", event_code)],
                                               substr(event_code[grepl("^M##", event_code)], 4, 10) ))) %>%


    # The following steps are only conducted for a non-empty list of questionnaire elements
    {
      if (length(questionnaire_elements)>0) {

        # integrate questionnaire, question, and answer ids into the event_codes
        dplyr::mutate(., event_code=replace(event_code,
                                            grepl("^Q##Q##A##", event_code),
                                            paste0("Q", plyr::mapvalues(questionnaireId, questionnaire_elements$questionnaire_id,
                                                                   questionnaire_elements$questionnaire_no, warn_missing = FALSE)[grepl("^Q##Q##A##", event_code)],
                                                   "Q", plyr::mapvalues(questionId, questionnaire_elements$question_id,
                                                                   questionnaire_elements$question_no, warn_missing = FALSE)[grepl("^Q##Q##A##", event_code)],
                                                   "A", plyr::mapvalues(answerId, questionnaire_elements$answer_category_id,
                                                                   questionnaire_elements$answer_no, warn_missing = FALSE)[grepl("^Q##Q##A##", event_code)],
                                                   substr(event_code[grepl("^Q##Q##A##", event_code)], 10, 10)))) %>%
        # set answer id to "00" for free text answers that are edited
        dplyr::mutate(., event_code=replace(event_code, grepl("^Q..Q..A..E", event_code),
                                            paste0(substr(event_code[grepl("^Q..Q..A..E$", event_code)], 1, 7),
                                                   "00",
                                                   substr(event_code[grepl("^Q..Q..A..E$", event_code)], 10, 10))))
      } else { . }
    } %>%


    # prepare content for the data column depending on the event type
    dplyr::mutate(data=dplyr::case_when(event_type=="StoreParticipantData" ~ paste0("Salutation: ", salutation, "; First name: ", firstName, "; Last name: ", lastName),
                                        event_type=="UpdateNotesText" | event_type=="UpdateEmailText" | event_type=="SendEmail" ~ text,
                                        event_type=="OpenTool" | event_type=="CloseTool" | event_type=="RestoreTool" | event_type=="MinimizeTool" ~ tool,
                                        event_type=="SelectEmailDirectory" ~ directory,
                                        event_type=="EndScenario" ~ endType,
                                        event_type=="OpenSpreadsheet" | event_type=="SelectSpreadsheet" ~ spreadsheetTitle,
                                        event_type=="SelectSpreadsheetCell" ~ cellName,
                                        event_type=="UpdateSpreadsheetCellValue" ~ paste0(cellName, ": ", value),
                                        event_type=="SelectSpreadsheetCellRange" ~ paste0(startCellName, " : ", endCellName),
                                        event_type=="ViewFile" ~ mimeType,
                                        event_type=="ViewDirectory" ~ paste0("Directory ID: ", directoryId),
                                        event_type=="OpenPdfBinary" | event_type=="SelectPdfBinary" | event_type=="SelectImageBinary" | event_type=="OpenImageBinary" | event_type=="OpenVideoBinary" | event_type=="SelectVideoBinary" ~ binaryFileTitle,
                                        event_type=="SendParticipantChatMessage" | event_type=="ReceiveSupervisorChatMessage" ~ message,
                                        event_type=="PasteFromClipboard" ~ content,
                                        event_type=="SelectQuestionnaireAnswer" | event_type=="UpdateQuestionnaireFreeTextAnswer" ~ value,
                                        event_type=="EvaluateIntervention" ~ paste0("Occurred: ", occurred, ", Intervention ID: ", interventionId),
                                        event_type=="OpenTextDocument" ~ textDocumentTitle,
                                        event_type=="UpdateTextDocumentContent" ~ content,
                                        event_type=="ErpSelectCell" ~ paste0("Table type: ", tableType, ", Column: ", columnName, ", Row index: ", rowId, ", Value: ", value),
                                        event_type=="ErpSelectTable" ~ paste0("Table name: ", tableName, ", Table type: ", tableType),
                                        # TODO: Completing the data column for not yet considered events
                                        #event_type=="" ~ value,
                                        event_type=="UpdateEmail" ~ paste0("To: ", to, "; CC: ", cc, "; Subject: ", subject)
                                        )) %>%

    # The following step is only conducted for a not empty list of scenario elements (in case only questionnaires were defined)
    { if (length(scenario_elements)>0) {
      # Fill missings in variable data with the value provided in the variable name (usually the name of the file the participant is working with)
      dplyr::mutate(., data=dplyr::coalesce(data, name))
      } else {
        .
      }
    } %>%

    # select final set of variables
    dplyr::select(invitation_id, survey_id, module_id, time, project_time, event_duration,
           label, event_code, data, event_type, data2, binary_file_id, email_id, spreadsheet_id, file_id) %>%

    # Removing hash IDs and debugging variables if 'debug_mode' is set to `FALSE`
    dplyr::select_if(debug_mode|!grepl("^event_type$|^data2$|^id$|_id$", names(.)))


    # If indicated insert idle events, in which the participant is not doing anything anymore
    if (idle_time) {
      event_list <- insert_idle_events(event_list, idle_time)
    }

    # Aggregate the events if indicated by the corresponding argument
    if (aggregate_duplicate_events) {
      event_list <- aggregate_duplicates(event_list)
    }


    # If indicated, split workflow into multiple lists, one for each module
    if (module_specific){
      full_event_list <- event_list %>%
        dplyr::mutate(module_time=project_time, .after=project_time)
      event_list <- list()
      for (module_code in project_modules$code){

        # Extracting the starting and ending event for the current module code
        first_module_event <- grep(paste0("^M",module_code,"STR_SCN$|^M",module_code,"STR_QST$"), full_event_list$event_code)

        # Making sure that the module was started - otherwise return NULL for this modules event list
        if (length(first_module_event)!=0) {
          last_module_event <- grep(paste0("^M",module_code,"END_SCN$|^M",module_code,"END_QST$"), full_event_list$event_code)
          # Checking if the participation was interrupted before the regular end and therefore no ending event is found
          last_module_event <- ifelse (length(last_module_event)==0, length(full_event_list$event_code), last_module_event)

          # Assigning the events from module starting to ending event to the current module
          event_list[[module_code]] <- slice(full_event_list, c(first_module_event:last_module_event))

          # Adjusting the run times to be module specific
          event_list[[module_code]]$module_time <- event_list[[module_code]]$module_time - event_list[[module_code]]$module_time[1]
        } else {
          event_list[[module_code]] <- NULL
        }

      }
    }

  event_list[["scenario_elements"]] <- scenario_elements

  return(event_list)
}
globalVariables(c("timestamp", "eventType", "index", "invitationId", "surveyId",
                  "event_type", "data", "invitation_id", "survey_id", "value",
                  "binaryFileId", "spreadsheetId", "fileId", "emailId",
                  "element_code", "tool", "mimeType", "module_id", "questionId",
                  "answerId", "name", "data2", "binary_file_id", "email_id",
                  "spreadsheet_id", "file_id", "questionnaireId"))


###############################################################################
### Helper Functions

#' Inserts event that are marked as idle
#'
#' Takes the event data from a single participation and returns the data
#' including additional events that are marked as idle when the participant was
#' not active for more than `idle_time` seconds.
#'
#' @param workflow A list including the event data
#' @param idle_time Numeric describing after how many seconds an event is
#'   considered as idle.
#'
#' @return An event list including additional idle events
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
insert_idle_events <- function (workflow, idle_time=5) {

  idle_events <- workflow %>%
    # helper variable to chek if the event_code is the same as the previous one
    dplyr::filter(event_duration>idle_time) %>%
    dplyr::mutate(project_time=project_time+idle_time) %>%
    dplyr::mutate(event_code=paste0(substr(event_code, 1,6),"_IDL")) %>%
    dplyr::mutate(label=paste0("Idled ", label))

  result_workflow <- rbind(workflow, idle_events) %>%
    # Sort enlarged event list according to time
    arrange(time) %>%
    # Calculation of the corrected event durations after inserting the idle events
    dplyr::mutate(event_duration=dplyr::lead(project_time)-project_time)


  return(result_workflow)
}


#' Aggregate event data from a single participation
#'
#' Takes event data from a single participation and returns an aggregated form,
#' where events with identical codes that occur directly after each other are
#' aggregated to a single event. The duration values are correspondingly adjusted,
#' and for each aggregated event an intensity is calculated that describes how
#' many events occurred in the original form.
#'
#' @param event_list A list including the event data
#'
#' @return A list including the aggregated event data
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr select
#'
aggregate_duplicates <- function (event_list) {

  aggregated_event_list <- event_list %>%
    # helper variable to check if the event_code is the same as the previous one
    dplyr::mutate(previous_event_code=dplyr::lag(event_code)) %>%

    # helper variable to later calculate the intensity (i.e. how often an event occurred)
    dplyr::mutate(event_no=1:n()) %>%

    # only keep those case where the current workflow code is different from the previous
    dplyr::filter(event_code!=previous_event_code) %>%

    # calculation of the activity duration after summarizing events with identical workflows codes occurring directly after each other
    dplyr::mutate(event_duration=project_time-dplyr::lag(project_time)) %>%

    # calculation  of the intensity (i.e. how how often the summarized events occurred in the original data set directly after each other)
    dplyr::mutate(intensity=dplyr::lead(event_no)-event_no) %>%

    # Deletion of temporary variables
    dplyr::select(!previous_event_code, !event_no)

  return(aggregated_event_list)
}
globalVariables(c("event_code", "previous_event_code", "project_time",
                  "event_no", "time", "event_duration", "label", "intensity",
                  "usage_type"))


