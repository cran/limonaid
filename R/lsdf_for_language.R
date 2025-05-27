#' Produce the dataframe containing the survey for one language
#'
#' This is used when exporting surveys to LimeSurvey's TSV format.
#'
#' @param language The language for which to produce the data frame.
#' @param groups The groups object in the Survey object.
#' @param exportGroupIdMapping,exportQuestionIdMapping Used to map Survey
#' object identifier onto the identifier model used in the LimeSurvey TSV.
#' @param backupLanguage The language to get content from if not available in
#' the primary language
#' @param silent Whether to be silent or chatty.
#'
#' @return Invisibly, the `Survey` object.
#' @export
lsdf_for_language <- function(language,
                              groups,
                              exportGroupIdMapping,
                              exportQuestionIdMapping,
                              backupLanguage,
                              silent = limonaid::opts$get("silent")) {

  currentLanguage <- language;

  dat <- data.frame();

  ###---------------------------------------------------------------------------
  ### Loop through groups
  ###---------------------------------------------------------------------------

  for (currentGroup in seq_along(groups)) {

    ### Check whether this group already has a new, 'remapped'
    ### numeric identifier for exporting. If not, create it.
    if (!(groups[[currentGroup]]$id
          %in%
          names(exportGroupIdMapping))) {
      if (length(exportGroupIdMapping) == 0) {
        exportGroupIdMapping <- 1;
      } else {
        exportGroupIdMapping <-
          c(exportGroupIdMapping,
            max(exportGroupIdMapping) + 1);
      }
      names(exportGroupIdMapping)[length(exportGroupIdMapping)] <-
        groups[[currentGroup]]$id;
    }

    ### Then assign this new identifier
    currentGroupId <-
      exportGroupIdMapping[groups[[currentGroup]]$id];

    ### For values unspecified for this language, get the value
    ### from the primary language
    curLang_surveyTitle <-
      ifelse(currentLanguage %in% names(groups[[currentGroup]]$titles) &&
               (nchar(trimws(groups[[currentGroup]]$titles[[currentLanguage]])) > 0),
             groups[[currentGroup]]$titles[[currentLanguage]],
             groups[[currentGroup]]$titles[[backupLanguage]]);
    curLang_surveyDescription <-
      ifelse(currentLanguage %in% names(groups[[currentGroup]]$descriptions) &&
               (nchar(trimws(groups[[currentGroup]]$descriptions[[currentLanguage]])) > 0),
             groups[[currentGroup]]$descriptions[[currentLanguage]],
             groups[[currentGroup]]$descriptions[[backupLanguage]]);

    if (!silent) {
      cat0("\n  Processing group: ", curLang_surveyTitle, "\n");
    }

    newRow <-
      data.frame(
        id = unname(currentGroupId),
        related_id = "",
        class="G",
        type.scale = "0",
        name = unname(curLang_surveyTitle),
        relevance = unname(groups[[currentGroup]]$relevance),
        text = unname(curLang_surveyDescription),
        help = "",
        language = unname(currentLanguage),
        validation = "",
        mandatory = "",
        other = "",
        default = "",
        same_default = "",
        random_group = unname(groups[[currentGroup]]$random_group),
        stringsAsFactors = FALSE
      );

    ### Add row using our homerolled version of plyr::rbind.fill
    dat <- append_lsdf_rows(dat, newRow);

    ###-------------------------------------------------------------------
    ### Loop through questions
    ###-------------------------------------------------------------------

    for (currentQuestionIndex in
         seq_along(groups[[currentGroup]]$questions)) {

      convenienceQ <-
        groups[[currentGroup]]$questions[[currentQuestionIndex]];

      ### Check whether this question already has a new, 'remapped'
      ### numeric identifier for exporting. If not, create it.
      uniqueQuestionCodeId <- convenienceQ$code;

      if (!(uniqueQuestionCodeId
            %in%
            names(exportQuestionIdMapping))) {
        if (length(exportQuestionIdMapping) == 0) {
          exportQuestionIdMapping <- 1;
        } else {
          exportQuestionIdMapping <-
            c(exportQuestionIdMapping,
              max(exportQuestionIdMapping) + 1);
        }
        names(exportQuestionIdMapping)[length(exportQuestionIdMapping)] <-
          uniqueQuestionCodeId;
      }

      ### Then assign this new identifier
      currentQuestionId <-
        exportQuestionIdMapping[uniqueQuestionCodeId];

      ### For values unspecified for this language, get the value
      ### from the primary language
      curLang_questionText <-
        ifelse((currentLanguage %in% names(convenienceQ$questionTexts)) &&
                 (nchar(trimws(convenienceQ$questionTexts[[currentLanguage]])) > 0),
               convenienceQ$questionTexts[[currentLanguage]],
               convenienceQ$questionTexts[[backupLanguage]]);

      if (limonaid::opts$get("debug")) {
        if ((currentLanguage %in% names(convenienceQ$questionTexts)) &&
            (nchar(trimws(convenienceQ$questionTexts[[currentLanguage]])) > 0)) {
          msg <-
            paste0(
              "\n    For question `", convenienceQ$code,
              "`, found question text in language `", currentLanguage,
              "` and used that."
            );
        } else {
          msg <-
            paste0(
              "\n    For question `", convenienceQ$code,
              "`, did NOT find question text in language `", currentLanguage,
              "`, so used question text from the backup language instead (`",
              backupLanguage, "`)."
            );
        }
        cat(msg);
      }

      curLang_questionHelp <-
        ifelse(currentLanguage %in% names(convenienceQ$helpTexts) &&
                 (nchar(trimws(convenienceQ$helpTexts[[currentLanguage]])) > 0),
               convenienceQ$helpTexts[[currentLanguage]],
               convenienceQ$helpTexts[[backupLanguage]]);
      curLang_otherReplaceText <-
        ifelse(currentLanguage %in% names(convenienceQ$otherReplaceTexts) &&
                 (nchar(trimws(convenienceQ$otherReplaceTexts[[currentLanguage]])) > 0),
               convenienceQ$otherReplaceTexts[[currentLanguage]],
               convenienceQ$otherReplaceTexts[[backupLanguage]]);

      ### Specify this new row
      newRow <-
        data.frame(
          id = unname(currentQuestionId),
          related_id = "",
          class="Q",
          type.scale = unname(convenienceQ$lsType),
          name = unname(convenienceQ$code),
          relevance = unname(convenienceQ$relevance),
          text = unname(curLang_questionText),
          help = unname(curLang_questionHelp),
          other_replace_text = unname(curLang_otherReplaceText),
          language = unname(currentLanguage),
          validation = unname(convenienceQ$validation),
          mandatory = unname(convenienceQ$mandatory),
          other = unname(convenienceQ$other),
          default = unname(convenienceQ$default),
          same_default = unname(convenienceQ$same_default),
          stringsAsFactors = FALSE
        );

      ### Add row using our homerolled version of plyr::rbind.fill
      dat <- append_lsdf_rows(dat, newRow);

      ### Set additional options for this question
      dat[nrow(dat), "array_filter"] <- convenienceQ$array_filter;
      dat[nrow(dat), "cssclass"] <- convenienceQ$cssclass;
      dat[nrow(dat), "hide_tip"] <- convenienceQ$hide_tip;
      if (length(convenienceQ$otherOptions) > 0) {
        otherOptionLengths <- unlist(lapply(convenienceQ$otherOptions,
                                            length));
        if (any(otherOptionLengths > 1)) {
          cat0("\nQuestion with code '", convenienceQ$code,
               "' has at least one 'other option' with length > 1: ",
               vecTxtQ(names(convenienceQ$otherOptions)[otherOptionLengths > 1]),
               ". That may not be a problem (e.g. for multilingual optiosn, ",
               "which are processed separately), but those options will ",
               "not be processed here.\n");
        }
        otherOptionsToProcess <-
          convenienceQ$otherOptions[otherOptionLengths==1];
        dat[nrow(dat), names(otherOptionsToProcess)] <-
          otherOptionsToProcess;
      }

      ###-----------------------------------------------------------------
      ### Work some question-type-specific magic
      ###-----------------------------------------------------------------

      if (convenienceQ$lsType == "M") {
        ### For multiple-choice questions, the options are stored as
        ### subquestions, not as answer options.
        if ((!is.null(convenienceQ$answerOptions)) &&
            (length(convenienceQ$answerOptions) > 0) &&
            (length(convenienceQ$subquestions) == 0)) {
          convenienceQ$subquestions <-
            lapply(
              convenienceQ$answerOptions,
              function(x) {
                return(
                  list(
                    code = x$code,
                    subquestionTexts = x$optionTexts,
                    relevance = x$relevance,
                    type.scale = x$type.scale,
                    helpTexts = stats::setNames(rep("",
                                                    length(x$optionTexts)),
                                                nm = names(x$optionTexts)),
                    validation = "",
                    mandatory = "",
                    default = "",
                    same_default = ""
                  )
                );
              }
            );
          names(convenienceQ$subquestions) <-
            names(convenienceQ$answerOptions);
          convenienceQ$answerOptions <- NULL;
        }
      }

      ###-----------------------------------------------------------------
      ### Loop through subquestions
      ###-----------------------------------------------------------------

      if (!is.null(convenienceQ$subquestions)) {
        for (currentSubquestionIndex in
             seq_along(convenienceQ$subquestions)) {

          convenienceSQ <-
            convenienceQ$subquestions[[currentSubquestionIndex]];

          ### Check whether this question already has a new, 'remapped'
          ### numeric identifier for exporting. If not, create it. Note
          ### that in this system, LimeSurvey numbers subquestions
          ### like questions.
          uniqueSubQuestionCodeId <-
            paste0(convenienceQ$code,
                   "_",
                   convenienceSQ$code);

          if (!(uniqueSubQuestionCodeId
                %in%
                names(exportQuestionIdMapping))) {
            exportQuestionIdMapping <-
              c(exportQuestionIdMapping,
                max(exportQuestionIdMapping) + 1);
            names(exportQuestionIdMapping)[length(exportQuestionIdMapping)] <-
              uniqueSubQuestionCodeId;
          }

          ### Then assign this new identifier
          currentSubQuestionId <-
            exportQuestionIdMapping[uniqueSubQuestionCodeId];

          ### Check and potentially correct type/scale
          typeScale <- convenienceSQ$type.scale;
          if (!(convenienceSQ$type.scale %in% 0:1)) {
            warning("The type/scale (`type.scale`) for subquestion ",
                    "with code '", convenienceSQ$code, "' in question ",
                    "with code '", convenienceQ$code, "' is not 0 or ",
                    "1, but ", typeScale,
                    ". I'm setting it to 0 while saving.");
            typeScale <- 0;
          }

          ### For values unspecified for this language, get the value
          ### from the primary language
          curLang_subquestionText <-
            ifelse(currentLanguage %in% names(convenienceSQ$subquestionTexts) &&
                     (nchar(trimws(convenienceSQ$subquestionTexts[[currentLanguage]])) > 0),
                   convenienceSQ$subquestionTexts[[currentLanguage]],
                   convenienceSQ$subquestionTexts[[backupLanguage]]);
          curLang_subquestionHelp <-
            ifelse(currentLanguage %in% names(convenienceSQ$helpTexts) &&
                     (nchar(trimws(convenienceSQ$helpTexts[[currentLanguage]])) > 0),
                   convenienceSQ$helpTexts[[currentLanguage]],
                   convenienceSQ$helpTexts[[backupLanguage]]);

          ### Specify this new row
          newRow <-
            data.frame(
              id = unname(currentSubQuestionId),
              related_id = "",
              class="SQ",
              type.scale = unname(typeScale),
              name = unname(convenienceSQ$code),
              relevance = unname(convenienceSQ$relevance),
              text = unname(curLang_subquestionText),
              help = unname(curLang_subquestionHelp),
              language = unname(currentLanguage),
              validation = unname(convenienceSQ$validation),
              mandatory = unname(convenienceSQ$mandatory),
              other = "",
              default = unname(convenienceSQ$default),
              same_default = unname(convenienceSQ$same_default),
              stringsAsFactors = FALSE
            );

          ### Add row using our homerolled version of plyr::rbind.fill
          dat <- append_lsdf_rows(dat, newRow);

        }
      }

      ###-----------------------------------------------------------------
      ### Loop through answer options
      ###-----------------------------------------------------------------

      if (!is.null(convenienceQ$answerOptions)) {
        for (currentAnswerOptionIndex in seq_along(convenienceQ$answerOptions)) {

          convenienceA <-
            convenienceQ$answerOptions[[currentAnswerOptionIndex]];

          typeScale <- convenienceA$type.scale;
          if (!(convenienceA$type.scale %in% 0:1)) {
            warning("The type/scale (`type.scale`) for answer option ",
                    "with code '", convenienceA$code, "' in question ",
                    "with code '", convenienceQ$code, "' is not 0 or ",
                    "1, but ", typeScale,
                    ". I'm setting it to 0 while saving.");
            typeScale <- 0;
          }

          ### For values unspecified for this language, get the value
          ### from the primary language
          curLang_optionText <-
            ifelse(currentLanguage %in% names(convenienceA$optionTexts) &&
                     (nchar(convenienceA$optionTexts[[currentLanguage]]) > 0),
                   convenienceA$optionTexts[[currentLanguage]],
                   convenienceA$optionTexts[[backupLanguage]]);

          ### Specify this new row
          newRow <-
            data.frame(
              id = unname(currentQuestionId),  ### Id of Q, not of A!
              related_id = "",
              class="A",
              type.scale = unname(typeScale),
              name = unname(convenienceA$code),
              relevance = unname(convenienceA$relevance),
              text = unname(curLang_optionText),
              help = "",
              language = unname(currentLanguage),
              validation = "",
              mandatory = "",
              other = "",
              default = "",
              same_default = "",
              stringsAsFactors = FALSE
            );

          ### Add row using our homerolled version of plyr::rbind.fill
          dat <- append_lsdf_rows(dat, newRow);

        }
      }

    }
  }

  return(list(dat = dat,
              exportGroupIdMapping = exportGroupIdMapping,
              exportQuestionIdMapping = exportQuestionIdMapping));

}

