#' R6 Class representing a LimeSurvey question
#'
#' A question has at least a code and a primary language.
#'
#' @export
Question <- R6::R6Class(
  "Question",

  ###---------------------------------------------------------------------------
  ### Public properties & methods
  ###---------------------------------------------------------------------------

  public = list(

    ###-------------------------------------------------------------------------
    ### Public properties
    ###-------------------------------------------------------------------------

    #' @field code The code of the question.
    code = NULL,

    #' @field id The identifier of the question (a unique number in a survey).
    id = NULL,

    #' @field gid The identifier of the group to which this question belongs.
    gid = NULL,

    #' @field sid The identifier of the survey to which this question belongs.
    sid = NULL,

    #' @field type The question type.
    type = NULL,

    #' @field lsType The question type in LimeSurvey's format.
    lsType = NULL,

    #' @field questionTexts The question text(s) in all languages.
    questionTexts = NULL,

    #' @field helpTexts The question help text(s) in all languages.
    helpTexts = NULL,

    #' @field relevance The relevance.
    relevance = 1,

    #' @field validation The question's validation.
    validation = "",

    #' @field language The primary language of the question.
    language = NULL,

    #' @field additional_languages Any additional languages for the title and description elements.
    additional_languages = "",

    #' @field answerOptions The answer options in the question.
    answerOptions = list(),

    #' @field subquestions The subquestions in the question.
    subquestions = list(),

    #' @field parent_qid The question identifier of the parent question (or 0).
    parent_qid = 0,

    #' @field mandatory Whether the question is mandatory (`Y` or `N`).
    mandatory = NULL,

    #' @field other Whether the question has an 'other' option (`Y` or `N`).
    other = NULL,

    #' @field otherReplaceTexts If the question has an 'other' option, its
    #' label if the default label should be overwritten (multilingual).
    otherReplaceTexts = NULL,

    #' @field default The default value.
    default = NULL,

    #' @field same_default Not entirely sure what this does.
    same_default = NULL,

    #' @field array_filter The question code of the array filter question
    #' to apply.
    array_filter = NULL,

    #' @field question_order The question order (starts at 0)
    question_order = NULL,

    #' @field cssclass The CSS class(es) to apply to this question.
    cssclass = NULL,

    #' @field hide_tip Whether to hide the tip (`Y` or `N`).
    hide_tip = NULL,

    #' @field otherOptions Any additional options, stored as a named list
    #' by assigning `as.list(...)`.
    otherOptions = NULL,

    ###-------------------------------------------------------------------------
    ### Initialization
    ###-------------------------------------------------------------------------

    #' @description
    #' Create a new question object. Most of this text comes directly
    #' from the TSV manual page at
    #' <https://www.limesurvey.org/manual/Tab_Separated_Value_survey_structure>, so
    #' please see that page for more details.
    #' @details The human-readable question types are (with some additional
    #' variants also being valid, in any case the literal labels used at
    #' <https://www.limesurvey.org/manual/Question_object_types#Current_question_types>):
    #'
    #' - "`array dual scale`"
    #' - "`5 point choice`"
    #' - "`5 point array`"
    #' - "`10 point array`"
    #' - "`yes/no/uncertain array`"
    #' - "`date`"
    #' - "`increase/same/decrease array`"
    #' - "`array`" (this is the "`array (flexible labels)`" type)
    #' - "`gender`"
    #' - "`array by column`"
    #' - "`language switch`"
    #' - "`multiple numerical input`",
    #' - "`radio`" (this is the "`list`" type)
    #' - "`checkboxes`" (this is the "`multiple choice`" type)
    #' - "`numerical input`",
    #' - "`list with comment`"
    #' - "`multiple choice with comments`"
    #' - "`multiple short text`"
    #' - "`ranking`"
    #' - "`short text`"
    #' - "`long text`"
    #' - "`huge text`"
    #' - "`text display`"
    #' - "`yes/no`"
    #' - "`multiple texts array`",
    #' - "`multiple dropdown array`"
    #' - "`file`"
    #' - "`dropdown`"
    #' - "`equation`".
    #' @param code The question code.
    #' @param type The human-readable question type (see details).
    #' @param lsType The type as LimeSurvey type ("`1`"; "`5`"; "`A`" to "`Y`",
    #' except "`J`", "`V`" and "`W`"; "`!`"; "`:`"; "`;`"; "`*`"; or "`|`" --see
    #' <https://www.limesurvey.org/manual/Question_object_types#Current_question_types>).
    #' @param id The identifier of the question (in a survey).
    #' @param gid The identifier of the group to which this question belongs.
    #' @param sid The identifier of the survey to which this question belongs.
    #' @param questionTexts The question text(s).
    #' @param helpTexts The help text(s).
    #' @param relevance The question's relevance equation.
    #' @param validation The question's validation.
    #' @param mandatory Whether the question is mandatory (`Y` or `N`);.
    #' @param other Whether the question has an 'other' option (`Y` or `N`).
    #' @param otherReplaceTexts If the question has an 'other' option, its
    #' label if the default label should be overwritten (multilingual).
    #' @param default The default value.
    #' @param same_default `Y` for true, in which case any default value set
    #' for the primary language applies to other languages.
    #' @param parent_qid The question identifier of the parent question (or 0).
    #' @param array_filter The question code of the array filter question
    #' to apply.
    #' @param cssclass The CSS class(es) to apply to this question.
    #' @param hide_tip Whether to hide the tip (`Y` or `N`).
    #' @param language The question's primary language.
    #' @param additional_languages Any additional languages
    #' @param new_id_fun A function to set identifiers (for XML exports, which
    #' mirrors MySQL tables and so needs identifiers). By default, new question
    #' objects receive this function from the group containing them; and groups
    #' receive it from the survey containing them. This ensures that identifiers
    #' are always unique in a survey (despite question objects not being able
    #' to 'see' anything in the group containing them, and group objects not
    #' being able to 'see' anything in the survey containing them; because they
    #' 'received' this function from the parent object, and it 'bubbles down'
    #' through groups to the questions, those functions still get and set a
    #' private identifier property in the 'top-most' object).
    #' @param question_order The question order (starts at 0)
    #' @param ... Any additional options, stored as a named list in the
    #' `otherOptions` property by assigning `as.list(...)`.
    #' @return A new `Question` object.
    initialize = function(code,
                          type = NULL,
                          lsType = NULL,
                          id = NULL,
                          gid = NULL,
                          sid = NULL,
                          questionTexts = "",
                          helpTexts = "",
                          relevance = 1,
                          validation = "",
                          mandatory = "N",
                          parent_qid = 0,
                          other = "N",
                          otherReplaceTexts = "",
                          default = "",
                          same_default = "0",
                          array_filter = "",
                          cssclass = "",
                          hide_tip = "",
                          language = "en",
                          additional_languages = "",
                          new_id_fun = NULL,
                          question_order = 0,
                          ...) {

      ###-----------------------------------------------------------------------
      ### Check whether the multilingual fields have been passed properly
      ###-----------------------------------------------------------------------

      questionTexts <-
        checkMultilingualFields(questionTexts,
                                language = language);

      helpTexts <-
        checkMultilingualFields(helpTexts,
                                language = language);

      otherReplaceTexts <-
        checkMultilingualFields(otherReplaceTexts,
                                language = language);

      ###-----------------------------------------------------------------------
      ### Set identifier function
      ###-----------------------------------------------------------------------

      if (is.null(new_id_fun)) {
        private$new_id <- function() {
          private$idCounter <-
            private$idCounter + 1;
          return(private$idCounter);
        }
      } else {
        private$new_id <- new_id_fun;
      }

      ###-----------------------------------------------------------------------
      ### Check question type
      ###-----------------------------------------------------------------------

      type <- trimws(tolower(type));

      if (is.null(lsType)) {
        if (type %in% c("array dual scale",
                        "array (dual scale)")) {
          lsType <- "1";

        } else if (type %in% c("5 point choice")) {
          lsType <- "5";

        } else if (type %in% c("5 point array",
                               "array (5 point choice)")) {
          lsType <- "A";

        } else if (type %in% c("10 point array",
                               "array (10 point choice)")) {
          lsType <- "B";

        } else if (type %in% c("yes, no, uncertain array",
                               "yes/no/uncertain array",
                               "array (yes/no/uncertain)")) {
          lsType <- "C";

        } else if (type %in% c("date")) {
          lsType <- "D";

        } else if (type %in% c("increase, same, decrease array",
                               "increase/same/decrease array",
                               "array (increase/same/decrease)")) {
          lsType <- "E";

        } else if (type %in% c("array",
                               "array (flexible labels)")) {
          lsType <- "F";

        } else if (type %in% c("gender")) {
          lsType <- "G";

        } else if (type %in% c("array by column",
                               "array (flexible labels) by column")) {
          lsType <- "H";

        } else if (type %in% c("language switch", "language")) {
          lsType <- "I";

        } else if (type %in% c("multiple numerical input", "slider")) {
          lsType <- "K";

        } else if (type %in% c("list", "radio", "radiobuttons", "radio buttons")) {
          lsType <- "L";

        } else if (type %in% c("multiple choice", "checkboxes", "checkbox")) {
          lsType <- "M";

        } else if (type %in% c("numerical input")) {
          lsType <- "N";

        } else if (type %in% c("list with comment",
                               "list (with comment)")) {
          lsType <- "O";

        } else if (type %in% c("multiple choice with comments",
                               "multiple choice (with comments)")) {
          lsType <- "P";

        } else if (type %in% c("multiple short text",
                               "short text (multiple)")) {
          lsType <- "Q";

        } else if (type %in% c("ranking")) {
          lsType <- "R";

        } else if (type %in% c("short free text",
                               "free text (short)",
                               "text (short)",
                               "short text")) {
          lsType <- "S";

        } else if (type %in% c("long free text",
                               "free text (long)",
                               "text (long)",
                               "long text")) {
          lsType <- "T";

        } else if (type %in% c("huge free text",
                               "free text (huge)",
                               "text (huge)",
                               "huge text")) {
          lsType <- "U";

        } else if (type %in% c("boilerplate", "text display")) {
          lsType <- "X";

        } else if (type %in% c("yes/no",
                               "yes, no")) {
          lsType <- "Y";

        } else if (type %in% c("array (flexible labels) multiple texts",
                               "multiple texts array",
                               "multiple texts array")) {
          lsType <- ":";

        } else if (type %in% c("array (flexible labels) multiple drop down",
                               "multiple drop down array",
                               "multiple dropdown array")) {
          lsType <- ":";

        } else if (type %in% c("file",
                               "file upload",
                               "upload")) {
          lsType <- "|";

        } else if (type %in% c("dropdown",
                               "list (dropdown)")) {
          lsType <- "!";

        } else if (type %in% c("equation")) {
          lsType <- "*";

        } else {
          stop("Questions of type '", type, "' are not yet supported.");
        }
      } else {
        type <- lsType;
        ### Optionally check lsType against valid values
      }

      ###-----------------------------------------------------------------------
      ### Set all other arguments as 'other options'
      ###-----------------------------------------------------------------------

      otherOptions <- list(...);

      ###-----------------------------------------------------------------------
      ### Check question type and set additional settings
      ###-----------------------------------------------------------------------

      if (type == "slider") {
        otherOptions$slider_layout <- 1;
        if (!"slider_accuracy" %in% names(otherOptions))
          otherOptions$slider_accuracy <- 1;
        if (!"slider_handle" %in% names(otherOptions))
          otherOptions$slider_handle <- 2;
      }

      ###
      ###
      ### Ideally, implement validation of all options
      ###
      ###

      ###-----------------------------------------------------------------------
      ### Set fields
      ###-----------------------------------------------------------------------

      self$id <- id;

      if (is.null(gid)) {
        self$gid <- private$new_id();
      } else {
        self$gid <- gid;
      }

      if (is.null(sid)) {
        self$sid <- private$new_id();
      } else {
        self$sid <- sid;
      }

      self$code <- code;
      self$type <- type;
      self$lsType <- lsType;

      self$question_order <- question_order;

      self$language <- language;
      self$additional_languages <- additional_languages;

      self$questionTexts <- questionTexts;
      self$helpTexts <- helpTexts;

      self$parent_qid <- parent_qid;

      self$relevance <- relevance;
      self$validation <- validation;
      self$mandatory <- mandatory;
      self$other <- other;

      self$otherReplaceTexts <- otherReplaceTexts;

      self$default <- default;
      self$same_default <- same_default;
      self$array_filter <- array_filter;
      self$cssclass <- cssclass;
      self$hide_tip <- hide_tip;

      self$otherOptions <- otherOptions;

    },

    ###-------------------------------------------------------------------------
    ### Add an answer option
    ###-------------------------------------------------------------------------

    #' @description
    #' Add an answer option to a question. Most of this text comes directly
    #' from the TSV manual page at
    #' https://www.limesurvey.org/manual/Tab_Separated_Value_survey_structure, so
    #' please see that page for more details.
    #' @param code The answer option code.
    #' @param optionTexts The answer option text(s).
    #' @param type.scale `0` or `1` (e.g. for dual-scale; 'scale_id').
    #' @param relevance The answer option's relevance equation.
    #' @param assessment.value If using assessment, this is the assessment
    #' value for the answer ('assessment_value').
    #' @param sort.order The sort order (to manually specify); starts at 0. If
    #' left empty, new options are added at the bottom.
    #' @return Invisibly, the question object.
    add_answer_option = function(code,
                                 optionTexts,
                                 type.scale = 0,
                                 relevance = "",
                                 assessment.value = 0,
                                 sort.order = NULL) {

      ###-----------------------------------------------------------------------
      ### Check code
      ###-----------------------------------------------------------------------

      if (!grepl("^[a-zA-Z0-9]*$",
                 code)) {
        stop("Answer option codes may only consist of digits and ",
             "letters!");
      }

      ###-----------------------------------------------------------------------
      ### Check whether the multilingual fields have been passed properly
      ###-----------------------------------------------------------------------

      optionTexts <-
        checkMultilingualFields(optionTexts,
                                language = self$language);

      ###-----------------------------------------------------------------------
      ### Set the sort order
      ###-----------------------------------------------------------------------

      if (is.null(sort.order)) {
        if (length(self$answerOptions) == 0) {
          sort.order <- 0;
        } else {
          sort.order <- max(unlist(lapply(self$answerOptions, \(x) x$sort.order))) + 1;
        }
      }

      ###-----------------------------------------------------------------------
      ### Set the answer options
      ###-----------------------------------------------------------------------

      self$answerOptions <-
        c(self$answerOptions,
          list(list(id = private$new_id(),
                    code = code,
                    optionTexts = optionTexts,
                    type.scale = type.scale,
                    relevance = relevance,
                    assessment.value = assessment.value,
                    sort.order = sort.order)));

      ### Set name of this new option
      names(self$answerOptions)[
        length(self$answerOptions)] <- code;

      ### Return self invisibly
      return(invisible(self));
    },

    ###-------------------------------------------------------------------------
    ### Add a subquestion
    ###-------------------------------------------------------------------------

    #' @description
    #' Add a subquestion to a question. Most of this text comes directly from
    #' the TSV manual page at
    #' https://www.limesurvey.org/manual/Tab_Separated_Value_survey_structure, so
    #' please see that page for more details.
    #' @param code The subquestions code.
    #' @param subquestionTexts The subquestion text(s).
    #' @param relevance When to show this subquestion.
    #' @param helpTexts As far as I know not yet implemented in LimeSurvey;
    #' but the TSV help page says "(Future) to support subquestion-level help".
    #' @param type.scale `0` or `1`, depending upon question type (e.g. array
    #' text will have two scales)0 or 1, depending upon question type (e.g.
    #' array text will have two scales)."
    #' @param validation As far as I know not yet implemented in LimeSurvey;
    #' but the TSV help page says "(Future) to support subquestion-level
    #' regular expression validation (e.g. for address parts)"
    #' @param mandatory As far as I know not yet implemented in LimeSurvey;
    #' but the TSV help page says "(Future) to support subquestion-level
    #' mandatory (e.g. make only a few subquestions mandatory)"
    #' @param default If set, then this is the default value for the
    #' subquestion (inserted into defaultvalues table).
    #' @param same_default If set, then the default for the primary language
    #' is used for all other languages.
    #' @param subquestion.order The subquestion order (to manually specify);
    #' starts at 0. If left empty, new options are added at the bottom.
    #' @return Invisibly, the question object.
    add_subquestion = function(code,
                               subquestionTexts,
                               relevance = "",
                               helpTexts = NULL,
                               type.scale = 0,
                               validation = "",
                               mandatory = "",
                               default = "",
                               same_default = "",
                               subquestion.order = NULL) {

      ###-----------------------------------------------------------------------
      ### Check code
      ###-----------------------------------------------------------------------

      if (!grepl("^[a-zA-Z0-9]*$",
                 code)) {
        stop("Subquestion codes may only consist of digits and ",
             "letters!");
      }

      ###-----------------------------------------------------------------------
      ### Check whether the multilingual fields have been passed properly
      ###-----------------------------------------------------------------------

      subquestionTexts <-
        checkMultilingualFields(subquestionTexts,
                                language = self$language);

      if (!is.null(helpTexts)) {
        helpTexts <-
          checkMultilingualFields(helpTexts,
                                  language = self$language);
      } else {
        helpTexts <- stats::setNames(rep("", length(subquestionTexts)),
                                     nm = names(subquestionTexts));
      }

      ###-----------------------------------------------------------------------
      ### Set the subquestion order
      ###-----------------------------------------------------------------------

      if (is.null(subquestion.order)) {
        if (length(self$subquestions) == 0) {
          subquestion.order <- 0;
        } else {
          subquestion.order <- max(unlist(lapply(self$subquestions, \(x) x$subquestion.order))) + 1;
        }
      }

      ###-----------------------------------------------------------------------
      ### Set the subquestions
      ###-----------------------------------------------------------------------

      self$subquestions <-
        c(self$subquestions,
          list(list(code = code,
                    subquestionTexts = subquestionTexts,
                    relevance = relevance,
                    type.scale = type.scale,
                    helpTexts = helpTexts,
                    validation = validation,
                    mandatory = mandatory,
                    default = default,
                    same_default = same_default,
                    subquestion.order = subquestion.order,
                    id = private$new_id())));

      ### Set name of this new subquestion
      names(self$subquestions)[
        length(self$subquestions)] <- code;

      ### Return self invisibly
      return(invisible(self));
    },

    ###-------------------------------------------------------------------------
    ### Export question row in XML format
    ###
    ### Note: question text and other language stuff is in question_l10ns rows,
    ### not the question rows
    ###
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the question in XML format (for lss, lsg, or lsq files).
    #' @param silent Whether to be silent or chatty.
    #' @return The produced XML
    xmlExport_row_question = function(silent = limonaid::opts$get("silent")) {

      return(
        xmlNode_from_vector(
          "row",
          subNames = ls_xmlFields$questions,
          c(self$id,
            na_if_null(self$parent_qid),
            self$sid,
            self$gid,
            self$lsType,
            self$code, ### "title"
            na_if_null(self$preg),
            na_if_null(self$other),
            na_if_null(self$mandatory),
            self$question_order,
            na_if_null(self$scale_id),
            na_if_null(self$same_default),
            na_if_null(self$relevance),
            na_if_null(self$modulename),
            na_if_null(self$encrypted),
            na_if_null(self$question_theme_name),
            na_if_null(self$same_script)
          ),
          cdata = TRUE
        )
      );

    },

    ###-------------------------------------------------------------------------
    ### Export subquestion row in XML format
    ###
    ### Note: question text and other language stuff is in question_l10ns rows,
    ### not the subquestion rows
    ###
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the question in XML format (for lss, lsg, or lsq files).
    #' @param silent Whether to be silent or chatty.
    #' @param returnRows Whether to return a list with each row as element,
    #' or a `rows` node (as `xml2` object) containing each row as nodes
    #' @return The produced XML
    xmlExport_row_subquestions = function(returnRows = FALSE,
                                          silent = limonaid::opts$get("silent")) {

      if (!self$has_subquestions) {

        return(NULL);

      } else {

        listOfRows <-
          lapply(
            self$subquestions,
            function(currentSubQuestion) {
              return(
                xmlNode_from_vector(
                  "row",
                  subNames = ls_xmlFields$subquestions,
                  c(currentSubQuestion$id,
                    self$id,
                    self$sid,
                    self$gid,
                    na_if_null(currentSubQuestion$type),
                    currentSubQuestion$code, ### "title"
                    na_if_null(currentSubQuestion$preg),
                    na_if_null(currentSubQuestion$other),
                    na_if_null(currentSubQuestion$mandatory),
                    na_if_null(currentSubQuestion$question_order),
                    na_if_null(currentSubQuestion$type), # scale_id
                    na_if_null(currentSubQuestion$same_default),
                    na_if_null(currentSubQuestion$relevance),
                    na_if_null(currentSubQuestion$modulename),
                    na_if_null(self$encrypted),
                    na_if_null(self$question_theme_name),
                    na_if_null(currentSubQuestion$same_script)
                  ),
                  cdata = TRUE
                )
              )
            }
          );

        if (returnRows) {

          xml <- xml2::xml_new_root("rows");

          for (i in seq_along(listOfRows)) {

            xml2::xml_add_child(
              xml,
              listOfRows[[i]]
            );

          }

          return(xml);

        } else {

          return(listOfRows);

        }

      }

    },

    ###-------------------------------------------------------------------------
    ### Export question_l10ns info in a list
    ###
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the question's question_l10ns info in a list of XML nodes.
    #' @param id_fun The function to use to produce unique identifiers
    #' @param silent Whether to be silent or chatty.
    #' @return The produced list of XML nodes
    xmlExport_row_question_l10ns = function(id_fun = private$new_id(),
                                            silent = limonaid::opts$get("silent")) {

      ###-----------------------------------------------------------------------
      ### Configure language list
      ###-----------------------------------------------------------------------

      if ((length(self$additional_languages) > 0) &&
          nchar(self$additional_languages[1]) > 0) {
        ### We have multiple languages
        languageList <-
          c(self$language,
            self$additional_languages);
      } else {
        ### Only one language
        languageList <- self$language;
      }

      ###-----------------------------------------------------------------------
      ### Add rows for main question fields
      ###-----------------------------------------------------------------------

      listOfRows <-
        lapply(
          languageList,
          function(currentLanguage) {
            return(
              xmlNode_from_vector(
                "row",
                subNames = ls_xmlFields$question_l10ns,
                c(private$new_id(),
                  self$id,
                  self$questionTexts[[currentLanguage]],
                  na_if_null(self$helpTexts[[currentLanguage]]),
                  currentLanguage,
                  na_if_null(self$script)
                ),
                cdata = TRUE
              )
            )
          }
        );

      if (self$has_subquestions) {

        for (currentSubquestion in self$subquestions) {

          for (currentLanguage in languageList) {

            listOfRows <-
              c(listOfRows,
                list(
                  xmlNode_from_vector(
                    "row",
                    subNames = ls_xmlFields$question_l10ns,
                    c(id_fun(),
                      currentSubquestion$id,
                      currentSubquestion$subquestionTexts[[currentLanguage]],
                      na_if_null(currentSubquestion$helpTexts[[currentLanguage]]),
                      currentLanguage,
                      na_if_null(currentSubquestion$script)
                    ),
                    cdata = TRUE
                  )
                ));

          }

        }

      }

      return(listOfRows);

    },

    ###-------------------------------------------------------------------------
    ### Export answer row in XML format
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the answer options in XML format (for lss, lsg, or lsq files).
    #' @param silent Whether to be silent or chatty.
    #' @param returnRows Whether to return a list with each row as element,
    #' or a `rows` node (as `xml2` object) containing each row as nodes
    #' @return The produced XML
    xmlExport_row_answers = function(returnRows = FALSE,
                                     silent = limonaid::opts$get("silent")) {

      if (!self$has_answerOptions) {

        return(NULL);

      } else {

        listOfRows <-
          lapply(
            self$answerOptions,
            function(currentAnswerOption) {
              return(
                xmlNode_from_vector(
                  "row",
                  subNames = ls_xmlFields$answers,
                  c(currentAnswerOption$id, # aid
                    self$id, # qid
                    currentAnswerOption$code, # code
                    currentAnswerOption$sort.order, # sortorder
                    currentAnswerOption$assessment.value, # assessment_value
                    na_if_null(currentAnswerOption$type.scale) # scale_id
                  ),
                  cdata = TRUE
                )
              )
            }
          );

        if (returnRows) {

          xml <- xml2::xml_new_root("rows");

          for (i in seq_along(listOfRows)) {

            xml2::xml_add_child(
              xml,
              listOfRows[[i]]
            );

          }

          return(xml);

        } else {

          return(listOfRows);

        }

      }

    },

    ###-------------------------------------------------------------------------
    ### Export answer l10ns info in a list
    ###
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the question's answer optoin l10ns info in a list of XML nodes.
    #' @param id_fun The function to use to produce unique identifiers
    #' @param silent Whether to be silent or chatty.
    #' @return The produced list of XML nodes
    xmlExport_row_answer_l10ns = function(id_fun = private$new_id,
                                          silent = limonaid::opts$get("silent")) {

      ###-----------------------------------------------------------------------
      ### Configure language list
      ###-----------------------------------------------------------------------

      if ((length(self$additional_languages) > 0) &&
          nchar(self$additional_languages[1]) > 0) {
        ### We have multiple languages
        languageList <-
          c(self$language,
            self$additional_languages);
      } else {
        ### Only one language
        languageList <- self$language;
      }

      if (!self$has_answerOptions) {

        return(NULL);

      } else {

        listOfRows <- list();

        for (currentAnswerOption in self$answerOptions) {

          for (currentLanguage in languageList) {

            listOfRows <-
              c(listOfRows,
                list(
                  xmlNode_from_vector(
                    "row",
                    subNames = ls_xmlFields$answer_l10ns,
                    c(id_fun(),
                      currentAnswerOption$id,
                      currentAnswerOption$optionTexts[[currentLanguage]],
                      currentLanguage
                    ),
                    cdata = TRUE
                  )
                ));

          }

        }

      }

      return(listOfRows);

    },

    ###-------------------------------------------------------------------------
    ### Export question attributes in a list
    ###
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the question's attributes in a list of XML nodes.
    #'
    #' @param silent Whether to be silent or chatty.
    #' @return The produced list of XML nodes
    xmlExport_row_attributes = function(silent = limonaid::opts$get("silent")) {

      ###-----------------------------------------------------------------------
      ### Configure language list
      ###-----------------------------------------------------------------------

      if ((length(self$additional_languages) > 0) &&
          nchar(self$additional_languages[1]) > 0) {
        ### We have multiple languages
        languageList <-
          c(self$language,
            self$additional_languages);
      } else {
        ### Only one language
        languageList <- self$language;
      }

      if (self$lsType %in% names(ls_attributeDefaults)) {

        defaultAttributes <- list();

        for (currentLanguage in languageList) {

          defaultAttributes <-
            c(defaultAttributes,
              lapply(names(ls_attributeDefaults[[self$lsType]]), \(x)
                     c(
                       qid = self$id,
                       attribute = x,
                       value = ls_attributeDefaults[[self$lsType]][x],
                       language = ifelse(x %in% ls_attributesWithLanguage, currentLanguage, NA)
                     )));

        }

        listOfRows <-
          lapply(
            defaultAttributes,
            \(x)
              xmlNode_from_vector(
                "row",
                subNames = ls_xmlFields$question_attributes,
                x,
                cdata = TRUE
              )
          );

        return(listOfRows);

      } else {

        stop("I cannot create attibutes for questions with LimeSurvey question type '",
             self$lsType, "' yet.");

      }

    }


  ), ### End of public properties and methods

  ###---------------------------------------------------------------------------
  ### Active fields
  ###---------------------------------------------------------------------------

  active = list(

    #' @field has_subquestions Whether the question has subquestions.
    has_subquestions = function(value) {
      if (missing(value)) {
        if (length(self$subquestions) > 0) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        warning("You cannot assign anything to this field!");
      }
    },

    #' @field has_answerOptions Whether the question has answer options
    has_answerOptions = function(value) {
      if (missing(value)) {
        if (length(self$answerOptions) > 0) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        warning("You cannot assign anything to this field!");
      }
    }

  ), ### End of active fields

  ###---------------------------------------------------------------------------
  ### Private properties & methods
  ###---------------------------------------------------------------------------

  private = list(

    ### Unique numeric identifiers (for MySQL basically)
    idCounter = 0,

    ### This will be loaded with a function to return identifiers
    ### https://stackoverflow.com/questions/39914775/updating-method-definitions-in-r6-object-instance#51714770
    new_id = NULL

  ) ### End of private properties and methods


)
