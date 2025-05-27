#' R6 Class representing a LimeSurvey group
#'
#' A group is mostly just a container for questions.
#'
#' @examples
#' myGroup <- limonaid::Group$new(
#'   group_name = "My Group"
#' );
#' myGroup$add_question(
#'   "testQuestion1",
#'   questionTexts = "First question",
#'   type="free text (short)"
#' );
#' myGroup$add_question(
#'   "testQuestion2",
#'   questionTexts = "Second question",
#'   type="radio"
#' );
#' myGroup$questions$testQuestion2$add_answer_option(
#'   "option1",
#'   "First option"
#' );
#' myGroup$questions$testQuestion2$add_answer_option(
#'   "option2",
#'   "Second option"
#' );
#'
#' cat(as.character(myGroup$export_to_lsg()));
#'
#' @export
Group <- R6::R6Class(
  "Group",

  ###---------------------------------------------------------------------------
  ### Public properties & methods
  ###---------------------------------------------------------------------------

  public = list(

    ###-------------------------------------------------------------------------
    ### Public properties
    ###-------------------------------------------------------------------------

    #' @field group_name The group name / title / label
    group_name = "",

    #' @field description The group description
    description = "",

    #' @field grelevance The relevance equation for the group
    grelevance = 1,

    #' @field group_order The group order (in the survey)
    group_order = 1,

    #' @field randomization_group The randomization group (that the group is a part of)
    randomization_group = NULL,

    #' @field language The language of the group; or primary language, if there are multiple languages.
    language = NULL,

    #' @field additional_languages Any additional languages for the title and description elements.
    additional_languages = "",

    #' @field id The identifier of the group (a unique number in a survey)
    id = NULL,

    #' @field sid The identifier of the survey that this group belongs to
    sid = NULL,

    #' @field otherOptions Any additional options, stored as a named list
    #' by assigning `as.list(...)`.
    otherOptions = NULL,

    #' @field questions The questions in this group
    questions = list(),

    ###-------------------------------------------------------------------------
    ### Initialization
    ###-------------------------------------------------------------------------

    #' @description
    #' Create a new group object. Most of this text comes directly
    #' from the TSV manual page at
    #' <https://www.limesurvey.org/manual/Tab_Separated_Value_survey_structure>, so
    #' please see that page for more details.
    #' @param group_name The title of the group (if there are multiple languages,
    #' a named vector where every element is the title in another language and
    #' every element's name is the language code).
    #' @param description The description of the group (if there are multiple
    #' languages, a named vector where every element is the title in another
    #' language and every element's name is the language code).
    #' @param grelevance The group's relevance equation
    #' @param group_order The group order (if the group is part of a survey)
    #' @param randomization_group The group's randomization group
    #' @param language The group's only or primary language
    #' @param additional_languages Any additional languages
    #' @param id Optionally, the id of the group.
    #' @param sid Optionally, the identifier of the survey that this group
    #' belongs to.
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
    #' @param uqid A Unique Questionnaire Identifier (UQID) to import a
    #' questionnaire and populate the group with it.
    #' @param repo_url The URL to a repo serving the questionnaire with the
    #' UQID in JSON.
    #' @param ... Any additional options, stored as a named list in the
    #' `otherOptions` property by assigning `as.list(...)`.
    #' @return A new `Group` object.
    initialize = function(group_name = "",
                          description = "",
                          grelevance = 1,
                          group_order = 1,
                          randomization_group = NULL,
                          language = "en",
                          additional_languages = "",
                          id = NULL,
                          sid = NULL,
                          new_id_fun = NULL,
                          uqid = NULL,
                          repo_url = "https://operationalizations.com/questionnaires/json",
                          ...) {

      ###-----------------------------------------------------------------------
      ### When populating from a SOQ retrieved using a UQID, we only do that
      ###-----------------------------------------------------------------------

      if (!is.null(uqid)) {

        if (!requireNamespace("psyverse", quietly = TRUE)) {
          stop("You need the {psyverse} package to be able to import from ",
               "a questionnaire repo, and at least version 0.3.");
        }




      }

      ###-----------------------------------------------------------------------
      ### Check whether the multilingual fields have been passed properly
      ###-----------------------------------------------------------------------

      group_name <-
        checkMultilingualFields(group_name,
                                language = language);

      description <-
        checkMultilingualFields(description,
                                language = language);

      ###-----------------------------------------------------------------------
      ### Set all other arguments as 'other options'
      ###-----------------------------------------------------------------------

      otherOptions <- list(...);

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
      ### Set fields
      ###-----------------------------------------------------------------------

      if (is.null(sid)) {
        self$sid <- private$new_id();
      } else {
        self$sid <- sid;
      }

      if (is.null(id)) {
        self$id <- private$new_id();
      } else {
        self$id <- id;
      }

      if (is.null(group_order)) {
        self$group_order <- 1;
      } else {
        self$group_order <- group_order;
      }

      self$group_name <- group_name;
      self$description <- description;

      self$grelevance <- grelevance;

      self$group_order <- group_order;

      self$randomization_group <- randomization_group;

      self$language <- language;

      self$additional_languages <- additional_languages;

      self$otherOptions <- otherOptions;

    },


    ###-------------------------------------------------------------------------
    ### Add a question
    ###-------------------------------------------------------------------------

    #' @description
    #' Add a question to a group object.
    #' @param code The question code.
    #' @param type The question type.
    #' @param lsType The question type, as LimeSurvey question type.
    #' @param question_order The question order; automatically filled if left
    #' empty; starts counting at 0.
    #' @param ... Additional arguments are used to create the Question using
    #' `Question$new`.
    #' @return Invisibly, the `thisQuestion` object that was just added. Note
    #' that you can further modify this, which will modify the question object
    #' "in" the survey group as well. This allows you to pipe the question
    #' creation on to, for example, add answer options.
    add_question = function(code,
                            type = NULL,
                            lsType = NULL,
                            question_order = NULL,
                            ...) {

      ###-----------------------------------------------------------------------
      ### Check whether this code is unique
      ###-----------------------------------------------------------------------

      ###
      ###
      ###
      ###
      ###

      if (is.null(type) && is.null(lsType)) {
        stop("You must specify a question type!");
      }

      ###-----------------------------------------------------------------------
      ### Set the question order
      ###-----------------------------------------------------------------------

      if (is.null(question_order)) {
        if (length(self$questions) == 0) {
          question_order <- 0;
        } else {
          question_order <- max(unlist(lapply(self$questions, \(x) x$question_order))) + 1;
        }
      }

      ###-----------------------------------------------------------------------
      ### Create question object and store it
      ###-----------------------------------------------------------------------

      thisQuestion <-
        Question$new(id = private$new_id(),
                     sid = self$sid,
                     gid = self$id,
                     code = code,
                     type = type,
                     lsType = lsType,
                     language = self$language,
                     new_id = private$new_id,
                     question_order = question_order,
                     ...);

      ### Add to group
      self$questions <-
        c(self$questions,
          list(thisQuestion));

      ### Set name
      names(self$questions)[
        length(self$questions)] <- code;

      ### Return self invisibly
      return(invisible(thisQuestion));
    },

    ###-------------------------------------------------------------------------
    ### Export group to an LSG (xml) file
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the group as an LSG (xml) file.
    #' @param file The filename to which to save the file.
    #' @param preventOverwriting Whether to prevent overwriting.
    #' @param parallel Whether to work serially or in parallel.
    #' @param encoding The encoding to use
    #' @param silent Whether to be silent or chatty.
    #' @param backupLanguage The language to get content from if not from
    #' the primary langage.
    #' @return Invisibly, the `Survey` object.
    export_to_lsg = function(file = NULL,
                             preventOverwriting = limonaid::opts$get("preventOverwriting"),
                             encoding = limonaid::opts$get("encoding"),
                             silent = limonaid::opts$get("silent"),
                             backupLanguage = self$language) {

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
      ### Create XML document
      ###-----------------------------------------------------------------------

      xml <- xml2::xml_new_root("document");

      xml2::xml_add_child(xml, "LimeSurveyDocType", "Group");
      xml2::xml_add_child(xml, "DBVersion", "495");

      ###-----------------------------------------------------------------------
      ### Add languages
      ###-----------------------------------------------------------------------

      xml2::xml_add_child(
        xml,
        xmlNode_from_vector("languages", "language", languageList)
      );

      ###-----------------------------------------------------------------------
      ### Add groups (well, one group only)
      ###-----------------------------------------------------------------------

      xml2::xml_add_child(xml, "groups");

      xml2::xml_add_child(
        xml2::xml_child(xml, "groups"),
        xmlNode_from_vector("fields", "fieldname", ls_xmlFields$groups)
      );

      xml2::xml_add_child(xml2::xml_find_all(xml, "groups"), "rows");

      tempNode <- xml2::xml_find_all(xml, "groups/rows");

      xml2::xml_add_child(
        tempNode,

        xmlNode_from_vector(
          "row",
          subNames = ls_xmlFields$groups,
          c(self$id,
            self$sid,
            self$group_order,
            na_if_null(self$randomization_group),
            na_if_null(self$grelevance)
          ),
          cdata = TRUE
        )
      );

      ###-----------------------------------------------------------------------
      ### Add group localization
      ###-----------------------------------------------------------------------

      xml2::xml_add_child(xml, "group_l10ns");

      xml2::xml_add_child(
        xml2::xml_child(xml, "group_l10ns"),
        xmlNode_from_vector("fields", "fieldname", ls_xmlFields$group_l10ns)
      );

      xml2::xml_add_child(xml2::xml_find_all(xml, "group_l10ns"), "rows");

      tempNode <- xml2::xml_find_all(xml, "group_l10ns/rows");

      for (currentLanguage in languageList) {

        xml2::xml_add_child(
          tempNode,

          xmlNode_from_vector(
            "row",
            subNames = ls_xmlFields$group_l10ns,
            c(private$new_id(),
              self$id,
              self$group_name[currentLanguage],
              self$description[currentLanguage],
              currentLanguage,
              self$sid,
              self$group_order,
              na_if_null(self$randomization_group),
              na_if_null(self$grelevance)
            ),
            cdata = TRUE
          )
        );

      }

      ###-----------------------------------------------------------------------
      ### Add questions
      ###-----------------------------------------------------------------------

      xml2::xml_add_child(xml, "questions");

      xml2::xml_add_child(
        xml2::xml_child(xml, "questions"),
        xmlNode_from_vector("fields", "fieldname", ls_xmlFields$questions)
      );

      xml2::xml_add_child(xml2::xml_find_all(xml, "questions"), "rows");

      tempNode <- xml2::xml_find_all(xml, "questions/rows");

      for (currentQuestionCode in names(self$questions)) {

        xml2::xml_add_child(
          tempNode,
          self$questions[[currentQuestionCode]]$xmlExport_row_question(silent = silent)
        );

      }

      ###-----------------------------------------------------------------------
      ### Add subquestions
      ###-----------------------------------------------------------------------

      if (any(unlist(lapply(self$questions, \(x) x$has_subquestions)))) {

        xml2::xml_add_child(xml, "subquestions");

        xml2::xml_add_child(
          xml2::xml_child(xml, "subquestions"),
          xmlNode_from_vector("fields", "fieldname", ls_xmlFields$subquestions)
        );

        xml2::xml_add_child(xml2::xml_find_all(xml, "subquestions"), "rows");

        tempNode <- xml2::xml_find_all(xml, "subquestions/rows");

        subquestionRows <- lapply(self$questions,
                                  \(x) x$xmlExport_row_subquestions());

        for (currentSubquestionRow in unlist(subquestionRows, recursive=FALSE)) {

          xml2::xml_add_child(
            tempNode,
            currentSubquestionRow
          );

        }

      }

      ###-----------------------------------------------------------------------
      ### Add question_l10ns
      ###-----------------------------------------------------------------------

      xml2::xml_add_child(xml, "question_l10ns");

      xml2::xml_add_child(
        xml2::xml_child(xml, "question_l10ns"),
        xmlNode_from_vector("fields", "fieldname", ls_xmlFields$question_l10ns)
      );

      xml2::xml_add_child(xml2::xml_find_all(xml, "question_l10ns"), "rows");

      tempNode <- xml2::xml_find_all(xml, "question_l10ns/rows");

      question_l10nsRows <- lapply(self$questions,
                                \(x) x$xmlExport_row_question_l10ns(id_fun = private$new_id));

      for (currentQuestion_l10nsRow in unlist(question_l10nsRows, recursive=FALSE)) {

        xml2::xml_add_child(
          tempNode,
          currentQuestion_l10nsRow
        );

      }

      if (any(unlist(lapply(self$questions, \(x) x$has_answerOptions)))) {

        ###---------------------------------------------------------------------
        ### Add answers (if any question has answer options)
        ###---------------------------------------------------------------------

        xml2::xml_add_child(xml, "answers");

        xml2::xml_add_child(
          xml2::xml_child(xml, "answers"),
          xmlNode_from_vector("fields", "fieldname", ls_xmlFields$answers)
        );

        xml2::xml_add_child(xml2::xml_find_all(xml, "answers"), "rows");

        tempNode <- xml2::xml_find_all(xml, "answers/rows");

        answerRows <- lapply(self$questions, \(x) x$xmlExport_row_answers());

        for (currentAnswerRow in unlist(answerRows, recursive=FALSE)) {

          xml2::xml_add_child(
            tempNode,
            currentAnswerRow
          );

        }

        ###---------------------------------------------------------------------
        ### Add answer_l10ns (if any question has answer options)
        ###--------------------------------------------------------------------

        xml2::xml_add_child(xml, "answer_l10ns");

        xml2::xml_add_child(
          xml2::xml_child(xml, "answer_l10ns"),
          xmlNode_from_vector("fields", "fieldname", ls_xmlFields$answer_l10ns)
        );

        xml2::xml_add_child(xml2::xml_find_all(xml, "answer_l10ns"), "rows");

        tempNode <- xml2::xml_find_all(xml, "answer_l10ns/rows");

        answer_l10nsRows <- lapply(self$questions,
                                   \(x) x$xmlExport_row_answer_l10ns(id_fun = private$new_id));

        for (currentAnswer_l10nsRow in unlist(answer_l10nsRows, recursive=FALSE)) {

          xml2::xml_add_child(
            tempNode,
            currentAnswer_l10nsRow
          );

        }

      }

      ###-----------------------------------------------------------------------
      ### Add question_attributes
      ###-----------------------------------------------------------------------

      xml2::xml_add_child(xml, "question_attributes");

      xml2::xml_add_child(
        xml2::xml_child(xml, "question_attributes"),
        xmlNode_from_vector("fields", "fieldname", ls_xmlFields$question_attributes)
      );

      xml2::xml_add_child(xml2::xml_find_all(xml, "question_attributes"), "rows");

      tempNode <- xml2::xml_find_all(xml, "question_attributes/rows");

      attributeRows <- lapply(self$questions,
                              \(x) x$xmlExport_row_attributes());

      for (currentAttributeRow in unlist(attributeRows, recursive=FALSE)) {

        xml2::xml_add_child(
          tempNode,
          currentAttributeRow
        );

      }



      ###-----------------------------------------------------------------------
      ### Potentially write file and return result
      ###-----------------------------------------------------------------------

      if (is.null(file)) {

        return(xml);

      } else {

        if (preventOverwriting && file.exists(file)) {
          warning("The file you specified, '", file, "', already exists, and ",
                  "the `preventOverwriting` argument is set to TRUE, so I ",
                  "will not overwrite the file. Set it to FALSE to write anyway.");
        } else {
          xml2::write_xml(xml,
                          file,
                          encoding = encoding);
        }

        return(invisible(xml));

      }

    }

  ), ### End of public properties and methods

  ###---------------------------------------------------------------------------
  ### Private properties & methods
  ###---------------------------------------------------------------------------

  private = list(

    ### Unique numeric identifiers (for MySQL basically)
    idCounter = 0,

    ### This will be loaded with a function to return identifiers
    ### https://stackoverflow.com/questions/39914775/updating-method-definitions-in-r6-object-instance#51714770
    new_id = NULL,

    ### Counters for exporting
    exportQuestionIdMapping = c()

  ) ### End of private properties and methods

)
