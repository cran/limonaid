### The importing of R6 is to avoid an error for R CMD Check, see:
### https://stackoverflow.com/questions/64055049/unexpected-note-namespace-in-imports-field-not-imported-from-r6

#' R6 Class representing a LimeSurvey survey
#'
#' Create and work with a Survey to programmatically (or interactively)
#' create a survey, export it to a tab separated values file, and import
#' it to LimeSurvey.
#'
#' @import R6
#'
#' @export
Survey <- R6::R6Class(

  "Survey",

  ###---------------------------------------------------------------------------
  ### Public properties & methods
  ###---------------------------------------------------------------------------

  public = list(

    ###-------------------------------------------------------------------------
    ### Public properties
    ###-------------------------------------------------------------------------

    #' @field titles The title of the survey in the primary language and
    #' any additional languages
    titles = NULL,

    #' @field descriptions The descriptions of the survey in the primary
    #' language and any additional languages
    descriptions = NULL,

    #' @field welcomeTexts The welcome texts of the survey in the primary
    #' language and any additional languages
    welcomeTexts = NULL,

    #' @field endTexts The end texts of the survey in the primary
    #' language and any additional languages
    endTexts = NULL,

    #' @field endURLs The end URLs of the survey in the primary
    #' language and any additional languages
    endURLs = NULL,

    #' @field endURLdescriptions The end URL descriptions of the survey in
    #' the primary language and any additional languages
    endURLdescriptions = NULL,

    #' @field dateformats The date format to use in
    #' the primary language and any additional languages; the index of
    #' the option from the dropdown in LimeSurvey (6 is the ISO standard,
    #' "YYYY-MM-DD").
    dateformats = 6,

    #' @field numberformats The number format to use in
    #' the primary language and any additional languages (for periods as
    #' decimal marks, `0`; for commas as decimal marks, `1`).
    numberformats = 0,

    #' @field sid The unique survey identifier; if this is free when
    #' importing the survey, this will be used.
    sid = "1",

    #' @field gsid The Survey Group identifier.
    gsid = "1",

    #' @field admin The name of the survey administrator
    admin = NULL,

    #' @field adminemail The email address of the survey administrator
    adminemail = "",

    #' @field anonymized Whether the survey uses anonymized
    #' responses (`Y` or `N`).
    anonymized = "Y",

    #' @field faxto The contents of the "Fax to" field
    faxto = "",

    #' @field format How to present the survey (`Q` for question by
    #' question; `G` for group by group; and `A` for all in one).
    format = "G",

    #' @field savetimings Whether to save timings of responses (`Y` or `N`).
    savetimings = "Y",

    #' @field template The name of the LimeSurvey theme to use.
    template = "",

    #' @field language The primary language of the survey.
    language = NULL,

    #' @field additional_languages Any additional languages the survey uses.
    additional_languages = "",

    #' @field datestamp Whether to datestamp responses (`Y` or `N`).
    datestamp = "Y",

    #' @field usecookie Whether to use cookies to enable answer persistence.
    usecookie = "N",

    #' @field allowregister Whether to allow public registration (`Y` or `N`).
    allowregister = "N",

    #' @field allowsave Whether to allow users to save their responses and
    #' returning later (`Y` or `N`).
    allowsave = "N",

    #' @field autonumber_start Where to start autonumbering
    autonumber_start = 1,

    #' @field autoredirect Whether to automatically redirect users to a
    #' URL (`Y` or `N`).
    autoredirect = "Y",

    #' @field allowprev Whether to allow users to return to previous
    #' pages (`Y` or `N`).
    allowprev = "N",

    #' @field printanswers Whether to allow printing of answer (`Y` or `N`).
    printanswers = "N",

    #' @field ipaddr Whether to store IP addresses (`Y` or `N`).
    ipaddr = "N",

    #' @field refurl Whether to store the referring URL (`Y` or `N`).
    refurl = "N",

    #' @field showsurveypolicynotice Whether to show the data policy
    #' notice (`Y` or `N`).
    showsurveypolicynotice = "N",

    #' @field publicstatistics Whether to have public statistics (`Y` or `N`).
    publicstatistics = "N",

    #' @field publicgraphs Whether to show graphs in public
    #' statistics (`Y` or `N`).
    publicgraphs = "N",

    #' @field listpublic Whether to list the survey publicly (`Y` or `N`).
    listpublic = "N",

    #' @field htmlemail Whether to use HTML format for token
    #' emails (`Y` or `N`).
    htmlemail = "Y",

    #' @field sendconfirmation Whether to send confirmation
    #' emails (`Y` or `N`).
    sendconfirmation = "N",

    #' @field tokenanswerspersistence Whether to use token-based
    #' response persistence (`Y` or `N`).
    tokenanswerspersistence = "N",

    #' @field assessments Whether to use assessments (`Y` or `N`).
    assessments = "N",

    #' @field usecaptcha Whether to use CAPTCHA's (`Y` or `N`).
    usecaptcha = "N",

    #' @field usetokens Whether to use tokens (`Y` or `N`).
    usetokens = "N",

    #' @field bounce_email Where bouncing emails should be sent.
    bounce_email = "",

    #' @field emailresponseto Where detailed admin notifications emails
    #' should be sent.
    emailresponseto = "",

    #' @field emailnotificationto Where a notification should be sent for new
    #' responses.
    emailnotificationto = "",

    #' @field tokenlength The token length.
    tokenlength = 15,

    #' @field showxquestions Whether to show "There are X questions in this
    #' survey" (`Y` or `N`).
    showxquestions = "N",

    #' @field showgroupinfo Whether to show group name and info (`B` for both,
    #' `?`, or `X` to show nothing).
    showgroupinfo = "X",

    #' @field shownoanswer Whether to show the "No answer" option (`Y` or `N`).
    shownoanswer = "N",

    #' @field showqnumcode Whether to show answer codes or numbers  (`Y`, `N`,
    #' or `X` to show nothing).
    showqnumcode = "",

    #' @field bounceprocessing Whether to process bouncing emails? (`Y` or `N`).
    bounceprocessing = "N",

    #' @field showwelcome Whether to show the welcome page (`Y` or `N`).
    showwelcome = "N",

    #' @field showprogress Whether to show the progress bar (`Y` or `N`).
    showprogress = "",

    #' @field questionindex Whether to show the question index  (`0` to
    #' disable; can also be set to incremental or full (`1` and `2`?)).
    questionindex = "0",

    #' @field navigationdelay The navigation delay in seconds
    navigationdelay = "0",

    #' @field nokeyboard Whether to show the on-screen keyboard (`Y` or `N`).
    nokeyboard = "N",

    #' @field alloweditaftercompletion Whether to allow multiple
    #' reponses (`N`) or to allow updating responses with one token (`Y`)?
    alloweditaftercompletion = "N",

    #' @field googleanalyticsstyle The google analytics settings; `0` for None,
    #' other values for other settings.
    googleanalyticsstyle = 0,

    #' @field googleanalyticsapikey The google analytics API key.
    googleanalyticsapikey = "",

    #' @field groups The groups in the survey.
    groups = list(),

    #' @field tsvData Used to store the dataframe saved to a file as
    #' tab separated values.
    tsvData = data.frame(),

    ###-------------------------------------------------------------------------
    ### Initialization
    ###-------------------------------------------------------------------------

    #' @description
    #' Create a new survey object.
    #' @param titles The titles of the survey in the primary language and
    #' optionally any addiitonal languages.
    #' @param descriptions The descriptions of the survey in the primary
    #' language and any additional languages
    #' @param welcomeTexts The welcome texts of the survey in the primary
    #' language and any additional languages
    #' @param endTexts The end texts of the survey in the primary
    #' language and any additional languages
    #' @param endURLs The end URLs of the survey in the primary
    #' language and any additional languages
    #' @param endURLdescriptions The end URL descriptions of the survey in
    #' the primary language and any additional languages
    #' @param dateformats The date formats to use in
    #' the primary language and any additional languages; the index of
    #' the option from the dropdown in LimeSurvey (6 is the ISO standard,
    #' "YYYY-MM-DD").
    #' @param numberformats The number formats to use in
    #' the primary language and any additional languages (for periods as
    #' decimal marks, `0`; for commas as decimal marks, `1`).
    #' @param sid The unique survey identifier; if this is free when
    #' importing the survey, this will be used.
    #' @param gsid The Survey Group identifier.
    #' @param admin The name of the survey administrator
    #' @param adminemail The email address of the survey administrator
    #' @param anonymized Whether the survey uses anonymized
    #' responses (`Y` or `N`).
    #' @param faxto The contents of the "Fax to" field
    #' @param format How to present the survey (`Q` for question by
    #' question; `G` for group by group; and `A` for all in one).
    #' @param savetimings Whether to save timings of responses (`Y` or `N`).
    #' @param template The name of the LimeSurvey theme to use.
    #' @param language The primary language of the survey.
    #' @param additional_languages Any additional languages the survey uses.
    #' @param datestamp Whether to datestamp responses (`Y` or `N`).
    #' @param usecookie Whether to use cookies to enable answer persistence.
    #' @param allowregister Whether to allow public registration (`Y` or `N`).
    #' @param allowsave Whether to allow users to save their responses and
    #' returning later (`Y` or `N`).
    #' @param autonumber_start Where to start autonumbering
    #' @param autoredirect Whether to automatically redirect users to a
    #' URL (`Y` or `N`).
    #' @param allowprev Whether to allow users to return to previous
    #' pages (`Y` or `N`).
    #' @param printanswers Whether to allow printing of answer (`Y` or `N`).
    #' @param ipaddr Whether to store IP addresses (`Y` or `N`).
    #' @param refurl Whether to store the referring URL (`Y` or `N`).
    #' @param showsurveypolicynotice Whether to show the data policy
    #' notice (`Y` or `N`).
    #' @param publicstatistics Whether to have public statistics (`Y` or `N`).
    #' @param publicgraphs Whether to show graphs in public
    #' statistics (`Y` or `N`).
    #' @param listpublic Whether to list the survey publicly (`Y` or `N`).
    #' @param htmlemail Whether to use HTML format for token
    #' emails (`Y` or `N`).
    #' @param sendconfirmation Whether to send confirmation
    #' emails (`Y` or `N`).
    #' @param tokenanswerspersistence Whether to use token-based
    #' response persistence (`Y` or `N`).
    #' @param assessments Whether to use assessments (`Y` or `N`).
    #' @param usecaptcha Whether to use CAPTCHA's (`Y` or `N`).
    #' @param usetokens Whether to use tokens (`Y` or `N`).
    #' @param bounce_email Where bouncing emails should be sent.
    #' @param emailresponseto Where detailed admin notifications emails
    #' should be sent.
    #' @param emailnotificationto Where a notification should be sent for new
    #' responses.
    #' @param tokenlength The token length.
    #' @param showxquestions Whether to show "There are X questions in this
    #' survey" (`Y` or `N`).
    #' @param showgroupinfo Whether to show group name and info (`Y`, `N`,
    #' or `X` to show nothing).
    #' @param shownoanswer Whether to show the "No answer" option (`Y` or `N`).
    #' @param showqnumcode Whether to show answer codes or numbers  (`Y`, `N`,
    #' or `X` to show nothing).
    #' @param bounceprocessing Whether to process bouncing emails? (`Y` or `N`).
    #' @param showwelcome Whether to show the welcome page (`Y` or `N`).
    #' @param showprogress Whether to show the progress bar (`Y` or `N`).
    #' @param questionindex Whether to show the question index  (`0` to
    #' disable; can also be set to incremental or full (`1` and `2`?)).
    #' @param navigationdelay The navigation delay in seconds
    #' @param nokeyboard Whether to show the on-screen keyboard (`Y` or `N`).
    #' @param alloweditaftercompletion Whether to allow multiple
    #' reponses (`N`) or to allow updating responses with one token (`Y`)?
    #' @param googleanalyticsstyle The google analytics settings; `0` for None,
    #' other values for other settings.
    #' @param googleanalyticsapikey The google analytics API key.
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
    #' @return A new `Survey` object.
    initialize = function(titles,
                          descriptions = "",
                          welcomeTexts = "",
                          endTexts = "",
                          endURLs = "",
                          endURLdescriptions = "",
                          dateformats = 6,
                          numberformats = 0,
                          sid = 1,
                          gsid = 1,
                          admin = "Admin Name",
                          adminemail = "email@add.ress",
                          anonymized = "Y",
                          faxto = "",
                          format = "G",
                          savetimings = "Y",
                          template = "vanilla",
                          language = "en",
                          additional_languages = "",
                          datestamp = "Y",
                          usecookie = "N",
                          allowregister = "N",
                          allowsave = "N",
                          autonumber_start = 0,
                          autoredirect = "Y",
                          allowprev = "N",
                          printanswers = "N",
                          ipaddr = "N",
                          refurl = "N",
                          showsurveypolicynotice = "0",
                          publicstatistics = "N",
                          publicgraphs = "N",
                          listpublic = "N",
                          htmlemail = "Y",
                          sendconfirmation = "N",
                          tokenanswerspersistence = "N",
                          assessments = "N",
                          usecaptcha = "N",
                          usetokens = "N",
                          bounce_email = "",
                          emailresponseto = "",
                          emailnotificationto = "",
                          tokenlength = 15,
                          showxquestions = "N",
                          showgroupinfo = "X",
                          shownoanswer = "N",
                          showqnumcode = "X",
                          bounceprocessing = "N",
                          showwelcome = "N",
                          showprogress = "N",
                          questionindex = "0",
                          navigationdelay = "0",
                          nokeyboard = "N",
                          alloweditaftercompletion = "N",
                          googleanalyticsstyle = 0,
                          googleanalyticsapikey = "",
                          new_id_fun = NULL) {

      ###-----------------------------------------------------------------------
      ### Check whether the multilingual fields have been passed properly
      ###-----------------------------------------------------------------------

      titles <-
        checkMultilingualFields(titles,
                                language = language);

      descriptions <-
        checkMultilingualFields(descriptions,
                                language = language);

      welcomeTexts <-
        checkMultilingualFields(welcomeTexts,
                                language = language);

      endTexts <-
        checkMultilingualFields(endTexts,
                                language = language);

      endURLs <-
        checkMultilingualFields(endURLs,
                                language = language);

      endURLdescriptions <-
        checkMultilingualFields(endURLdescriptions,
                                language = language);

      dateformats <-
        checkMultilingualFields(dateformats,
                                language = language,
                                classCheck = is.numeric,
                                className = "numeric");

      numberformats <-
        checkMultilingualFields(numberformats,
                                language = language,
                                classCheck = is.numeric,
                                className = "numeric");

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
      ### Set general settings
      ###-----------------------------------------------------------------------

      additional_languages <-
        setdiff(names(titles),
                language);

      self$sid <- sid;
      self$gsid <- gsid;
      self$admin <- admin;
      self$adminemail <- adminemail;
      self$anonymized <- anonymized;
      self$faxto <- faxto;
      self$format <- format;
      self$savetimings <- savetimings;
      self$template <- template;
      self$language <- language;
      self$additional_languages <- additional_languages;
      self$datestamp <- datestamp;
      self$usecookie <- usecookie;
      self$allowregister <- allowregister;
      self$allowsave <- allowsave;
      self$autonumber_start <- autonumber_start;
      self$autoredirect <- autoredirect;
      self$allowprev <- allowprev;
      self$printanswers <- printanswers;
      self$ipaddr <- ipaddr;
      self$refurl <- refurl;
      self$showsurveypolicynotice <- showsurveypolicynotice;
      self$publicstatistics <- 	publicstatistics;
      self$publicgraphs <- publicgraphs;
      self$listpublic <- listpublic;
      self$htmlemail <- htmlemail;
      self$sendconfirmation <- sendconfirmation;
      self$tokenanswerspersistence <- tokenanswerspersistence;
      self$assessments <- assessments;
      self$usecaptcha <- usecaptcha;
      self$usetokens <- usetokens;
      self$bounce_email <- bounce_email;
      self$emailresponseto <- emailresponseto;
      self$emailnotificationto <- emailnotificationto;
      self$tokenlength <- tokenlength;
      self$showxquestions <- showxquestions;
      self$showgroupinfo <- showgroupinfo;
      self$shownoanswer <- shownoanswer;
      self$showqnumcode <- showqnumcode;
      self$bounceprocessing <- bounceprocessing;
      self$showwelcome <- showwelcome;
      self$showprogress <- showprogress;
      self$questionindex <- questionindex;
      self$navigationdelay <- navigationdelay;
      self$nokeyboard <- nokeyboard;
      self$alloweditaftercompletion <- alloweditaftercompletion;
      self$googleanalyticsstyle <- googleanalyticsstyle;
      self$googleanalyticsapikey <- googleanalyticsapikey;

      ###-----------------------------------------------------------------------
      ### Set language-specific settings
      ###-----------------------------------------------------------------------

      self$titles <- titles;
      self$descriptions <- descriptions;
      self$welcomeTexts <- welcomeTexts;
      self$endTexts <- endTexts;
      self$endURLs <- endURLs;
      self$endURLdescriptions <- endURLdescriptions;
      self$dateformats <- dateformats;
      self$numberformats <- numberformats;

    },

    ###-------------------------------------------------------------------------
    ### Add a question group
    ###-------------------------------------------------------------------------

    #' @description
    #' Add a group to a survey object.
    #' @param titles The group's title, either as a named character vector
    #' where each element is the group title in a different language, and
    #' every element's name is the language code; or as a single character
    #' value, in which case the survey's primary language is used.
    #' @param descriptions The group description, either as a named character
    #' vector where each element is the group description in a different
    #' language, and every element's name is the language code; or as a single
    #' character value, in which case the survey's primary language is used.
    #' @param relevance The group's relevance equation.
    #' @param random_group The group's randomization group.
    #' @return Invisibly, the `Survey` object.
    add_group = function(titles,
                         descriptions = "",
                         relevance = 1,
                         random_group = "") {

      ###-----------------------------------------------------------------------
      ### Check and fix titles and descriptions
      ###-----------------------------------------------------------------------

      titles <-
        checkMultilingualFields(titles,
                                language = self$language);

      descriptions <-
        checkMultilingualFields(descriptions,
                                language = self$language);

      ###-----------------------------------------------------------------------
      ### Create group object and store it
      ###-----------------------------------------------------------------------

      thisGroup <-
        list(id = private$new_id(),
             titles = titles,
             descriptions = descriptions,
             relevance = relevance,
             random_group = random_group,
             new_id_fun = private$new_id);

      ### Add to groups in survey
      self$groups <-
        c(self$groups,
          list(thisGroup));

      ### Set name
      names(self$groups)[
        length(self$groups)] <- thisGroup$id;

      ### Return self invisibly
      return(invisible(self));
    },

    ###-------------------------------------------------------------------------
    ### Add a question
    ###-------------------------------------------------------------------------

    #' @description
    #' Add a question to a survey object.
    #' @param groupId The id of the group to add the question to.
    #' @param code The question code.
    #' @param type The question type.
    #' @param lsType The question type, as LimeSurvey question type.
    #' @param ... Additional arguments are used to create the Question using
    #' `Question$new`.
    #' @return Invisibly, the `Survey` object.
    add_question = function(groupId,
                            code,
                            type = NULL,
                            lsType = NULL,
                            ...) {

      groupId <- as.character(groupId);

      ###-----------------------------------------------------------------------
      ### Check whether this code is unique
      ###-----------------------------------------------------------------------

      ###
      ###
      ###
      ###
      ###

      if (!(groupId %in% self$get_group_ids)) {
        stop("The group id you specified, ",
             groupId,
             " does not exist in this survey (ids that do exist: ",
             vecTxt(self$get_group_ids),
             ").");
      }

      if (is.null(type) && is.null(lsType)) {
        stop("You must specify a question type!");
      }

      ###-----------------------------------------------------------------------
      ### Create question object and store it
      ###-----------------------------------------------------------------------

      thisQuestion <-
        Question$new(id = private$new_id(),
                     code = code,
                     type = type,
                     lsType = lsType,
                     language = self$language,
                     new_id_fun = private$new_id,
                     ...);

      ### Add to group
      self$groups[[groupId]]$questions <-
        c(self$groups[[groupId]]$questions,
          list(thisQuestion));

      ### Set name
      names(self$groups[[groupId]]$questions)[
        length(self$groups[[groupId]]$questions)] <- code;

      ### Return self invisibly
      return(invisible(self));
    },

    ###-------------------------------------------------------------------------
    ### Export the survey as a tab separated values file
    ###-------------------------------------------------------------------------

    #' @description
    #' Export the survey as a tab separated values file (see
    #' https://manual.limesurvey.org/Tab_Separated_Value_survey_structure).
    #' @param file The filename to which to save the file.
    #' @param preventOverwriting Whether to prevent overwriting.
    #' @param parallel Whether to work serially or in parallel.
    #' @param encoding The encoding to use
    #' @param silent Whether to be silent or chatty.
    #' @param backupLanguage The language to get content from if not from
    #' the primary langage.
    #' @return Invisibly, the `Survey` object.
    export_to_tsv = function(file,
                             preventOverwriting = limonaid::opts$get("preventOverwriting"),
                             parallel = TRUE,
                             encoding = limonaid::opts$get("encoding"),
                             silent = limonaid::opts$get("silent"),
                             backupLanguage = self$language) {

      ###-----------------------------------------------------------------------
      ### First we create the right dataframe; then we write that dataframe
      ### using `ls_write_tsv()`.
      ###-----------------------------------------------------------------------

      ### Note that since only a few columns are quoted anyway, which
      ### is done 'manually' by `ls_write_tsv()`, we can just set all
      ### columns to character
      dat <-
        data.frame(
          id = character(),
          related_id = character(),
          class = character(),
          type.scale = character(),
          name = character(),
          relevance = character(),
          text = character(),
          help = character(),
          language = character(),
          validation = character(),
          mandatory = character(),
          other = character(),
          default = character(),
          same_default = character(),
          stringsAsFactors = FALSE
        );

      selfAsList <- as.list(self);

      ###-----------------------------------------------------------------------
      ### Add general survey settings
      ###-----------------------------------------------------------------------

      if (!silent) {
        cat0("\n\nProcessing general survey settings.\n\n");
      }

      for (name in private$generalSurveySettings) {

        text <- selfAsList[[name]];

        if (is.null(text)) {
          text <- "";
        }

        if (length(text) > 1) {
          text <- paste(text, collapse=" ");
        } else if (length(text) == 0) {
          text <- "";
        }

        newRow <-
          data.frame(
            id = "",
            related_id = "",
            class="S",
            type.scale = "",
            name = name,
            relevance = "",
            text = text,
            help = "",
            language = "",
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

      if (!silent) {
        cat0("\n\nWill now start processing the survey with ",
             "primary language '", self$language, "' and additional ",
             "languages ", vecTxtQ(self$additional_languages),
             ", using '", backupLanguage, "' as a backup language.\n\n");
      }

      ###-----------------------------------------------------------------------
      ### Add language-specific survey settings
      ###-----------------------------------------------------------------------

      if (!silent) {
        cat0("\n\nProcessing survey-level, language-specific settings.\n\n");
      }

      for (currentLanguage in languageList) {

        if (!silent) {
          cat0("\n\n  Processing language `", currentLanguage, "`.\n\n");
        }

        newRow <-
          data.frame(
            id = rep("", 2),
            related_id = rep("", 2),
            class = rep("SL", 2),
            type.scale = rep("", 2),
            name = c("surveyls_survey_id", "surveyls_language"),
            relevance = rep("", 2),
            text = c(self$sid, currentLanguage),
            help = rep("", 2),
            language = rep(currentLanguage, 2),
            validation = rep("", 2),
            mandatory = rep("", 2),
            other = rep("", 2),
            default = rep("", 2),
            same_default = rep("", 2),
            stringsAsFactors = FALSE
          );

        ### Add row using our homerolled version of plyr::rbind.fill
        dat <- append_lsdf_rows(dat, newRow);

        for (i in seq_along(private$languageSpecificSurveySettings)) {

          name <- names(private$languageSpecificSurveySettings)[i];
          propertyName <- private$languageSpecificSurveySettings[i];

          text <- selfAsList[[propertyName]];
          if ((!is.null(text)) &&
              (currentLanguage %in% names(text))) {
            text <- selfAsList[[propertyName]][[currentLanguage]];
          } else if (name %in% names(private$emailDefaults)) {
            text <- private$emailDefaults[name];
          } else {
            ### Set primary language for everything unspecified in this language

            if (!backupLanguage %in% names(selfAsList[[propertyName]])) {
              stop("Neither the language I am processing, `", currentLanguage,
                   "`, nor the backup language, `", backupLanguage,
                   "`, is available in property `", propertyName,
                   "`. The only available languages are: ",
                   vecTxt(names(selfAsList[[propertyName]]), useQuote="`"),
                   ".");
            } else {
              text <- selfAsList[[propertyName]][[backupLanguage]];
            }
          }

          newRow <-
            data.frame(
              id = "",
              related_id = "",
              class="SL",
              type.scale = "",
              name = name,
              relevance = "",
              text = text,
              help = "",
              language = currentLanguage,
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

      ###-----------------------------------------------------------------------
      ### Loop through languages and add groups, questions, subquestions, and
      ### answer options
      ###-----------------------------------------------------------------------

      ### As per
      ### https://www.limesurvey.org/manual/Tab_Separated_Value_survey_structure,
      ### identifiers should count questions and subquestions, so we map
      ### unique identifiers based on the codes onto these numeric identifiers.

      ### First for primary language

      if (!silent) {
        cat0("\n\nProcessing survey for language: ", self$language);
        if (length(self$additional_languages) > 0) {
          cat0(" (primary language; 1 of ", length(languageList), ")\n");
        } else {
          cat0(" (primary and only language in this survey)\n");
        }
      }

      lsdf_for_language_list <-
        lsdf_for_language(
          language = self$language,
          groups = self$groups,
          exportGroupIdMapping = private$exportGroupIdMapping,
          exportQuestionIdMapping = private$exportQuestionIdMapping,
          backupLanguage = backupLanguage,
          silent = silent
        );

      ### Store potentially updated mappings
      private$exportGroupIdMapping <-
        lsdf_for_language_list$exportGroupIdMapping;
      private$exportQuestionIdMapping <-
        lsdf_for_language_list$exportQuestionIdMapping;

      ### Add row using our homerolled version of plyr::rbind.fill
      dat <- append_lsdf_rows(dat,
                              lsdf_for_language_list$dat);

      ### Then, if need be, for the secondary languages, either serially
      ### or in parallel.
      if (
        (length(self$additional_languages) == 0) ||
        (
          (length(self$additional_languages) == 1) &&
          self$additional_languages == ""
         )
      ) {
        if (!silent) {
          cat0("\n\nNo additional langues specified in this survey.\n\n");
        }
      } else {

        if (!silent) {
          cat0("\n\nStarting to process ", length(self$additional_languages),
               " additional languages (",
               vecTxt(self$additional_languages, useQuote="`"),
               "), ", ifelse(parallel, "", "not "), "using parallel ",
               "computing to utilize all CPU cores.\n\n");
        }

        if (parallel && requireNamespace("parallel", quietly = TRUE)) {

          ### Then for all other languages in parallel; detect number of cores
          ### and create a cluster
          nCores <- parallel::detectCores();

          ### Because the trick below doesn't seem to work
          maxCores <- limonaid::opts$get("maxCores");
          if (!is.null(maxCores) && is.numeric(maxCores)) {
            nCores <- min(maxCores, nCores);
          }

          ### From https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
          chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
          if (nzchar(chk) && chk == "TRUE") {
            # use 2 cores in CRAN/Travis/AppVeyor
            nCores <- min(2L, nCores);
          }

          cl <- parallel::makeCluster(nCores);

          ### Load the limonaid package in each cluster
          parallel::clusterEvalQ(
            cl,
            library(limonaid)
          );

          ### Prepare objects to export to each cluster
          groups <- self$groups;
          exportGroupIdMapping <- private$exportGroupIdMapping;
          exportQuestionIdMapping <- private$exportQuestionIdMapping;
          primaryLanguage <- self$language;

          ### Export these objects and the 'silent' setting
          parallel::clusterExport(
            cl,
            c('groups',
              'exportGroupIdMapping',
              'exportQuestionIdMapping',
              'primaryLanguage',
              'silent'),
            envir = environment()
          );

          ### Perform the parallel computations
          parallelOutput <-
            parallel::parLapply(
              cl,
              self$additional_languages,
              function(language) {
                return(
                  limonaid::lsdf_for_language(
                    language = language,
                    groups = groups,
                    exportGroupIdMapping = exportGroupIdMapping,
                    exportQuestionIdMapping = exportQuestionIdMapping,
                    backupLanguage = backupLanguage,
                    silent=silent)
                );
              }
            );

          ### Stop the cluster
          parallel::stopCluster(cl);

          ### Extract the dataframes
          for (i in seq_along(parallelOutput)) {
            dat <- append_lsdf_rows(dat,
                                    parallelOutput[[i]]$dat);
          }

        } else {

          if (parallel) {
            warning("Argument `parallel` was `TRUE` (its default value), ",
                    "but you don't have package 'parallel' installed. ",
                    "If you want to install it, you can use:\n\n",
                    "  install.packages('parallel');\n");
          }

          for (currentLanguage in self$additional_languages) {

            if (!silent) {
              cat0("\n\nProcessing survey for language: ", currentLanguage,
                   " (",
                   which(self$additional_languages == currentLanguage) + 1,
                   " out of ",
                   length(self$additional_languages) + 1,
                   ")\n");
            }

            lsdf_for_language_list <-
              lsdf_for_language(
                language = currentLanguage,
                groups = self$groups,
                exportGroupIdMapping = private$exportGroupIdMapping,
                exportQuestionIdMapping = private$exportQuestionIdMapping,
                backupLanguage = backupLanguage,
                silent = silent
              );

            ### Store potentially updated mappings
            private$exportGroupIdMapping <-
              lsdf_for_language_list$exportGroupIdMapping;
            private$exportQuestionIdMapping <-
              lsdf_for_language_list$exportQuestionIdMapping;

            ### Add row using our homerolled version of plyr::rbind.fill
            dat <- append_lsdf_rows(dat,
                                    lsdf_for_language_list$dat);

          }

        }
      }

      if (!silent) {
        cat0("\n\nFinalizing survey and starting to write file.\n\n");
      }

      ### Add other columns
      dat[, setdiff(private$otherColumns, names(dat))] <- "";

      ###-----------------------------------------------------------------------
      ### Write file
      ###-----------------------------------------------------------------------

      self$tsvData <-
        ls_write_tsv(data = dat,
                     file = file,
                     preventOverwriting = preventOverwriting,
                     encoding = encoding,
                     silent = silent);

      if (!silent) {
        cat0("Wrote file to '", file, "'\n\n");
      }

      ### Return self invisibly
      return(invisible(self));
    },

    ###-------------------------------------------------------------------------
    ### Find the numeric group identifier by group title
    ###-------------------------------------------------------------------------

    #' @description
    #' Find the numeric group identifier by group title.
    #' @param title The survey title.
    #' @param titleLanguage The language in which to search.
    #' @return Invisibly, the `Survey` object.
    find_group_id = function(title,
                             titleLanguage = NULL) {

      if (is.null(titleLanguage)) {
        titleLanguage <- self$language;
      }

      groupIds <- self$get_group_ids;

      groupTitles <-
        unlist(
          lapply(
            groupIds,
            function(gId) {
              return(self$groups[[gId]]$titles[[titleLanguage]]);
            }));

      titleIndex <-
        which(groupTitles == title);

      if (length(titleIndex) > 0) {
        return(groupIds[titleIndex]);
      } else {
        return(FALSE);
      }

    }

  ), ### End of public properties and methods

  ###---------------------------------------------------------------------------
  ### Active fields
  ###---------------------------------------------------------------------------

  active = list(

    #' @field get_group_ids A list of all group ids.
    get_group_ids = function(value) {
      if (missing(value)) {
        return(unlist(lapply(self$groups,
                             function(x) return(x$id)),
                      use.names=FALSE));
      } else {
        stop("You cannot directly set group ids.");
      }
    },

    #' @field get_group_titles A list of all group ids.
    get_group_titles = function(value) {
      if (missing(value)) {
        if (length(self$groups) == 1) {
          return(NULL);
        }
        allTitles <-
          lapply(self$groups,
                 function(x) return(x$titles));
        res <- data.frame(allTitles,
                          row.names = names(allTitles[[1]]));
        res <-
          apply(res, 1, function(row) return(list(unname(row))));
        return(unlist(res,
                      recursive = FALSE));
      } else {
        stop("You cannot set group titles this way.");
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
    new_id = NULL,

    ### Counters for exporting
    exportGroupIdMapping = c(),
    exportQuestionIdMapping = c(),

    generalSurveySettings =
      c("sid",
        "gsid",
        "admin",
        "adminemail",
        "anonymized",
        "faxto",
        "format",
        "savetimings",
        "template",
        "language",
        "additional_languages",
        "datestamp",
        "usecookie",
        "allowregister",
        "allowsave",
        "autonumber_start",
        "autoredirect",
        "allowprev",
        "printanswers",
        "ipaddr",
        "refurl",
        "showsurveypolicynotice",
        "publicstatistics",
        "publicgraphs",
        "listpublic",
        "htmlemail",
        "sendconfirmation",
        "tokenanswerspersistence",
        "assessments",
        "usecaptcha",
        "usetokens",
        "bounce_email",
        "emailresponseto",
        "emailnotificationto",
        "tokenlength",
        "showxquestions",
        "showgroupinfo",
        "shownoanswer",
        "showqnumcode",
        "bounceprocessing",
        "showwelcome",
        "showprogress",
        "questionindex",
        "navigationdelay",
        "nokeyboard",
        "alloweditaftercompletion",
        "googleanalyticsstyle",
        "googleanalyticsapikey"),

    languageSpecificSurveySettings =
      c(surveyls_title = "titles",
        surveyls_description = "descriptions",
        surveyls_welcometext = "welcomeTexts",
        surveyls_endtext = "endTexts",
        surveyls_url = "endURLs",
        surveyls_urldescription = "endURLdescriptions",
        surveyls_email_invite_subj = "surveyls_email_invite_subj",
        surveyls_email_invite = "surveyls_email_invite",
        surveyls_email_remind_subj = "surveyls_email_remind_subj",
        surveyls_email_remind = "surveyls_email_remind",
        surveyls_email_register_subj = "surveyls_email_register_subj",
        surveyls_email_register = "surveyls_email_register",
        surveyls_email_confirm_subj = "surveyls_email_confirm_subj",
        surveyls_email_confirm = "surveyls_email_confirm",
        surveyls_dateformat = "dateformats",
        email_admin_notification_subj = "email_admin_notification_subj",
        email_admin_notification = "email_admin_notification",
        email_admin_responses_subj = "email_admin_responses_subj",
        email_admin_responses = "email_admin_responses",
        surveyls_numberformat = "numberformats"),

    otherColumns =
      c("allowed_filetypes",
        "alphasort",
        "answer_width",
        "answer_width_bycolumn",
        "array_filter",
        "array_filter_exclude",
        "array_filter_style",
        "assessment_value",
        "category_separator",
        "choice_input_columns",
        "choice_title",
        "code_filter",
        "commented_checkbox",
        "commented_checkbox_auto",
        "cssclass",
        "date_format",
        "date_max",
        "date_min",
        "display_columns",
        "display_rows",
        "display_type",
        "dropdown_dates",
        "dropdown_dates_minute_step",
        "dropdown_dates_month_style",
        "dropdown_prefix",
        "dropdown_prepostfix",
        "dropdown_separators",
        "dropdown_size",
        "dualscale_headerA",
        "dualscale_headerB",
        "em_validation_q",
        "em_validation_q_tip",
        "em_validation_sq",
        "em_validation_sq_tip",
        "equals_num_value",
        "equation",
        "exclude_all_others",
        "exclude_all_others_auto",
        "hidden",
        "hide_tip",
        "input_boxes",
        "input_size",
        "label_input_columns",
        "location_city",
        "location_country",
        "location_defaultcoordinates",
        "location_mapheight",
        "location_mapservice",
        "location_mapwidth",
        "location_mapzoom",
        "location_nodefaultfromip",
        "location_postal",
        "location_state",
        "max_answers",
        "max_filesize",
        "max_num_of_files",
        "max_num_value",
        "max_num_value_n",
        "max_subquestions",
        "maximum_chars",
        "min_answers",
        "min_num_of_files",
        "min_num_value",
        "min_num_value_n",
        "multiflexible_checkbox",
        "multiflexible_max",
        "multiflexible_min",
        "multiflexible_step",
        "num_value_int_only",
        "numbers_only",
        "other_comment_mandatory",
        "other_numbers_only",
        "other_replace_text",
        "page_break",
        "parent_order",
        "prefix",
        "printable_help",
        "public_statistics",
        "question_template",
        "random_group",
        "random_order",
        "rank_title",
        "repeat_headings",
        "reverse",
        "samechoiceheight",
        "samelistheight",
        "scale_export",
        "show_comment",
        "show_grand_total",
        "show_title",
        "show_totals",
        "showpopups",
        "slider_accuracy",
        "slider_custom_handle",
        "slider_default",
        "slider_default_set",
        "slider_handle",
        "slider_layout",
        "slider_max",
        "slider_middlestart",
        "slider_min",
        "slider_orientation",
        "slider_rating",
        "slider_reset",
        "slider_reversed",
        "slider_separator",
        "slider_showminmax",
        "statistics_graphtype",
        "statistics_showgraph",
        "statistics_showmap",
        "suffix",
        "text_input_columns",
        "text_input_width",
        "time_limit",
        "time_limit_action",
        "time_limit_countdown_message",
        "time_limit_disable_next",
        "time_limit_disable_prev",
        "time_limit_message",
        "time_limit_message_delay",
        "time_limit_message_style",
        "time_limit_timer_style",
        "time_limit_warning",
        "time_limit_warning_2",
        "time_limit_warning_2_display_time",
        "time_limit_warning_2_message",
        "time_limit_warning_2_style",
        "time_limit_warning_display_time",
        "time_limit_warning_message",
        "time_limit_warning_style",
        "use_dropdown",
        "value_range_allows_missing"),

    emailDefaults =
      c(surveyls_email_invite_subj =
          "Invitation to participate in a survey",
        surveyls_email_invite =
          paste0(
            "Dear {FIRSTNAME},<br /><br />you have been invited to ",
            "participate in a survey.<br /><br />The survey is ",
            "titled:<br />\"{SURVEYNAME}\"<br />",
            "<br />\"{SURVEYDESCRIPTION}\"<br /><br />To participate, please ",
            "click on the link below.<br /><br />Sincerely,<br />",
            "<br />{ADMINNAME} ({ADMINEMAIL})<br />",
            "<br />----------------------------------------------<br />Click ",
            "here to do the survey:<br />{SURVEYURL}<br /><br />If you do ",
            "not want to participate in this survey and don't want to ",
            "receive any more invitations please click the following link:",
            "<br />{OPTOUTURL}<br /><br />If you are blacklisted but want to ",
            "participate in this survey and want to receive invitations ",
            "please click the following link:<br />{OPTINURL}"),
        surveyls_email_remind_subj =
          "Reminder to participate in a survey",
        surveyls_email_remind =
          paste0("Dear {FIRSTNAME},<br /><br />Recently we invited you to ",
                 "participate in a survey.<br /><br />We note that you have ",
                 "not yet completed the survey, and wish to remind you that ",
                 "the survey is still available should you wish to take ",
                 "part.<br /><br />The survey is titled:",
                 "<br />\"{SURVEYNAME}\"<br /><br />\"{SURVEYDESCRIPTION}\"<br />",
                 "<br />To participate, please click on the link below.<br />",
                 "<br />Sincerely,<br /><br />{ADMINNAME} ({ADMINEMAIL})<br />",
                 "<br />----------------------------------------------<br />",
                 "Click here to do the survey:<br />{SURVEYURL}<br /><br />If ",
                 "you do not want to participate in this survey and don't want ",
                 "to receive any more invitations please click the following ",
                 "link:<br />{OPTOUTURL}"),
        surveyls_email_register_subj =
          "Survey registration confirmation"	,
        surveyls_email_register =
          paste0("Dear {FIRSTNAME},<br /><br />You, or someone using your ",
                 "email address, have registered to participate in an online ",
                 "survey titled {SURVEYNAME}.<br /><br />To complete this ",
                 "survey, click on the following URL:<br />",
                 "<br />{SURVEYURL}<br /><br />If you have any questions about ",
                 "this survey, or if you did not register to participate and ",
                 "believe this email is in error, please contact {ADMINNAME} ",
                 "at {ADMINEMAIL}."),
        surveyls_email_confirm_subj =
          "Confirmation of your participation in our survey",
        surveyls_email_confirm =
          paste0("Dear {FIRSTNAME},<br /><br />this email is to confirm that ",
                 "you have completed the survey titled {SURVEYNAME} and your ",
                 "response has been saved. Thank you for participating.<br />",
                 "<br />If you have any further questions about this email, ",
                 "please contact {ADMINNAME} on {ADMINEMAIL}.<br /><br />",
                 "Sincerely,<br /><br />{ADMINNAME}"),
        email_admin_notification_subj =
          "Response submission for survey {SURVEYNAME}",
        email_admin_notification =
          paste0("Hello,<br /><br />A new response was submitted for your ",
                 "survey '{SURVEYNAME}'.<br /><br />Click the following link ",
                 "to see the individual response:<br />{VIEWRESPONSEURL}<br />",
                 "<br />Click the following link to edit the individual ",
                 "response:<br />{EDITRESPONSEURL}<br /><br />View statistics ",
                 "by clicking here:<br />{STATISTICSURL}"),
        email_admin_responses_subj =
          "Response submission for survey {SURVEYNAME} with results",
        email_admin_responses =
          paste0("Hello,<br /><br />A new response was submitted for your ",
                 "survey '{SURVEYNAME}'.<br /><br />Click the following link ",
                 "to see the individual response:<br />{VIEWRESPONSEURL}<br />",
                 "<br />Click the following link to edit the individual ",
                 "response:<br />{EDITRESPONSEURL}<br /><br />View statistics ",
                 "by clicking here:<br />{STATISTICSURL}<br /><br /><br />The ",
                 "following answers were given by the participant:",
                 "<br />{ANSWERTABLE}")
        )

  ) ### End of private properties and methods

);

