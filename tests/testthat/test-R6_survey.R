###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("creating a simple survey works", {
  ls <- Survey$new("Test Survey");
  testthat::expect_equal(ls$titles, c(en = "Test Survey"));
})

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("adding groups works", {
  ls <- Survey$new("Test Survey");
  ls$add_group("Group 1");
  ls$add_group("Group 2");
  testthat::expect_equal(ls$groups$`1`$titles[['en']], "Group 1");
})

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("adding a question works", {
  ls <- Survey$new("Test Survey");
  ls$add_group("Group 1");
  ls$add_question(groupId = 1,
                  code = "firstQuestion",
                  type = "radiobuttons");
  testthat::expect_equal(ls$groups$`1`$questions$firstQuestion$code,
                         "firstQuestion");
})

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("exporting a survey to a TSV file works", {

  ### CRAN tests only allow 2 cores max, and the trick from Stack Exchange
  ### seems to not work properly.
  limonaid::opts$set(maxCores = 2)

  ls <- limonaid::Survey$new("Test Survey");
  ls$add_group("Only group",
               "This is the description of the only group in the survey.");
  ls$add_question(groupId = 1,
                  code = "onlyQuestion",
                  questionTexts = "This is the question text of the only question in this survey.",
                  helpTexts = "This is the help text of the only question in this survey.",
                  type = "radiobuttons");

  ls$groups$`1`$questions$onlyQuestion$add_answer_option(
    code = 1,
    optionTexts = "This is the first answer option");

  ls$groups$`1`$questions$onlyQuestion$add_answer_option(
    code = 2,
    optionTexts = "This is the second answer option");

  tmpFile <- tempfile(fileext = ".txt");

  ### Temporary
  #tmpFile <- "B:/Data/R/limonaid/inst/extdata/testing-export.txt";

  ls$export_to_tsv(file = tmpFile,
                   preventOverwriting = FALSE);

  cat(tmpFile);

  testthat::expect_equal(ls$groups$`1`$questions$onlyQuestion$code,
                         "onlyQuestion");
})

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("exporting a survey with two languages to a TSV file works", {

  ### CRAN tests only allow 2 cores max, and the trick from Stack Exchange
  ### seems to not work properly.
  limonaid::opts$set(maxCores = 2)

  ls <- limonaid::Survey$new(c(en = "Test Survey",
                               nl = "Uittestvragenlijst"));

  ls$add_group(c(en = "Only group",
                 nl = "Enige groep"),
               c(en = "This is the description of the only group in the survey.",
                 nl = "Beschrijving van de enige groep"));

  ls$add_question(groupId = 1,
                  code = "onlyQuestion",
                  questionTexts =
                    c(en = "This is the question text of the only question in this survey.",
                      nl = "Vraagtekst"),
                  helpTexts =
                    c(en = "This is the help text of the only question in this survey.",
                      nl = "Hulptekst"),
                  type = "radiobuttons");

  ls$groups$`1`$questions$onlyQuestion$add_answer_option(
    code = 1,
    optionTexts = c(en = "This is the first answer option",
                    nl = "Dit is antwoordoptie 1"));

  ls$groups$`1`$questions$onlyQuestion$add_answer_option(
    code = 2,
    optionTexts = c(en = "This is the second answer option",
                    nl = "Antwoordoptie 2"));

  tmpFile <- tempfile(fileext = ".txt");

  ### Temporary
  #tmpFile <- "B:/Data/R/limonaid/inst/extdata/testing-multilingual-export.txt";

  ls$export_to_tsv(file = tmpFile,
                   preventOverwriting = FALSE);

  cat(tmpFile);

  testthat::expect_equal(ls$groups$`1`$questions$onlyQuestion$code,
                         "onlyQuestion");
})

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("exporting an en/nl survey with a slider works", {

  ### CRAN tests only allow 2 cores max, and the trick from Stack Exchange
  ### seems to not work properly.
  limonaid::opts$set(maxCores = 2);

  ls <- limonaid::Survey$new(c(en = "Test Survey",
                               nl = "Uittestvragenlijst"));

  ls$add_group(c(en = "First group",
                 nl = "Eerste groep"),
               c(en = "First group description",
                 nl = "Beschrijving van de eerste groep"));

  firstGroupId <- as.character(ls$get_group_ids);

  ls$add_question(groupId = firstGroupId,
                  code = "q1",
                  questionTexts =
                    c(en = "Q1 question text",
                      nl = "Q1 vraagtekst"),
                  helpTexts =
                    c(en = "Q1 help text",
                      nl = "Q1 hulptekst"),
                  type = "radiobuttons");

  ls$groups[[firstGroupId]]$questions$q1$add_answer_option(
    code = 1,
    optionTexts = c(en = "Q1, A1, in English",
                    nl = "Q1, A1, in Dutch"));

  ls$groups[[firstGroupId]]$questions$q1$add_answer_option(
    code = 2,
    optionTexts = c(en = "Q1, A2, in English",
                    nl = "Q1, A2, in Dutch"));

  ls$add_group(c(en = "Second group",
                 nl = "Tweede groep"),
               c(en = "Second group description",
                 nl = "Beschrijving van de tweede groep"));

  secondGroupId <- as.character(ls$get_group_ids[2]);

  ls$add_question(groupId = secondGroupId,
                  code = "sliderQuestion",
                  questionTexts =
                    c(en = "Q2 question text",
                      nl = "Q2 vraagtekst"),
                  helpTexts =
                    c(en = "Q2 help text",
                      nl = "Q2 hulptekst"),
                  type = "slider",
                  slider_min = 1,
                  slider_max = 25,
                  slider_default = 5);



  ls$groups[[secondGroupId]]$questions$sliderQuestion$add_subquestion(
    code = "nr",
    subquestionTexts = "subquestion text"
  );

  tmpFile <- tempfile(fileext = ".txt");

  ### Temporary
  #tmpFile <- "B:/Data/R/limonaid/inst/extdata/testing-multilingual-slider-export.txt";

  ls$export_to_tsv(file = tmpFile,
                   preventOverwriting = FALSE,
                   silent=TRUE);

  cat(tmpFile);

  testthat::expect_equal(ls$groups$`1`$questions$q1$answerOptions$`1`$code,
                         1);
})

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("autofixing multiple choice questions works", {

  ### CRAN tests only allow 2 cores max, and the trick from Stack Exchange
  ### seems to not work properly.
  limonaid::opts$set(maxCores = 2)

  ls <- limonaid::Survey$new(c(en = "Test Survey",
                               nl = "Uittestvragenlijst"));

  ls$add_group(c(en = "First group",
                 nl = "Eerste groep"));

  ls$add_question(groupId = 1,
                  code = "q1",
                  questionTexts =
                    c(en = "Q1 question text",
                      nl = "Q1 vraagtekst"),
                  helpTexts =
                    c(en = "Q1 help text",
                      nl = "Q1 hulptekst"),
                  type = "checkboxes");

  ls$groups$`1`$questions$q1$add_answer_option(
    code = 1,
    optionTexts = c(en = "Q1, A1, in English",
                    nl = "Q1, A1, in Dutch"));

  ls$groups$`1`$questions$q1$add_answer_option(
    code = 2,
    optionTexts = c(en = "Q1, A2, in English",
                    nl = "Q1, A2, in Dutch"));

  tmpFile <- tempfile(fileext = ".txt");

  ### Temporary
  tmpFile <- tempfile(pattern = "testing-checkbox-autofix-export",
                      fileext = ".txt");

  ls$export_to_tsv(file = tmpFile,
                   preventOverwriting = FALSE);

  cat(tmpFile);

  testthat::expect_equal(ls$groups$`1`$questions$q1$code,
                         "q1");

})

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
