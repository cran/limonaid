###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

testthat::test_that("exporting a group to LSG works", {

  ### Example from Group

  myGroup <- limonaid::Group$new(
    group_name = "My Group"
  );
  myGroup$add_question(
    "testQuestion1",
    questionTexts = "First question",
    type="free text (short)"
  );
  myGroup$add_question(
    "testQuestion2",
    questionTexts = "Second question",
    type="radio"
  );
  myGroup$questions$testQuestion2$add_answer_option(
    "1",
    "First option"
  );
  myGroup$questions$testQuestion2$add_answer_option(
    "2",
    "Second option"
  );
  # myGroup$questions$testQuestion2$add_subquestion(
  #   "subquestion1",
  #   "First subquestion"
  # );
  # myGroup$questions$testQuestion2$add_subquestion(
  #   "subquestion2",
  #   "Second subquestion"
  # );

  tempFile <- tempfile(fileext=".lsg");

  writingResult <-
    myGroup$export_to_lsg(file = tempFile);

  # cat(tempFile);

  xmlAsList <- xml2::as_list(writingResult);

  testthat::expect_equal(
    xmlAsList$document$answer_l10ns$rows[[1]]$answer[[1]],
    "First option"
  );

})
