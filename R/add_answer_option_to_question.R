#' Add an answer option to a question
#'
#' This is a convenience function that allows you to add an answer option to
#' a question object.
#'
#' @param question The limonaid Question object
#' @param ... Options that are passed on to the Question's `add_answer_option()`
#' method.
#'
#' @return The question object.
#' @export
#'
#' @examples
#' myQuestion <-
#'   limonaid::Question$new(
#'     code = 'myQuestion',
#'     type='radio'
#'   ) |>
#'   add_answer_option_to_question(
#'     code = 1,
#'     optionTexts = "First option"
#'   ) |>
#'   add_answer_option_to_question(
#'     code = 2,
#'     optionTexts = "Second option"
#'   );
add_answer_option_to_question <- function(question,
                                          ...) {
  if ((!inherits(question, "R6")) || (!inherits(question, "Question"))) {
    stop("As `question`, you must pass a {limonaid} Question object!");
  }
  return(question$add_answer_option(...));
}
