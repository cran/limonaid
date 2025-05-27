#' Transpose a data frame
#'
#' Returns a list of lists, where each list contains a row.
#'
#' @param x The data frame.
#'
#' @return A list.
#' @export
#'
#' @examples
#' limonaid::transpose_df(
#'   mtcars[1:3, 1:3]
#' );
transpose_df <- function(x) {
  if (!inherits(x, "data.frame")) {
    stop("You have to provide a data frame!");
  }
  return(apply(x, 1, as.list, simplify = FALSE));
}
