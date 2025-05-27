na_if_null <- function(x) {
  if (is.null(x)) {
    return(NA);
  } else {
    return(x);
  }
}
