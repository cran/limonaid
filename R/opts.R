#' Options for the limonaid package
#'
#' The `limonaid::opts` object contains three functions to set, get, and reset
#' options used by the escalc package. Use `limonaid::opts$set` to set options,
#' `limonaid::opts$get` to get options, or `limonaid::opts$reset` to reset specific or
#' all options to their default values.
#'
#' It is normally not necessary to get or set `limonaid` options.
#'
#' The following arguments can be passed:
#'
#' \describe{
#'   \item{...}{For `limonaid::opts$set`, the dots can be used to specify the options
#'   to set, in the format `option = value`, for example,
#'   `silent = FALSE`. For
#'   `limonaid::opts$reset`, a list of options to be reset can be passed.}
#'   \item{option}{For `limonaid::opts$set`, the name of the option to set.}
#'   \item{default}{For `limonaid::opts$get`, the default value to return if the
#'   option has not been manually specified.}
#' }
#'
#' The following options can be set:
#'
#' \describe{
#'
#'   \item{silent}{Whether to be chatty or silent.}
#'
#'   \item{encoding}{The encoding to use when writing files.}
#'
#'   \item{preventOverwriting}{The name of the column
#'   with the missing values.}
#'
#' }
#'
#' @aliases opts set get reset
#'
#' @usage opts
#'
#' @examples ### Get the default silent setting
#' limonaid::opts$get('silent');
#'
#' ### Set it to FALSE
#' limonaid::opts$set(silent = FALSE);
#'
#' ### Check that it worked
#' limonaid::opts$get('silent');
#'
#' ### Reset this option to its default value
#' limonaid::opts$reset('silent');
#'
#' ### Check that the reset worked, too
#' limonaid::opts$get('silent');
#'
#' @export
opts <- list();

opts$set <- function(...) {
  dots <- list(...);
  dotNames <- names(dots);
  names(dots) <-
    paste0("limonaid.", dotNames);
  if (all(dotNames %in% names(opts$defaults))) {
    do.call(options,
            dots);
  } else {
    stop("Option '", option, "' is not a valid (i.e. existing) option for limonaid!");
  }
}

opts$get <- function(option, default=FALSE) {
  ### manual to prevent problems with devtools:document
  if (option == "workspace_id") {
    return(getOption("limonaid.workspace_id",
                     "default"));
  }
  optionName <- as.character(substitute(option));
  if (!optionName %in% names(opts$defaults)) {
    stop("Option '", optionName, "' is not a valid (i.e. existing) option for limonaid!");
  } else {
    return(getOption(paste0("limonaid.", optionName),
                     opts$defaults[[optionName]]));
  }
}

opts$reset <- function(...) {
  optionNames <-
    unlist(lapply(as.list(substitute(...())),
                  as.character));
  if (length(optionNames) == 0) {
    do.call(opts$set,
            opts$defaults);
  } else {
    prefixedOptionNames <-
      paste0("limonaid.", optionNames);
    if (all(optionNames %in% names(opts$defaults))) {
      do.call(opts$set,
              opts$defaults[optionNames]);
    } else {
      invalidOptions <-
        !(optionNames %in% names(opts$defaults));
      stop("Option(s) ", vecTxtQ(optionNames[invalidOptions]),
           "' is/are not a valid (i.e. existing) option for limonaid!");
    }
  }
}

opts$defaults <-
  list(

    preventOverwriting=TRUE,

    encoding = "UTF-8",

    debug = FALSE,

    maxCores = NULL,

    recTab2Eq_varCodeCol = "varCode",
    recTab2Eq_valueCol = "value",
    recTab2Eq_recodeToCol = "recode",
    recTab2Eq_operatorCol = NULL,

    eq_elseExpr = "",

    ### For importing LimeSurvey files

    data_import_RegEx_varNames =
      "names\\(data\\)\\[\\d*\\] <- ",

    data_import_RegEx_toChar =
      "data\\[, \\d*\\] <- as.character\\(data\\[, \\d*\\]\\)",

    data_import_RegEx_varLabels =
      "attributes\\(data\\)\\$variable.labels\\[(\\d*)\\] <- \"(.*)\"",

    data_import_RegEx_toFactor =
      paste0("data\\[, (\\d*)\\] <- ",
             "factor\\(data\\[, \\d*\\], ",
             "levels=(c\\(.*\\)),.*labels=(c\\(.*\\))\\)"),

    data_import_RegEx_varNameSanitizing =
      list(list(pattern = "#", replacement = "_"),
           list(pattern = "\\$", replacement = ".")),

    ### Regular expressions to use when parsing LimeSurvey
    ### labels in processLSvarLabels

    labelExtractionRegExPair = c("\\[(.*)\\].*", "\\1"),

    leftAnchorRegExPairs = list(
      c(".*[[:graph:]]\\s*([A-Z][a-z][^|]*)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*\\S\\.\\.\\.(\\S[^|]+)\\|(.+)",
        "\\1"),
      c(".*\\.([^|]+)\\|(.+)",
        "\\1"),
      c(".*\u2026(\\S[^|]+)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*:([^|]+)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*\\?([^|]+)\\s*\\|\\s*(.+)",
        "\\1"),
      c(".*\\S\u2026(\\S[^|]+)\\|(.+)",
        "\\1")
    ),

    rightAnchorRegExPairs = list(
      c(".*[[:graph:]]\\s*([A-Z][a-z][^|]*)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\\.\\.\\.([^|]+)\\|(.+)",
        "\\2"),
      c(".*\\.([^|]+)\\|(.+)",
        "\\2"),
      c(".*:([^|]+)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\u2026([^|]+)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\\?([^|]+)\\s*\\|\\s*(.+)",
        "\\2"),
      c(".*\\S\u2026(\\S[^|]+)\\|(.+)",
        "\\2")
    ),

    ls_domain = "limesurvey.net",

    ls_subdomain = "account",

    sticky = TRUE,

    silent=TRUE

  )

