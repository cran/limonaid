#' Apply specific code bits from LimeSurvey data import R script
#'
#' This function applies specific code bits from the LimeSurvey data
#' import R script, read by [ls_parse_data_import_script()], for example
#' to update variable names, set labels, etc.
#'
#' @param data The dataframe.
#' @param scriptBits The object returned by the call
#' to [ls_parse_data_import_script()].
#' @param setVarNames,setLabels,convertToCharacter,convertToFactor Whether to
#' set variable names or labels, or convert to character or factor, using the
#' code isolated using the specified regular expression.
#' @param categoricalQuestions Which variables (specified using LimeSurvey
#' variable names) are considered categorical questions; for these, the script
#' to convert the variables to factors, as extracted from the LimeSurvey import
#' file, is applied.
#' @param sticky Whether to make labels sticky (requires the {`sticky`}
#' package).
#' @param massConvertToNumeric Whether to convert all variables to numeric
#' using \code{\link{massConvertToNumeric}}.
#' @param silent Whether to be silent or verbose ('chatty').
#' @return The dataframe.
#' @export
ls_apply_script_bits <- function(data,
                                 scriptBits,
                                 setVarNames = TRUE,
                                 setLabels = TRUE,
                                 convertToCharacter = FALSE,
                                 convertToFactor = FALSE,
                                 categoricalQuestions = NULL,
                                 massConvertToNumeric = TRUE,
                                 silent = limonaid::opts$get("silent"),
                                 sticky = limonaid::opts$get("sticky")) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame, but has class `", class(data), "`.");
  }
  if (!("lsScriptBits" %in% class(scriptBits))) {
    stop("`scriptBits` must have class `scriptBits`, but has class ",
         vecTxt(class(scriptBits), useQuote="`"), ".");
  }

  varNamesScript <- scriptBits$varNamesScript;
  varLabelsScript <- scriptBits$varLabelsScript;
  toCharScript <- scriptBits$toCharScript;
  toFactorScript <- scriptBits$toFactorScript;
  valueLabels <- scriptBits$valueLabels;
  varLabels <- scriptBits$varLabels;

  if (setVarNames) {
    if (!silent) {
      cat0("\nSetting variable names.");
    }
    eval(parse(text=varNamesScript));
  }

  if (convertToCharacter) {
    if (!silent) {
      cat0("\nConverting columns to character.");
    }
    eval(parse(text=toCharScript));
  }

  if (convertToFactor || (!is.null(categoricalQuestions))) {

    if (!silent) {
      cat0("\nConverting columns to factors.");
    }

    if (massConvertToNumeric) {
      data <- massConvertToNumeric(data);
    }

    if (!is.null(categoricalQuestions)) {
      if (setVarNames) {
        varNames <- names(data);
      } else {
        stop("You can't set setVarNames to FALSE and also set ",
             "categoricalQuestions to anything else than NULL, ",
             "because the content of categoricalQuestions should ",
             "be the LimeSurvey variables names!");
      }
      toFactorScript <- unlist(lapply(as.list(categoricalQuestions),
                                      function(x, string=toFactorScript,
                                               varNms=varNames) {
                                        return(grep(paste0("data\\[, ",
                                                           which(varNms==x),
                                                           "\\] <-"),
                                                    string, value=TRUE));
                                      }));
    }

    eval(parse(text=toFactorScript));

  }

  if (sticky) {
    if (requireNamespace("sticky", quietly = TRUE)) {
      data <- sticky::sticky_all(data);
    } else {
      warning("The `sticky` package is not installed. Without this ",
              "package, the variable and value labels that will be ",
              "attached to every variable (i.e. data frame column) ",
              "will be lost when the data frame is subset, for example ",
              "when selecting specific rows or columns.\n\n",
              "You can install the `sticky` package (34KB) with:\n\n",
              "  install.packages('sticky');\n\n",
              "You can disable this warning by setting the `sticky` ",
              "argument to `FALSE`.");
    }
  }

  ### Labels are set as last action, because other actions
  ### sometimes erase attributes
  if (setLabels) {
    if (!silent) {
      cat0("\nSetting variable labels.");
    }
    ### This is the default attribute
    eval(parse(text=varLabelsScript));
    ### Also apply to `label`, to be consistent with e.g. haven etc
    varLabelsScript <- gsub("variable\\.labels",
                            "label",
                            varLabelsScript);
    eval(parse(text=varLabelsScript));

    if (!silent) {
      cat0("\nStoring variable labels as variable attributes following `labeler` convention.");
    }
    for (i in names(varLabels)) {
      attr(data[, as.numeric(i)], "label") <-
        varLabels[[i]];
    }

    if (!silent) {
      cat0("\nStoring value labels as variable attributes following `labeler` convention.");
    }
    for (i in names(valueLabels)) {
      attr(data[, as.numeric(i)], "labels") <-
        valueLabels[[i]];
    }
  }

  return(data);


}
