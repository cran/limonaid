#' Extract specific code bits from LimeSurvey data import R script
#'
#' This function extracts specific code bits from the LimeSurvey data
#' import R script, which can then be applied to imported data using
#' [ls_apply_script_bits()], for example to update variable names, set
#' labels, etc.
#'
#' @param scriptfile The path and filename of the script file.
#' @param scriptEncoding The encoding of the script file; can be used
#' to override the setting in the `limonaid` options (i.e. in `opts`) in the
#' `encoding` field (the default value is "`UTF-8`").
#' @param silent Whether to be silent or verbose ('chatty').
#'
#' @return A list with four components.
#' @export
ls_parse_data_import_script <- function(scriptfile = NULL,
                                        scriptEncoding=limonaid::opts$get("encoding"),
                                        silent=limonaid::opts$get("silent")) {

  limeSurveyRegEx.varNames <-
    limonaid::opts$get("data_import_RegEx_varNames");
  limeSurveyRegEx.toChar <-
    limonaid::opts$get("data_import_RegEx_toChar");
  limeSurveyRegEx.varLabels <-
    limonaid::opts$get("data_import_RegEx_varLabels");
  limeSurveyRegEx.toFactor <-
    limonaid::opts$get("data_import_RegEx_toFactor");

  if (!file.exists(scriptfile)) {
    stop("File specified as scriptfile ('", scriptfile, "') does not exist!");
  }
  if (!silent) {
    cat0("\nReading script file from '",
         scriptfile, "'.");
  }

  ### Use separate connection to make sure proper encoding is selected
  con <- file(scriptfile, encoding=scriptEncoding)
  datascript <- readLines(con, warn=FALSE);
  close(con);

  if (!silent) {
    cat0("\nApplying regular expressions to script file contents to ",
         "extract lines to set variable names, labels, convert to ",
         "character values, and convert to factors.");
  }

  res <-
    list(
      varNamesScript = datascript[grepl(limeSurveyRegEx.varNames,
                                        datascript)],
      varLabelsScript = datascript[grepl(limeSurveyRegEx.varLabels,
                                         datascript)],
      toCharScript = datascript[grepl(limeSurveyRegEx.toChar,
                                      datascript)],
      toFactorScript = datascript[grepl(limeSurveyRegEx.toFactor,
                                        datascript)]
    );

  res$valueLabels <-
    regmatches(
      res$toFactorScript,
      regexec(
        limeSurveyRegEx.toFactor,
        res$toFactorScript
      )
    );

  res$valueLabels <-
    stats::setNames(
      lapply(
        res$valueLabels,
        function(x) {
          
          ### 2025-01-08: when importing a file with an answer option
          ### label with an emoji (😭), the last parenthesis isn't removed
          ### when reading the regex. Added this hacky check to force that
          ### if need be (no time to figure out why this is suddenly
          ### happening now)
          x[[4]] <- gsub("\\)\\)$", ")", x[[4]]);
          
          eval(parse(text = paste0("y<-", x[[4]])));
          eval(parse(text = paste0("z<-", x[[3]])));
          return(stats::setNames(get('y'), nm = get('z')));
        }),
      nm = unlist(
        lapply(
          res$valueLabels,
          function(x) {
            return(x[[2]]);
          })
      )
    );

  res$varLabels <-
    regmatches(
      res$varLabelsScript,
      regexec(
        limeSurveyRegEx.varLabels,
        res$varLabelsScript
      )
    );

  res$varLabels <-
    stats::setNames(
      lapply(
        res$varLabels,
        function(x) {
          return(x[[3]]);
        }),
      nm = unlist(
        lapply(
          res$varLabels,
          function(x) {
            return(x[[2]]);
          })
      )
    );

  class(res) <- c("lsScriptBits", "list");

  return(res);

}
