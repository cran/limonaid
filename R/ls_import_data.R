#' Reading LimeSurvey data exported to R
#'
#' This function can be used to import files exported by LimeSurvey.
#'
#' This function was intended to make importing data from LimeSurvey a bit
#' easier. The default settings used by LimeSurvey are not always convenient,
#' and this function provides a bit more control.
#'
#' @param sid,path The easiest way to load data is to not rename the datafile
#' and script file downloaded from LimeSurvey (so that both contain the Survey
#' Identifier, the `sid`) and simply specify that `sid` and the path where both
#' files are stored.
#' @param datafile The path and filename of the file containing the data (comma
#' separated values).
#' @param dataPath,datafileRegEx Path containing datafiles: this can be used to
#' read multiple datafiles, if the data is split between those. This is useful
#' when downloading the entire datafile isn't possible because of server
#' restrictions, for example when the processing time for the script in
#' LimeSurvey that generates the datafiles is limited. In that case, the data
#' can be downloaded in portions, and specifying a path here enables reading
#' all datafiles in one go. Use the regular expression to indicate which files
#' in the path should be read.
#' @param scriptfile The path and filename of the file containing the R script
#' to import the data.
#' @param setVarNames,setLabels,convertToCharacter,convertToFactor Whether to
#' set variable names or labels, or convert to character or factor, using the
#' code isolated using the specified regular expression.
#' @param categoricalQuestions Which variables (specified using LimeSurvey
#' variable names) are considered categorical questions; for these, the script
#' to convert the variables to factors, as extracted from the LimeSurvey import
#' file, is applied.
#' @param massConvertToNumeric Whether to convert all variables to numeric
#' using \code{\link{massConvertToNumeric}}.
#' @param dataHasVarNames Whether the variable names are included as header
#' (first line) in the comma separated values file (data file).
#' @param dataEncoding,scriptEncoding The encoding of the files; can be used
#' to override the setting in the `limonaid` options (i.e. in `opts`) in the
#' `encoding` field (the default value is "`UTF-8`").
#' @param sticky Whether to make labels sticky (requires the {`sticky`}
#' package).
#' @param silent Whether to be silent or verbose ('chatty').
#' @return The dataframe.
#' @examples
#'
#' \dontrun{
#' ### Of course, you need valid LimeSurvey files. This is an example of
#' ### what you'd do if you have them, assuming you specified that path
#' ### containing the data in 'dataPath', the name of the datafile in
#' ### 'dataFileName', the name of the script file in 'dataLoadScriptName',
#' ### and that you only want variables 'informedConsent', 'gender', 'hasJob',
#' ### 'currentEducation', 'prevEducation', and 'country' to be converted to
#' ### factors.
#' dat <- limonaid::ls_import_data(
#'   datafile = file.path(dataPath, dataFileName),
#'   scriptfile = file.path(dataPath, dataLoadScriptName),
#'   categoricalQuestions = c('informedConsent',
#'                            'gender',
#'                            'hasJob',
#'                            'currentEducation',
#'                            'prevEducation',
#'                            'country')
#' );
#' }
#'
#' @export
ls_import_data <- function(
  sid = NULL,
  path = NULL,
  datafile = NULL,
  dataPath = NULL,
  datafileRegEx = NULL,
  scriptfile = NULL,
  setVarNames = TRUE,
  setLabels = TRUE,
  convertToCharacter = FALSE,
  convertToFactor = FALSE,
  categoricalQuestions = NULL,
  massConvertToNumeric = TRUE,
  dataHasVarNames = TRUE,
  dataEncoding="UTF-8-BOM", #'UTF-8', 'unknown',
  scriptEncoding=NULL, # 'ASCII'
  sticky = limonaid::opts$get("sticky"),
  silent = limonaid::opts$get("silent")) {

  # limeSurveyRegEx.varNames <-
  #   limonaid::opts$get("data_import_RegEx_varNames");
  # limeSurveyRegEx.toChar <-
  #   limonaid::opts$get("data_import_RegEx_toChar");
  # limeSurveyRegEx.varLabels <-
  #   limonaid::opts$get("data_import_RegEx_varLabels");
  # limeSurveyRegEx.toFactor <-
  #   limonaid::opts$get("data_import_RegEx_toFactor");
  limeSurveyRegEx.varNameSanitizing <-
    limonaid::opts$get("data_import_RegEx_varNameSanitizing");

  if (is.null(dataEncoding)) {
    dataEncoding <- limonaid::opts$get("encoding");
  }
  if (is.null(scriptEncoding)) {
    scriptEncoding <- limonaid::opts$get("encoding");
  }

  if (!is.null(sid) && !is.null(path)) {
    if (!silent) {
      cat0("\nReading data and analysis script for survey ",
           sid, " from path '", path, "'.");
    }
    dataPath <- path;
    datafileRegEx <- paste0(".*", sid, ".*\\.csv$");
    scriptfile <-
      file.path(
        path,
        list.files(
          path = path,
          pattern = paste0(".*", sid, ".*\\.R$")
        )[1]
      );
  }

  ### Set filename(s) to read
  if (!is.null(dataPath) && !is.null(datafileRegEx)) {
    files <- unique(list.files(path = dataPath,
                               pattern = datafileRegEx,
                               ignore.case = TRUE,
                               recursive=TRUE,
                               full.names=TRUE));

  } else if (!is.null(datafile)) {
    if (!file.exists(datafile)) {
      stop("File specified as datafile ('", datafile, "') does not exist!");
    } else {
      files <- datafile;
    }
  } else {
    stop("Please specify a datafile to read, or a datafileRegEx to read multiple datafiles!");
  }

  ### Load datafile(s)
  data <- NULL;
  for (currentDatafile in files) {
    if (dataHasVarNames) {
      if (!silent) {
        cat0("\nReading datafile (with variable names) from '",
             currentDatafile, "'.");
      }
      currentData <-
        utils::read.csv(currentDatafile,
                        quote = "'\"",
                        na.strings=c("", "\"\""),
                        stringsAsFactors=FALSE,
                        encoding=dataEncoding,
                        header=TRUE);
    } else {
      if (!silent) {
        cat0("\nReading datafile (without variable names) from '",
             currentDatafile, "'.");
      }
      currentData <-
        utils::read.csv(currentDatafile,
                        quote = "'\"",
                        na.strings=c("", "\"\""),
                        stringsAsFactors=FALSE,
                        encoding=dataEncoding,
                        header=FALSE);
    }
    if (is.null(data)) {
      data <- currentData;
    } else {
      data <- rbind(data, currentData);
    }
  }

  ### Load scriptfile
  if (!is.null(scriptfile)) {

    scriptBits <-
      ls_parse_data_import_script(
        scriptfile = scriptfile,
        scriptEncoding=scriptEncoding,
        silent=silent
      );

    if (!silent) {
      cat0("\nProcessing scripts depending on values of `setVarNames` (",
           setVarNames, "), `setLabels` (", setLabels,
           "), `convertToCharacter` (", convertToCharacter,
           "), `convertToFactor` (", convertToFactor,
           "), and `categoricalQuestions` (", categoricalQuestions, ").");
    }

    data <-
      ls_apply_script_bits(data = data,
                           scriptBits = scriptBits,
                           setVarNames = setVarNames,
                           setLabels = setLabels,
                           convertToCharacter = convertToCharacter,
                           convertToFactor = convertToFactor,
                           categoricalQuestions = categoricalQuestions,
                           massConvertToNumeric = massConvertToNumeric,
                           sticky = sticky,
                           silent = silent);

  } else {
    if (massConvertToNumeric) {
      data <- massConvertToNumeric(data);
    }
  }

  if (length(limeSurveyRegEx.varNameSanitizing)) {
    if (!silent) {
      cat0("\nSanitizing variable names.");
    }
    for (currentRegexPair in limeSurveyRegEx.varNameSanitizing) {
      names(data) <- gsub(currentRegexPair$pattern,
                          currentRegexPair$replacement,
                          names(data));
    }
  }

  return(data);
}
