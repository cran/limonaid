#' Write a data frame to a LimeSurvey Tab Separated Values file
#'
#' @param data The dataframe to write.
#' @param file The file to write to.
#' @param encoding The encoding to write to.
#' @param preventOverwriting Whether to prevent overwriting, should the
#' target file exist, already.
#' @param silent Whether to be silent or chatty.
#'
#' @return The dataframe, adapted for writing, invisibly.
#'
#' @examples ### Add example once something is available.
#' @export
ls_write_lsg <- function(data,
                         file,
                         encoding = limonaid::opts$get("encoding"),
                         preventOverwriting = limonaid::opts$get("preventOverwriting"),
                         silent = limonaid::opts$get("silent")) {

  ###-----------------------------------------------------------------------
  ### Check whether the directory to write to exists
  ###-----------------------------------------------------------------------

  dirToWriteTo <- dirname(file);
  if (nchar(dirToWriteTo) > 0) {
    if (!dir.exists(dirToWriteTo)) {
      stop("The directory you specified to write the output file to, '",
           dirToWriteTo, "', does not exist.");
    }
  }

  ###-----------------------------------------------------------------------
  ### Check whether the file exists
  ###-----------------------------------------------------------------------
  if (file.exists(file) && preventOverwriting) {
    stop("The filename you specified to write the output file to, '",
         file, "', already exists, and `preventOverwriting` is set to ",
         "TRUE, so I'm aborting.");
  }

  ###-----------------------------------------------------------------------
  ### Prepare data
  ###-----------------------------------------------------------------------

  ### Quote relevance equations if necessary
  data$relevance <-
    ifelse(
      data$relevance == 1,
      "1",
      ifelse(
        is.na(data$relevance) | (nchar(data$relevance) == 0),
        "",
        paste0("\"",
               data$relevance,
               "\"")
      )
    );

  ### Double quotes
  data$text <-
    gsub("\"",
         "\"\"",
         data$text);

  ### Quote strings
  data$text <-
    ifelse(grepl(" |@",
                 data$text),
           paste0("\"",
                  data$text,
                  "\""),
           data$text);

  ### Quote strings
  data$name <-
    ifelse(grepl(" |@",
                 data$name),
           paste0("\"",
                  data$name,
                  "\""),
           data$name);

  ### Fix slash in "type/scale" column name
  colNames <- names(data);
  colNames[colNames == "type.scale"] <-
    "type/scale";

  if (trimws(tolower(encoding)) == "utf-8") {

    ###-------------------------------------------------------------------------
    ### 'Manually' glue all columns together with tabs and rows with newlines
    ###-------------------------------------------------------------------------

    fileToWrite <-
      paste(apply(data, 1, paste, collapse="\t"), collapse="\n");

    ### Add the column names
    fileToWrite <-
      paste(paste(colNames, collapse="\t"),
            "\n",
            fileToWrite);

    ### Using
    ### https://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/

    fileToWrite <- enc2utf8(fileToWrite);

    con <- file(file,
                open = "w",
                encoding="native.enc"); #"UTF-8");
    writeLines(fileToWrite, con = con, useBytes=TRUE);
    close(con);

  } else {

    if (trimws(tolower(encoding)) == "default-utf-8") {
      encoding <- "UTF-8";
    }

    ###-------------------------------------------------------------------------
    ### Writing file
    ###-------------------------------------------------------------------------

    ### Write file
    utils::write.table(
      data,
      file = file,
      col.names = colNames,
      sep = "\t",
      na = "",
      quote = FALSE,
      row.names = FALSE,
      qmethod = "double",
      fileEncoding = encoding
    );

  }

  return(invisible(data));

}
