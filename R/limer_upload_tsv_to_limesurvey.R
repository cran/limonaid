#' Upload a tab separated limesurvey text file
#'
#' To use this function, you need to setup R for the LimeSuevey API, as
#' described in `vignette("limesurvey_api_setup")`.
#'
#' @param ls_txt_path Path of the limesurvey text file
#' @param open_url Character vector containing one or more of the strings in
#'   `c("preview", "survey", "none")`. If it contains "none", nothing is done.
#'   `"preview"` (the default) previews the survey on limesurvey. `"survey"`
#'   opens the survey summary.
#' @param hostname The host to use (if not using the one specified in
#' the options). If no hostname is specified in the `'lime_api'` option and
#' no host name is passed as `hostname`, the subdomain stored in
#' `limonaid::opts$get("ls_subdomain")` will be combined with the
#' domain stored in `limonaid::opts$get("ls_domain")` to create the
#' host name. You can change these using the `limonaid::opts$set()` function.
#'
#' @return The value of the id of your survey in the specified
#' LimeSurvey installation,
#'
#' @export
#' @examples
#'
#' \dontrun{
#' ### Log into the LimeSurvey API:
#' limonaid::get_session_key();
#'
#' ### Upload a tab separated values file:
#' limer_upload_tsv_to_limesurvey(
#'   "PATH/TO/YOUR/LIMESURVEY/TXT FILE",
#'   c("preview", "survey")
#' );
#' }
limer_upload_tsv_to_limesurvey <- function(ls_txt_path,
                                           open_url = "preview",
                                           hostname = getOption('lime_api')) {

  if (is.null(hostname)) {
    hostname <-
      paste0(
        limonaid::opts$get("ls_subdomain"),
        ".",
        limonaid::opts$get("ls_domain")
      );
  }

  open_url <- match.arg(
    open_url,
    c("preview", "survey", "none"),
    several.ok = TRUE
  );

  ### Switched to jsonlite since we're dependent on that already
  survey64 <- jsonlite::base64_enc(ls_txt_path);

  survey_params <- list(
    sImportData = survey64,
    sImportDataType = "txt"
  );

  survey_id <-
    limonaid::limer_call_limer(
      method = "import_survey",
      params = survey_params
    );

  ### Preview survey - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  if ("none" %in% open_url) {
    ### Returning visibly because we're not previewing
    return(survey_id);
  }

  if ("preview" %in% open_url) {

    preview_survey_url <- paste0(
      "https://",
      hostname,
      "/",
      survey_id
    );

    utils::browseURL(preview_survey_url);

  }

  if ("survey" %in% open_url) {

    survey_url <- paste0(
      "https://",
      hostname,
      "/surveyAdministration/view?surveyid=",
      survey_id
    );

    utils::browseURL(survey_url);

  }

  return(invisible(survey_id));

}
