#' Make a call to the LimeSurvey API
#'
#' This function makes a generic call to the LimeSurvey API.
#' See \url{https://www.limesurvey.org/manual/RemoteControl_2_API} for
#' API documentation. It was
#' adapted by Gjalt-Jorn Peters from a function originally written by Andrew
#' Heiss.
#'
#' @param method API function to call. Full lis Defaults to value set in \code{options()}.
#' @param params Optional named list of parameters to pass to the function.
#' @param encoding The encoding to use
#' @param \dots Other arguments passed to \code{\link[httr]{POST}}.
#' @return Results from the API (sometimes plain text, sometimes base64-encoded text).
#' @examples \dontrun{
#' limer_call_limer(method = "list_surveys")
#' limer_call_limer(method = "get_summary",
#'                  params = list(iSurveyID = 238481,
#'                                sStatname = "completed_responses"))
#' }
#' @export

limer_call_limer <- function(method, params = list(), ..., encoding="utf-8") {

  if (!is.list(params)) {
    stop("params must be a list.")
  }

  if (exists("session_key", envir = session_cache)) {

    key.list <- list(sSessionKey = session_cache$session_key);

  } else if (is.logical(getOption("limonaid_cache_session_key", FALSE))) {

    key.list <- list(sSessionKey = getOption("limonaid_cache_session_key", FALSE));

  } else {

    stop("You need to get a session key first. Run limonaid::limer_get_session_key().");

  }

  params.full <- c(
    key.list,
    params
  );

  body.json <-
    list(
      method = method,
      # This seems to not matter, but the API call breaks without it,
      # so just pass nothing. ¯\_(ツ)_/¯
      id = " ",
      params = params.full
    );

  r <-
    httr::POST(
      getOption('lime_api'),
      httr::content_type_json(),
      body = jsonlite::toJSON(
        body.json,
        auto_unbox = TRUE
      ),
      ...
    );

  res <-
    jsonlite::fromJSON(
      httr::content(
        r,
        as='text',
        encoding=encoding
      )
    )$result;   # incorporated fix by petrbouchal

  return(res);

}
