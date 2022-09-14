##' Show information from user TileDB Cloud user profile
##'
##' This function shows user information for the  currently logged-in account on
##' TileDB Cloud.
##'
##' Nominally you will first call \code{\link{login}}; if not, the results of the last
##' login at \code{~/.tiledb/cloud.json} will be used.
##'
##' @param include_logo If set to True, include the \code{logo} field in the return value.
##' By default this is omitted since it's a long base64-encoded string which takes up
##' a lot of screen space and is likely uninteresting.
##'
##' @return A list of user properties from the currently logged-in TileDB cloud account.
##'
##' @family {manual-layer functions}
##' @export
user_profile <- function(include_logo=FALSE) {
  apiClientInstance <- get_api_client_instance()
  userApiInstance <- UserApi$new(apiClientInstance)

  resultObject <- userApiInstance$GetUser()
  # Decode the result
  body <- .get_raw_response_body_or_stop(resultObject)
  info <- jsonlite::fromJSON(rawToChar(body))

  if (!include_logo) {
    info[["logo"]] <- NULL
  }
  info
}
