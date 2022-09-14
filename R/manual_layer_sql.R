##' Execute a SQL query on TileDB Cloud
##'
##' This invokes a user-defined function in TileDB Cloud.
##'
##' Nominally you will first call \code{\link{login}}; if not, the results
##' of the last login at \code{~/.tiledb/cloud.json} will be used.
##'
##' The \code{udf} and \code{namespace} arguments are required; the \code{args}
##' argument is optional.
##'
##' @param query SQL query as a string.
##'
##' @param name A descriptive name to give the query.
##'
##' @param namespace Namespace within TileDB cloud to charge. If this is null, the
##' logged-in user's username will be used for the namespace.
##'
##' @return The result of the SQL query.
##' @family {manual-layer functions}
##' @export
execute_sql_query <- function(query, name=NULL, namespace=NULL) {
  if (is.null(namespace)) {
    namespace <- .get_default_namespace_charged()
  }

  api.client.instance <- get_api_client_instance()
  sql.api.instance <- sql <- SqlApi$new(api.client.instance)

  sql.parameters <- SQLParameters$new(name=name, query=query)
  resultObject <- sql$RunSQL(namespace, sql.parameters)

  body <- .get_raw_response_body_or_stop(resultObject)
  parsed <- jsonlite::fromJSON(rawToChar(body))
  parsed
}
