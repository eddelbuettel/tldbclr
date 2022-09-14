##' Show information about an array on TileDB Cloud
##'
##' This function shows array information on TileDB Cloud.
##'
##' Nominally you will first call \code{\link{login}}; if not, the results of
##' the last login at \code{~/.tiledb/cloud.json} will be used.
##'
##' @param namespace Like "TileDB-Inc"
##'
##' @param arrayname Like "quickstart_dense"
##'
##' @return A list of array properties
##' @family {manual-layer functions}
##' @export
array_info <- function(namespace, arrayname) {
  apiClientInstance <- get_api_client_instance()
  arrayApiInstance <- ArrayApi$new(apiClientInstance)
  # The $toJSON() converts from R6 class to R named list.
  # That in turn is nicely printable using str().
  arrayApiInstance$GetArrayMetadata(namespace, arrayname)$toJSON()
}

##' Show listing of arrays
##'
##' Returns a dataframe of metadata for all arrays that meet the
##' filter applied.
##'
##' Note that this is a paginable API but default params return all results on
##' one call, even hundreds of them. As currently implemented, pagination
##' information is not returned from this function. The \code{public} and
##' \code{shared} arguments may not both be true.
##'
##' @param public logical TRUE means list public arrays
##' @param shared logical TRUE means list shared arrays. If \code{public} and \code{shared} are both
##' false then arrays owned by you are listed.
##' @param page integer
##' @param per.page integer
##' @param search character
##' @param namespace character
##' @param orderby character
##' @param permissions character
##' @param tag list( character )
##' @param exclude.tag list( character )
##' @param file.type list( character )
##' @param exclude.file.type list( character )
##' @param file.property list( character )
##'
##' @return Dataframe of metadata for all arrays in your account that meet the filter applied.
##' @family {manual-layer functions}
##' @export
list_arrays <- function(public=FALSE, shared=FALSE, page=NULL, per.page=NULL, search=NULL, namespace=NULL,
  orderby=NULL, permissions=NULL, tag=NULL, exclude.tag=NULL, file.type=NULL, exclude.file.type=NULL, file.property=NULL, ...) {
  if (public && shared) {
    stop("The public and shared arguments must not both be TRUE.")
  }
  apiClientInstance <- get_api_client_instance()
  arrayApiInstance <- ArrayApi$new(apiClientInstance)

  if (public) {
    resultObject <- arrayApiInstance$ArraysBrowserPublicGet(page, per.page, search, namespace, orderby,
      permissions, tag, exclude.tag, file.type, exclude.file.type, file.property)
  } else if (shared) {
    resultObject <- arrayApiInstance$ArraysBrowserSharedGet(page, per.page, search, namespace, orderby,
      permissions, tag, exclude.tag, file.type, exclude.file.type, file.property)
  } else {
    resultObject <- arrayApiInstance$ArraysBrowserOwnedGet(page, per.page, search, namespace, orderby,
      permissions, tag, exclude.tag, file.type, exclude.file.type, file.property)
  }

  body <- .get_raw_response_body_or_stop(resultObject)

  bodyAsJSONString <- rawToChar(body)
  # Output has keys 'arrays' and 'pagination_metadata'; keep only the former
  jsonlite::fromJSON(bodyAsJSONString)[["arrays"]]
}

##' Register an existing array on TileDB Cloud
##'
##' The underlying storage must already exist.
##'
##' @param namespace Namespace within TileDB cloud to charge. If this is null, the
##' logged-in user's username will be used for the namespace.
##'
##' @param array_name The name to call the array in TileDB Cloud.
##'
##' @param uri The URI of where the array is stored.
##'
##' @param description Optional description field for the array.
##'
##' @param access_credentials_name Credentials to access the array storage. If omitted,
##'  the logged-in user's default credentials will be used.
##'
##' @family {manual-layer functions}
##' @export
register_array <- function(namespace=NULL, array_name, uri, description=NULL, access_credentials_name=NULL) {
  apiClientInstance <- get_api_client_instance()
  arrayApiInstance <- ArrayApi$new(apiClientInstance)

  if (is.null(namespace)) {
    namespace <- .get_default_namespace_charged_or_stop()
  }

  info <- tiledbcloud::ArrayInfoUpdate$new(
    name=array_name,
    uri=uri,
    description=description,
    access_credentials_name=access_credentials_name
  )

  resultObject <- arrayApiInstance$RegisterArray(namespace=namespace, array=array_name, array.metadata=info)

  # Decode the result, expecting empty string.
  .get_empty_response_body_or_stop(resultObject)
  invisible("OK")
}

##' Deregister an array from TileDB Cloud
##'
##' The underlying storage will not be removed.
##'
##' @param namespace Namespace within TileDB cloud to charge. If this is null, the
##' logged-in user's username will be used for the namespace.
##'
##' @param array_name The name to call the array in TileDB Cloud.
##'
##' @family {manual-layer functions}
##' @export
deregister_array <- function(namespace=NULL, array_name) {
  apiClientInstance <- get_api_client_instance()
  arrayApiInstance <- ArrayApi$new(apiClientInstance)

  if (is.null(namespace)) {
    namespace <- .get_default_namespace_charged_or_stop()
  }

  resultObject <- arrayApiInstance$DeregisterArray(namespace=namespace, array=array_name)

  # Decode the result, expecting empty string.
  .get_empty_response_body_or_stop(resultObject)
  invisible("OK")
}
