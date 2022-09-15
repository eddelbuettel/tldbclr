##' Execute a generic UDF on TileDB Cloud
##'
##' This invokes a user-defined function in TileDB Cloud.
##'
##' Nominally you will first call \code{\link{login}}; if not, the results
##' of the last login at \code{~/.tiledb/cloud.json} will be used.
##'
##' The \code{udf} and \code{namespace} arguments are required; the \code{args}
##' argument is optional.
##'
##' @param udf An R function. Arguments are specified separately via \code{args}.
##' One of \code{udf} and \code{registered_udf_name} must be non-null.
##'
##' @param registered_udf_name Name of a registered UDF, of the form \code{namespace/udfname}.
##' Arguments are specified separately via \code{args}.  One of \code{udf} and
##' \code{registered_udf_name} must be non-null.
##'
##' @param args Arguments to the function. If the function takes
##' no arguments, this can be omitted. If you want to call by
##' position, use a list like \code{args=list(123, 456)}. If you want
##' to call by name, use a named list like \code{args=list(x=123,y=456)}.
##'
##' @param result_format One of \code{native}, \code{json}, or \code{arrow}. These are
##' used as wire format for returning results from the server to this library, primarily
##' for memory-usage control.  UDF return values handed back to your code from this
##' library are converted back to natural R objects.
##'
##' @param args_format One of \code{native}, \code{json}, or \code{arrow}. These are
##' used as wire format for sending arguments to the server. Normally you do not need
##' to specify this. If you're invoking an R UDF, \code{native} is used; if you're
##' invoking a registered Python UDF, \code{json} is used but you can select \code{arrow}
##' if you wish.
##'
##' @param namespace Namespace within TileDB cloud to charge. If this is null, the
##' logged-in user's username will be used for the namespace.
##'
##' @param language If omitted, defaults to \code{"r"}. Can be set to \code{"python"}
##" when executing registered Python UDFs.
##'
##' @return The R object which is the return value from the UDF.
##' @family {manual-layer functions}
##' @export
execute_generic_udf <- function(udf=NULL, registered_udf_name=NULL, args=NULL, result_format='native',
  args_format='native', namespace=NULL, language='r')
{
  if (is.null(namespace)) {
    namespace <- .get_default_namespace_charged_or_stop()
  }
  apiClientInstance <- get_api_client_instance()
  udfApiInstance <- UdfApi$new(apiClientInstance)
  generic_udf <- GenericUDF$new()

  # We rely on UDFLanguage$new to do enum-matching
  generic_udf$language <- UDFLanguage$new(language)
  generic_udf$version <- .set_udf_version(language)

  # For invoking registered Python UDFs, we must send the args in language-independent way,
  # and retrieve the results in a language-independent way.
  if (language == 'python') {
    if (args_format == 'native') {
      args_format <- 'json'
    }
    if (result_format == 'native') {
      result_format <- 'json'
    }
  }

  # Here we rely on ResultFormat$new to match against the acceptable values for
  # result format, which in turn are automatically generated from the
  # TileDB-Cloud OpenAPI spec.
  generic_udf$result_format <- ResultFormat$new(result_format)

  .check_udf_or_unregistered_exclusively('execute_generic_udf', udf, registered_udf_name)
  if (!is.null(udf)) {
    generic_udf$exec <- jsonlite::toJSON(as.integer(serialize(udf, NULL)))
  }
  if (!is.null(registered_udf_name)) {
    generic_udf$udf_info_name <- registered_udf_name
  }

  if (!is.null(args)) {
    if (args_format == 'native') {
      generic_udf$argument <- jsonlite::toJSON(as.integer(serialize(args, NULL)))
    } else if (args_format == 'json') {
      # Particularly useful for invoking registered Python UDFs
      generic_udf$argument <- jsonlite::toJSON(args, auto_unbox = TRUE, null = "null")
      generic_udf$argument <- gsub('"', '\\\\"', generic_udf$argument) # hack hack hack -- does not nest well
    } else {
      stop('args_format must be one of "native" or "json"')
    }
  }

  resultObject <- udfApiInstance$SubmitGenericUDF(namespace, generic_udf)

  # Decode the result
  entire_json_is_result <- language == 'python' && result_format == 'json'
  .get_decoded_response_body_or_stop(resultObject, result_format, entire_json_is_result=entire_json_is_result)
}

##' Execute a single-array UDF on TileDB Cloud
##'
##' This invokes a user-defined function in TileDB Cloud.
##'
##' Nominally you will first call \code{\link{login}}; if not, the results
##' of the last login at \code{~/.tiledb/cloud.json} will be used.
##'
##' All arguments are required.
##'
##' @param array Name of the array, in the form either \code{tiledb://hello/world}
##' or \code{hello/world}.
##'
##' @param udf An R function which takes a dataframe as argument.
##' Arguments are specified separately via \code{args}.
##' One of \code{udf} and \code{registered_udf_name} must be non-null.
##'
##' @param registered_udf_name Name of a registered UDF, of the form \code{namespace/udfname}.
##' Arguments are specified separately via \code{args}.  One of \code{udf} and
##' \code{registered_udf_name} must be non-null.
##'
##' @param selectedRanges List of two-column matrices, one matrix per dimension,
##' each matrix being a start-end pair: e.g. \code{list(cbind(1,10),cbind(1,20))}.
##'
##' @param attrs Optional list of attributes (default: all) for the server-side
##' code to select for UDF execution. Specifying only what your UDF needs is
##' useful for memory-usage control.
##'
##' @param layout One of \code{row-major}, \code{col-major}, \code{global-order}, or
##' \code{unordered},
##'
##' @param args Arguments to the function. If the function takes
##' no arguments, this can be omitted. If you want to call by
##' position, use a list like \code{args=list(123, 456)}. If you want
##' to call by name, use a named list like \code{args=list(x=123,y=456)}.
##'
##' @param result_format One of \code{native}, \code{json}, or \code{arrow}. These are
##' used as wire format for returning results from the server to this library, primarily
##' for memory-usage control.  UDF return values handed back to your code from this
##' library are converted back to natural R objects.
##'
##' @param args_format One of \code{native}, \code{json}, or \code{arrow}. These are
##' used as wire format for sending arguments to the server. Normally you do not need
##' to specify this. If you're invoking an R UDF, \code{native} is used; if you're
##' invoking a registered Python UDF, \code{json} is used but you can select \code{arrow}
##' if you wish.
##'
##' @param namespace Namespace within TileDB cloud to charge. If this is null, the
##' logged-in user's username will be used for the namespace.
##'
##' @param language If omitted, defaults to \code{"r"}. Can be set to \code{"python"}
##" when executing registered Python UDFs.
##'
##' @return Return value from the UDF.
##'
##' @family {manual-layer functions}
##' @export
execute_array_udf <- function(array, udf=NULL, registered_udf_name=NULL, selectedRanges, attrs=NULL, layout=NULL, args=NULL,
  result_format='native', args_format='native', namespace=NULL, language='r')
{
  if (is.null(namespace)) {
    namespace <- .get_default_namespace_charged_or_stop()
  }
  apiClientInstance <- get_api_client_instance()
  udfApiInstance <- UdfApi$new(apiClientInstance)
  multi_array_udf <- MultiArrayUDF$new()

  # We rely on UDFLanguage$new to do enum-matching
  multi_array_udf$language <- UDFLanguage$new(language)
  multi_array_udf$version <- .set_udf_version(language)

  # For invoking registered Python UDFs, we must send the args in language-independent way,
  # and retrieve the results in a language-independent way.
  if (language == 'python') {
    if (args_format == 'native') {
      args_format <- 'json'
    }
    if (result_format == 'native') {
      result_format <- 'json'
    }
  }

  # Here we rely on ResultFormat$new to match against the acceptable values for
  # result format, which in turn are automatically generated from the
  # TileDB-Cloud OpenAPI spec.
  multi_array_udf$result_format <- ResultFormat$new(result_format)

  .check_udf_or_unregistered_exclusively('execute_array_udf', udf, registered_udf_name)
  if (!is.null(udf)) {
    multi_array_udf$exec <- jsonlite::toJSON(as.integer(serialize(udf, NULL)))
  }
  if (!is.null(registered_udf_name)) {
    multi_array_udf$udf_info_name <- registered_udf_name
  }

  if (!is.null(layout)) {
    layout <- Layout$new(layout)
  }

  # selected_ranges are required for dense arrays; optional for sparse arrays.
  # At the user/library level this is a list of two-column matrices, e.g.
  # 'list(cbind(1,2), cbind(3,4))'.
  queryRanges <- QueryRanges$new(layout=layout, ranges=selectedRanges)
  multi_array_udf$ranges <- queryRanges

  # This function is for single arrays which are packed a little differently
  # from the way the multi-array function does it. Namely, here we pass buffers &
  # ranges outside of the multi_array_udf object.
  #
  # Attrs can be optionally specified by the client. If they are not, the
  # server-side code will load all attributes.
  if (!is.null(attrs)) {
    multi_array_udf$buffers <- attrs
  }

  if (!is.null(args)) {
    if (args_format == 'native') {
      multi_array_udf$argument <- jsonlite::toJSON(as.integer(serialize(args, NULL)))
    } else if (args_format == 'json') {
      # Particularly useful for invoking registered Python UDFs
      multi_array_udf$argument <- jsonlite::toJSON(args, auto_unbox = TRUE, null = "null")
      multi_array_udf$argument <- gsub('"', '\\\\"', multi_array_udf$argument) # hack hack hack -- does not nest well
    } else {
      stop('args_format must be one of "native" or "json"')
    }
  }

  # * The namespace parameter is whom to charge.
  # * The array argument should be of the form 'tiledb://foo/bar'.
  #   o The whom-to-charge parameter maps to x.payer in the REST API.
  #   o The array namespace and name map to namespace and array in the REST API.
  # This is necessary for a user to be able to run UDFs on arrays they do not
  # own, charging their own namespace.
  uri_fields <- .split_uri(array)

  array_namespace <- uri_fields[["namespace"]]
  array_name <- uri_fields[["name"]]
  x.payer <- if (is.null(namespace)) array_namespace else namespace

  # Make the network request.
  resultObject <- udfApiInstance$SubmitUDF(namespace=array_namespace, array=array_name, udf=multi_array_udf, x.payer=x.payer)

  # Decode the result
  entire_json_is_result <- language == 'python' && result_format == 'json'
  .get_decoded_response_body_or_stop(resultObject, result_format, entire_json_is_result=entire_json_is_result)
}

##' Execute a multi-array UDF on TileDB Cloud
##'
##' This invokes a user-defined function in TileDB Cloud.
##'
##' Nominally you will first call \code{\link{login}}; if not, the results
##' of the last login at \code{~/.tiledb/cloud.json} will be used.
##'
##' All arguments are required.
##'
##' @param array_list List of \code{UDFArrayDetails} objects.
##' Example list element: \code{tiledbcloud::UDFArrayDetails$new(uri="tiledb://demo/quickstart_dense", ranges=QueryRanges$new(layout=Layout$new('row-major'), ranges=list(cbind(1,4),cbind(1,4))), buffers=list("a"))}
##'
##' @param udf An R function which takes dataframes as arguments, one dataframe argument for each element in \code{array_list}.
##' Arguments are specified separately via \code{args}.
##' One of \code{udf} and \code{registered_udf_name} must be non-null.
##'
##' @param registered_udf_name Name of a registered UDF, of the form \code{namespace/udfname}.
##' Arguments are specified separately via \code{args}.  One of \code{udf} and
##' \code{registered_udf_name} must be non-null.
##'
##' @param args Arguments to the function. If the function takes
##' no arguments, this can be omitted. If you want to call by
##' position, use a list like \code{args=list(123, 456)}. If you want
##' to call by name, use a named list like \code{args=list(x=123,y=456)}.
##'
##' @param result_format One of \code{native}, \code{json}, or \code{arrow}. These are
##' used as wire format for returning results from the server to this library, primarily
##' for memory-usage control.  UDF return values handed back to your code from this
##' library are converted back to natural R objects.
##'
##' @param args_format One of \code{native}, \code{json}, or \code{arrow}. These are
##' used as wire format for sending arguments to the server. Normally you do not need
##' to specify this. If you're invoking an R UDF, \code{native} is used; if you're
##' invoking a registered Python UDF, \code{json} is used but you can select \code{arrow}
##' if you wish.
##'
##' @param namespace Namespace within TileDB cloud to charge. If this is null, the
##' logged-in user's username will be used for the namespace.
##'
##' @param language If omitted, defaults to \code{"r"}. Can be set to \code{"python"}
##" when executing registered Python UDFs.
##'
##' @return Return value from the UDF.
##' @family {manual-layer functions}
##' @export
execute_multi_array_udf <- function(array_list, udf=NULL, registered_udf_name=NULL, args=NULL,
  result_format='native', args_format='native', namespace=NULL, language='r')
{
  if (is.null(namespace)) {
    namespace <- .get_default_namespace_charged_or_stop()
  }
  apiClientInstance <- get_api_client_instance()
  udfApiInstance <- UdfApi$new(apiClientInstance)
  multi_array_udf <- MultiArrayUDF$new()

  # We rely on UDFLanguage$new to do enum-matching
  multi_array_udf$language <- UDFLanguage$new(language)
  multi_array_udf$version <- .set_udf_version(language)

  # For invoking registered Python UDFs, we must send the args in language-independent way,
  # and retrieve the results in a language-independent way.
  if (language == 'python') {
    if (args_format == 'native') {
      args_format <- 'json'
    }
    if (result_format == 'native') {
      result_format <- 'json'
    }
  }

  # Here we rely on ResultFormat$new to match against the acceptable values for
  # result format, which in turn are automatically generated from the
  # TileDB-Cloud OpenAPI spec.
  multi_array_udf$result_format <- ResultFormat$new(result_format)

  .check_udf_or_unregistered_exclusively('execute_multi_array_udf', udf, registered_udf_name)
  if (!is.null(udf)) {
    multi_array_udf$exec <- jsonlite::toJSON(as.integer(serialize(udf, NULL)))
  }
  if (!is.null(registered_udf_name)) {
    multi_array_udf$udf_info_name <- registered_udf_name
  }

  if (!is.null(args)) {
    if (args_format == 'native') {
      multi_array_udf$argument <- jsonlite::toJSON(as.integer(serialize(args, NULL)))
    } else if (args_format == 'json') {
      # Particularly useful for invoking registered Python UDFs
      multi_array_udf$argument <- jsonlite::toJSON(args, auto_unbox = TRUE, null = "null")
      multi_array_udf$argument <- gsub('"', '\\\\"', multi_array_udf$argument) # hack hack hack -- does not nest well
    } else {
      stop('args_format must be one of "native" or "json"')
    }
  }

  # TODO: type-check the array_list parameter to be sure it's list of UDFArrayDetails
  multi_array_udf$arrays <- array_list

  # Make the network request.
  resultObject <- udfApiInstance$SubmitMultiArrayUDF(namespace=namespace, udf=multi_array_udf)

  # Decode the result
  entire_json_is_result <- language == 'python' && result_format == 'json'
  .get_decoded_response_body_or_stop(resultObject, result_format, entire_json_is_result=entire_json_is_result)
}

##' Register a UDF on TileDB Cloud
##'
##' Registers a user-defined function on TileDB Cloud, so that it may be invoked by name later.
##'
##' @param namespace Namespace for the UDF to be stored in, e.g. \code{mynamespace}. If omitted, defaults to username.
##'
##' @param name character Name for the function to be stored under in TileDB Cloud, e.g. \code{myudfname}.
##'
##' @param type character One of \code{generic}, \code{single_array}, or \code{multi_array}.
##'
##' @param func An R function which takes a dataframe as first argument.
##'
##' @param exec_raw character Text to display in the TileDB Cloud UI's Preview tab. If omitted, a full deparse of \code{func} is used. You can set this to something shorter for brevity if you like.
##'
##' @param version character Optional version string.
##'
##' @param image_name character
##'
##' @param readme README text to be displayed in the TileDB Cloud UI.
##'
##' @param license_id character See the TileDB Cloud UI for options.
##'
##' @param license_text character See the TileDB Cloud UI for options.
##'
##' @param tags list(character) Tags to apply to the UDF.
##'
##' @return No return value.
##' @family {manual-layer functions}
##' @export
register_udf <- function(namespace=NULL, name, type, func, func_text=NULL, version=NULL, image_name=NULL, readme=NULL,
                         license_id=NULL, license_text=NULL, tags=NULL) {
  apiClientInstance <- get_api_client_instance()
  udfApiInstance <- UdfApi$new(apiClientInstance)

  if (is.null(func_text)) {
    func_text <- paste(deparse(func), collapse="\\n")
  }

  if (is.null(version)) {
    version <- .set_udf_version('r')
  }

  info <- tiledbcloud::UDFInfoUpdate$new(
    name=name,
    type=tiledbcloud::UDFType$new(type),
    exec=jsonlite::toJSON(as.integer(serialize(func, NULL))),
    exec_raw=func_text,
    language=tiledbcloud::UDFLanguage$new('r'),
    version=version,
    image_name=image_name,
    readme=readme,
    license_id=license_id,
    license_text=license_text,
    tags=tags
  )

  resultObject <- udfApiInstance$RegisterUDFInfo(namespace, name, info)

  # Decode the result, expecting empty string.
  .get_empty_response_body_or_stop(resultObject)
  invisible("OK")
}

##' Update a UDF on TileDB Cloud
##'
##' Updates information for a specified user-defined function on TileDB Cloud. Please provide all
##' parameters to be set, not only the ones that need changing.
##'
##' @param namespace Namespace for the UDF to be stored in, e.g. \code{mynamespace}. If omitted, defaults to username.
##'
##' @param name character Name for the function to be stored under in TileDB Cloud, e.g. \code{myudfname}.
##'
##' @param type character One of \code{generic}, \code{single_array}, or \code{multi_array}.
##'
##' @param func An R function which takes a dataframe as first argument.
##'
##' @param exec_raw character Text to display in the TileDB Cloud UI's Preview tab. If omitted, a full deparse of \code{func} is used. You can set this to something shorter for brevity if you like.
##'
##' @param version character Optional version string.
##'
##' @param image_name character
##'
##' @param readme README text to be displayed in the TileDB Cloud UI.
##'
##' @param license_id character See the TileDB Cloud UI for options.
##'
##' @param license_text character See the TileDB Cloud UI for options.
##'
##' @param tags list(character) Tags to apply to the UDF.
##'
##' @return No return value.
##' @family {manual-layer functions}
##' @export
update_udf_info <- function(namespace, name, type, func=NULL, func_text=NULL, version=NULL, image_name=NULL,
                            readme=NULL, license_id=NULL, license_text=NULL, tags=NULL) {
  apiClientInstance <- get_api_client_instance()
  udfApiInstance <- UdfApi$new(apiClientInstance)

  exec <- NULL
  exec_raw <- NULL
  if (!is.null(func)) {
    exec <- jsonlite::toJSON(as.integer(serialize(func, NULL)))
    if (is.null(func_text)) {
      exec_raw <- paste(deparse(func), collapse="\\n")
    }
  }

  info <- tiledbcloud::UDFInfoUpdate$new(
    name=name,
    type=tiledbcloud::UDFType$new(type),
    exec=exec,
    exec_raw=exec_raw,
    language=tiledbcloud::UDFLanguage$new('r'),
    version=version,
    image_name=image_name,
    readme=readme,
    license_id=license_id,
    license_text=license_text,
    tags=tags
  )

  resultObject <- udfApiInstance$UpdateUDFInfo(namespace, name, info)

  # Decode the result, expecting empty string.
  .get_empty_response_body_or_stop(resultObject)
  invisible("OK")
}

##' Get information about a UDF on TileDB Cloud
##'
##' Reads back information for a specified user-defined function on TileDB Cloud.
##' Note that \code{version}, \code{image_name}, \code{exec}, and \code{exec_raw}
##' are writable via \code{register_udf} but are not read back by this function.
##'
##' @param namespace Namespace for the UDF in TileDB Cloud, e.g. \code{mynamespace}.
##'
##' @param name Name of the UDF in TileDB Cloud, e.g. \code{myudfname}.
##'
##' @return List of key-value pairs of UDF information.
##'
##' @family {manual-layer functions}
##' @export
get_udf_info <- function(name, namespace) {
  apiClientInstance <- get_api_client_instance()
  udfApiInstance <- UdfApi$new(apiClientInstance)

  resultObject <- udfApiInstance$GetUDFInfo(name=name, namespace=namespace)

  # Decode the result
  body <- .get_raw_response_body_or_stop(resultObject)
  parsed <- jsonlite::fromJSON(rawToChar(body))
  parsed
}

##' Deregister a UDF from TileDB Cloud
##'
##' Deletes a registered UDF. This removes all sharing and cannot be undone.
##'
##' @param namespace Namespace for the UDF in TileDB Cloud, e.g. \code{mynamespace}.
##'
##' @param name Name of the UDF in TileDB Cloud, e.g. \code{myudfname}.
##'
##' @return No return value.
##' @family {manual-layer functions}
##' @export
deregister_udf <- function(name, namespace) {
  apiClientInstance <- get_api_client_instance()
  udfApiInstance <- UdfApi$new(apiClientInstance)

  resultObject <- udfApiInstance$DeleteUDFInfo(name=name, namespace=namespace)

  # Decode the result, expecting empty string.
  .get_empty_response_body_or_stop(resultObject)
  invisible("OK")
}

# Shared by generic/single-array/multi-array UDF executors: people can send an
# R function or a registered UDF name -- one or the other.
.check_udf_or_unregistered_exclusively <- function(caller_name, udf, registered_udf_name) {
  if (is.null(udf) && is.null(registered_udf_name)) {
    stop(caller_name, ": one, but not both, of udf and registered_udf_name must be provided.")
  }
  if (!is.null(udf) && !is.null(registered_udf_name)) {
    stop(caller_name, ": one, but not both, of udf and registered_udf_name must be provided.")
  }
}

# Shared by generic/single-array/multi-array UDF executors:
# * We're an R client, always
# * If we're sending an R function: tell the R version we're running so the
#   REST server can select the right Docker image to run.
# * If we're asking for an invocation of a registered R function: this is a
#   TODO. For now send our R version, but ideally the R version of the
#   registered function would be stored in the registered-UDF object.
# * If we're asking for an invocation of a registered Python function:
#   leave the version blank (NULL). As a TODO we can let people say, as an argument
#   in this API, which Python version they want to run; or store the Python version
#   in registered-UDF objects.
# Note that the primary issue for having language versions is for package
# dependencies server-side, as well as (more crucially) for Python pickling of
# function bodies and args. R's serialization is a bit more stable over time.
.set_udf_version <- function(language)
  if (language == 'r') {
    # Send only strings like '4.1.2' without any decoration like that offered
    # by R.version.string. We want to send something simple the REST server can
    # easily do math on when need be.
    format(getRversion())
  } else {
    NULL
  }
