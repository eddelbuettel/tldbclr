##' Define a function to be executed within a task graph
##'
##' @param args Can be provided here with \code{c <- delayed(function(...) { sum(...) }, args=list(a,b))}
##' or separately with \code{c <- delayed(function(...) { sum(...) })` and later
##' \code{delayed_args(c) <- list(a,b)}.
##'
##' @param name Optional -- e.g. \code{a} or \code{b}. If omitted, it defaults to a UUID.
##'
##' @param namespace If supplied, a namespace to use for executing this particular node.
##' If omitted, a namespace can be applied at your top-level call to \code{compute}.
##' If omitted there as well, your logged-in account's default namespace will be used.
##'
##' @param local If true, execute the functions on the local host; if else, execute them as
##' UDFs in TileDB Cloud.
##'
##' @return A task-graph node object on which you can later call \code{compute}.
##'
##' @family {manual-layer functions}
##' @export
delayed <- function(func, args=NULL, name=NULL, namespace=NULL, local=FALSE) {
  have_args <- FALSE

  if (is.null(args)) {
    # Function args were not passed here in the factory function. They'll need
    # to set args later. Unless the function takes no args in which it's
    # pedantic of us to insist on that.
    args <- list()
    if (length(formals(func)) == 0) {
      have_args <- TRUE
    }
  } else {
    # Function args were passed here in the factory function. They need
    # to be a list, like 'delayed(f, list(3,4))' not 'delayed(f, 3, 4)'.
    # TODO: find a way to accept the latter ...
    stopifnot(is.list(args))
    have_args <- TRUE
  }

  Node$new(func=func, args=args, have_args=have_args, name=name, namespace=namespace, local=local)
}

##' Define a SQL query function to be executed within a task graph
##'
##' @param query SQL query string -- see vignette for examples
##'
##' @param name A display name for the query
##'
##' @param namespace If supplied, the TileDB-Cloud namespace to charge the query to.
##' If omitted, a namespace can be applied at your top-level call to \code{compute}.
##' If omitted there as well, your logged-in account's default namespace will be used.
##'
##' @return A task-graph node object on which you can later call \code{compute}.  The return value from
##' compute() will be the query result as a dataframe.  Note that results will be strings, so numerical
##' results will need to be explicitly cast as such.
##'
##' @family {manual-layer functions}
##' @export
delayed_sql <- function(query, name=NULL, namespace=NULL) {
  # It is absolutely necessary that this be a locally executing call to the
  # remote REST service. A non-local execution of this would mean the REST
  # server calling itself -- not only would that be a circular dependency, but
  # moreover the tiledbcloud is not running on the REST server.
  delayed(function() {
    execute_sql_query(query=query, name=name, namespace)
  }, local=TRUE)
}

##' Define a generic UDF to be executed within a task graph
##'
##' @param udf An R function. Arguments are specified separately via \code{args}.
##' One of \code{udf} and \code{registered_udf_name} must be non-null.
##'
##' @param registered_udf_name Name of a registered UDF, of the form \code{namespace/udfname}.
##' Arguments are specified separately via \code{args}.  One of \code{udf} and
##' \code{registered_udf_name} must be non-null.
##'
##' @param args Can be provided here with \code{c <- delayed(function(...) { sum(...) }, args=list(a,b))}
##' or ##' separately with \code{c <- delayed_generic_udf(...)` and later \code{c$args <- list(a,b)}.
##'
##' @param name Optional -- e.g. \code{a} or \code{b}. If omitted, it defaults to a UUID.
##'
##' @param namespace If supplied, a namespace to use for executing this particular node.
##' If omitted, a namespace can be applied at your top-level call to \code{compute}.
##' If omitted there as well, your logged-in account's default namespace will be used.
##'
##' @param language If omitted, defaults to \code{"r"}. Can be set to \code{"python"}
##" when executing registered Python UDFs.
##'
##' @return The return value from the UDF as an R object.
##'
##' @family {manual-layer functions}
##' @export
delayed_generic_udf <- function(udf=NULL, registered_udf_name=NULL, args=NULL, name=NULL, namespace=NULL, language='r')
{
  # It is absolutely necessary that this be a locally executing call to the
  # remote REST service. A non-local execution of this would mean the REST
  # server calling itself -- not only would that be a circular dependency, but
  # moreover the tiledbcloud is not running on the REST server.

  if (is.null(udf) && is.null(registered_udf_name)) {
    stop("One, but not both, of udf and registered_udf_name must be provided.")
  }
  if (!is.null(udf) && !is.null(registered_udf_name)) {
    stop("One, but not both, of udf and registered_udf_name must be provided.")
  }

  delayed(
    func=function(...) {
      execute_generic_udf(
        udf=udf,
        registered_udf_name=registered_udf_name,
        args=list(...),
        namespace=namespace,
        language=language)
    },
    args=args,
    name=NULL,
    local=TRUE)
}

##' Define a single-array UDF to be executed within a task graph
##'
##' @param array TileDB URI -- see vignette for examples.
##'
##' @param udf User-defined function, as in UDF examples. Arguments are specified separately via \code{args}.
##' One of \code{udf} and \code{registered_udf_name} must be non-null.
##'
##' @param registered_udf_name Name of a registered UDF, of the form \code{namespace/udfname}.
##' Arguments are specified separately via \code{args}.  One of \code{udf} and
##' \code{registered_udf_name} must be non-null.
##'
##' @param selectedRanges As in UDF examples.
##'
##' @param attrs As in UDF examples.
##'
##' @param layout As in UDF examples.
##'
##' @param args Can be provided here with \code{c <- delayed_array_udf(..., args=list(a,b))}
##' or separately with \code{c <- delayed(function(...) { sum(...) })` and later
##' \code{c$args <- list(a,b)}.
##'
##' @param result_format As in UDF examples.
##'
##' @param name A display name for the query
##'
##' @param namespace If supplied, a namespace to use for executing this particular node.
##' If omitted, a namespace can be applied at your top-level call to \code{compute}.
##' If omitted there as well, your logged-in account's default namespace will be used.
##'
##' @param language If omitted, defaults to \code{"r"}. Can be set to \code{"python"}
##" when executing registered Python UDFs.
##'
##' @return The return value from the UDF as an R object.
##'
##' @family {manual-layer functions}
##' @export
delayed_array_udf <- function(array, udf=NULL, registered_udf_name=NULL, selectedRanges, attrs,
  layout=NULL, args=NULL, result_format='native', name=NULL, namespace=NULL, language='r')
{
  # It is absolutely necessary that this be a locally executing call to the
  # remote REST service. A non-local execution of this would mean the REST
  # server calling itself -- not only would that be a circular dependency, but
  # moreover the tiledbcloud is not running on the REST server.

  if (is.null(udf) && is.null(registered_udf_name)) {
    stop("One, but not both, of udf and registered_udf_name must be provided.")
  }
  if (!is.null(udf) && !is.null(registered_udf_name)) {
    stop("One, but not both, of udf and registered_udf_name must be provided.")
  }
  # Array UDFs are different in that when they're executed server-side, the dataframe loaded from
  # the array is always passed as the first argument to the UDF. Then, any *additional* arguments
  # the UDF may take. We need to do this to avoid a runtime error of the form "delayed object must
  # have args set before calling compute" for the case when the UDF takes no arguments beyond the
  # single dataframe argument.
  if (is.null(args)) {
    args <- list()
  }

  delayed(
    func=function(...) {
      execute_array_udf(
        array=array,
        udf=udf,
        registered_udf_name=registered_udf_name,
        selectedRanges=selectedRanges,
        attrs=attrs,
        layout=layout,
        args=list(...),
        result_format=result_format,
        namespace=namespace,
        language=language)
    },
    args=args,
    name=name,
    local=TRUE)
}
