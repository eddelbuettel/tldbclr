# ================================================================
# Node is an R reference class. We need references since we have a mutable DAG of nodes and we must
# be able to update them as their computation progresses.
#
# See the delayed() function in this package for a factory function. This constructor is not
# intended to be invoked directly, but rather, by delayed().
#
# Notes on dev/debug: Please see the developer vignette about task graphs.
# ================================================================

# ----------------------------------------------------------------
# Users can name their nodes with display-names if they like. Absent that, the following is used
# for display names, and regardless, is always used for hash-mapping node IDs to node objects.
make_id_generator <- function() {
    count <- 0
    f <- function() {
        count <<- count + 1
        return(sprintf("n%06d", count))
    }
    return(f)
}
id_generator <-make_id_generator()

##' @export
##' @importFrom future future
Node <- R6::R6Class(
  'Node',
  public = list(
    func          = NULL,
    args          = NULL,
    # have_args is necessary in addition to args (with check args being non-null)
    # since some functions have no arguments and it's pedantic (bad UX) for us to
    # insist that people separately articulate that. See the 'delayed' factory
    # function.
    have_args     = NULL,
    local         = NULL,
    # Normally namespace comes in through the DAG. But users can specify a particular
    # namespace for executing a given node, if they wish.
    namespace     = NULL,

    future        = NULL,
    future_result = NULL,
    status        = NULL,
    result        = NULL,

    # Only the terminal node in the DAG has this being non-null
    dag_for_terminal = NULL,

    id            = NULL,
    name          = NULL,

    # Note: args are non-optional here but user-convenience optionals are
    # implemented in the 'delayed' factory function.
    initialize = function(func, args, have_args, name, local, namespace) {
      self$result    <- NULL
      self$func      <- func
      self$args      <- args
      self$have_args <- have_args
      self$future    <- NULL
      self$status    <- NOT_STARTED
      self$result    <- NULL
      self$local     <- local
      self$namespace <- namespace

      self$id <- id_generator()
      if (is.null(name)) {
        self$name <- self$id
      } else {
        self$name <- name
        # Note: for debugging it's sometimes nice to do this, as long as
        # all nodes have unique names:
        # self$id <- name
      }
    },

    set_args = function(value) {
      stopifnot(is.list(value))
      self$args <- value
      self$have_args <- TRUE
    },

    # For future use
    add_arg = function(value) {
      if (is.null(self$args)) {
        self$args <- list(value)
      } else {
        self$args <- append(self$args, value)
      }
      self$have_args <- TRUE
    },

    # ----------------------------------------------------------------
    # This was written first during package development, and is also perhaps
    # useful for debug. Or if anyone is ever deeply refactoring / taking apart
    # and reassembling this stuff.
    compute_sequentially = function() {
      if (!is.null(self$result)) {
        return (self$result)
      }
      if (!self$have_args) {
        stop("delayed object must have args set before calling compute")
      }
      evaluated <- lapply(self$args, function(arg) {
        if (is(arg, "Node")) {
          arg$compute_sequentially()
        } else {
          arg
        }
      })
      # Memoize for the benefit of any other nodes that depend on this one
      self$result <- do.call(self$func, evaluated)
      self$result
    },

    # ----------------------------------------------------------------
    # Nominal use-case is people call compute(...) on a return value from delayed(...).
    # However, for more detailed inspection we support getting a DAG and doing dag$poll()
    # and inspecting the results as computation progresses.
    make_dag = function() {
      self$dag_for_terminal <- DAG$new(terminal_node=self)
      self$dag_for_terminal
    },

    # ----------------------------------------------------------------
    # This is for reruns in case of transient node failures.
    reset = function() {
      if (self$status == FAILED) {
        self$status        <- NOT_STARTED
        self$future        <- NULL
        self$future_result <- NULL
        self$result        <- NULL
      }
    },

    # ----------------------------------------------------------------
    # This launches a compute of the entire DAG which *terminates* at this node.
    # This is not a solely-self-compute method to run on this particular item's
    # delayed function.
    #
    # NULL timeout_seconds means wait indefinitely.
    #
    # The namespace here is one to be applied to the entire DAG. Individual nodes may
    # have been constructed with their own overrides, which will be honored on node$poll().
    #
    # This is nominally called not as node$compute() but via the generic compute(node).
    # See comments in the compute generic for more information.
    compute = function(timeout_seconds=NULL, verbose=FALSE, namespace=NULL, force_all_local=FALSE) {
      # Store this DAG object inside the terminal node so that after a compute() (whether successful
      # or failed) people can show(n$dag_for_terminal) to visualize future results, stdout from the
      # forked processes, etc.
      if (is.null(self$dag_for_terminal)) {
        self$make_dag()
      }

      self$dag_for_terminal$compute(timeout_seconds=timeout_seconds, verbose=verbose, namespace=namespace,
        force_all_local=force_all_local)
    },

    # ----------------------------------------------------------------
    # IMPORTANT: even though we're using reference classes, the stuff inside the future body is
    # executed in a *separated forked process*. So a given node can't just walk the args-DAG to see
    # if its callers are done. Crucially, the interaction between the forked process and the parent
    # is the future's return value.
    #
    # Our DAG is a poll-driven DAG so dag$poll() must be called repeatedly in order to launch
    # futures for initial nodes, detect when they are resolved, launch subsequent nodes, etc.
    poll = function(verbose=FALSE, namespace=NULL, force_local=FALSE) {
      # Look up namespace to charge:
      # 1. Node-level specification, if any
      # 2. DAG-level argument, if any
      # 3. Account-level default
      namespace_to_use <- NULL
      if (!is.null(self$namespace)) { # Node-level
        namespace_to_use <- self$namespace
      } else if (!is.null(namespace)) { # DAG-level
        namespace_to_use <- namespace
      } else {
        namespace_to_use <- .get_default_namespace_charged() # Account-level
      }
      if (is.null(namespace_to_use) && (!self$local && !force_local)) {
        stop("namespace must be provided in a task graph with any non-local nodes, and no account-local default was found")
      }

      if (self$status == COMPLETED) {
        return(TRUE)
      }

      if (!self$have_args) {
        stop("delayed object must have args set before calling compute")
      }
      for (arg in self$args) {
        if (is(arg, "Node")) {
          arg$poll(verbose=verbose, namespace=namespace_to_use, force_local=force_local)
        }
      }
      if (!self$args_ready()) {
        return(FALSE)
      }

      # Check if already running.  This protects against multiple launches in
      # diamond-dependency cases.
      if (!is.null(self$future)) {
        if (future::resolved(self$future)) {

          # Save this off for show()/str() later
          self$future_result <- future::result(self$future)

          if (verbose) {
            # These are output lines from within the forked process. We get them
            # all at once, so it's extra-important that they had been printed
            # with timestamps within them.
            cat(self$future_result$stdout)
            t <- Sys.time()
            cat(as.integer(t), as.character(t), "END  ", self$name, "\n")
          }

          if (!is.null(future::result(self$future)$visible) && future::result(self$future)$visible) {
            self$result <- future::result(self$future)$value
            self$status <- COMPLETED
          } else {
            self$status <- FAILED
            stop("node failed: ", self$name, ": ",
              paste(sapply(future::result(self$future)$conditions, function(c) {c$condition$message}), collapse=";"))
          }

          # Empty this out so dependent nodes have less data to serialize
          self$future <- NULL

          return(TRUE)
        }
        return(FALSE)
      }

      if (verbose) {
        t <- Sys.time()
        cat(as.integer(t), as.character(t), "START", self$name, "\n")
      }

      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      # As noted above, everything inside the curly braces here executes within a
      # separate forked process. All args are snapshotted as of the fork. Any
      # updates to self between the curly braces won't persist after the forked
      # process has executed.  Communication back to the parent process is via
      # the return value from the stuff between the curly braces.
      self$future <- future::future(
        {
        # TODO: make helper method
        evaluated <- lapply(self$args, function(arg) {
          if (is(arg, "Node")) {
            if (arg$status == FAILED) {
              stop("dependency has failed")
            }
            if (arg$status != COMPLETED) {
              stop("internal coding error: detected at node ", self$name,
                " result for arg ", arg$name, " should have already been awaited")
            }
            arg$result
          } else {
            arg
          }
        })

        # These cat lines to stdout won't go anywhere visible as they're inside
        # the forked process -- only when the parent collects result via
        # 'cat(self$future_result$stdout)' will they be user-visible. For this reason it's
        # extra-important that we provide timestamps.
        if (self$local || force_local) {
          t <- Sys.time()
          cat(as.integer(t), as.character(t), "launch local compute  ", self$name, "\n")
          self$result <- do.call(self$func, evaluated)
          t <- Sys.time()
          cat(as.integer(t), as.character(t), "finish local compute  ", self$name, "\n")
        } else {
          t <- Sys.time()
          cat(as.integer(t), as.character(t), "launch remote compute  ", self$name, "\n")
          self$result <- execute_generic_udf(udf=self$func, args=evaluated, namespace=namespace_to_use)
          t <- Sys.time()
          cat(as.integer(t), as.character(t), "finish remote compute  ", self$name, "\n")
        }
        # This return value back to the call
        self$result
        },
        earlySignal=FALSE
      )
      # If earlySignal == TRUE, resolved(self$future) will throw before returning.
      # With earlySignal == FALSE, we have to do more bookkeeping ourselves but it's
      # easier to set the node status to FAILED.

      self$status <- RUNNING
      return(FALSE)
    },

    # ----------------------------------------------------------------
    # This is used for DAG construction.
    is_initial = function() {
      if (!self$have_args) {
        stop("delayed object must have args set before calling compute")
      }
      for (arg in self$args) {
        if (is(arg, "Node")) {
          return(FALSE)
        }
      }
      return(TRUE)
    },

    # ----------------------------------------------------------------
    args_ready = function() {
      for (arg in self$args) {
        if (is(arg, "Node")) {
          if (arg$status == FAILED) {
            stop("dependency has failed")
          }
          if (arg$status != COMPLETED) {
            return(FALSE)
          }
        }
      }
      return(TRUE)
    },

    # ----------------------------------------------------------------
    # For DAG display
    show_status = function() {
      cat("  ", self$name, " ", sep="")
      cat(" args_ready=", ifelse(self$have_args, self$args_ready(), "(none set)"), sep="")
      cat(" status=", self$status, sep="")
      cat("\n")
    },

    show = function() {
      cat("node=", self$name, sep="")
      cat(",nargs=", ifelse(self$have_args, length(self$args), "(none set)"), sep="")
      cat(",args_ready=", ifelse(self$have_args, self$args_ready(), "(none set)"), sep="")
      cat(",future=", ifelse(is.null(self$future), "absent", "present"), sep="")
      cat(",status=", self$status, sep="")
      cat("\n")

      if (!is.null(self$result)) {
        cat("Result:\n")
        str(self$result)
      }



      if (!is.null(self$dag_for_terminal)) {
        show(self$dag_for_terminal)
      }
    }
  )
)

# ----------------------------------------------------------------
# This generic lets people do 'compute(f)' in addition to 'f$compute()'

##' Launch a task graph from a given terminal node in the task graph
##'
##' The task graph is implicitly defined by various \code{delayed} objects having others
##' in their argument lists.
##'
##' @param node The object whose args are being set -- nominally, produced by
##' \code{delayed}, \code{delayed_generic_udf}, etc.
##'
##' @param timeout_seconds Number of seconds after which to stop waiting for results.
##' Note that in-flight computationsa are not cancelled; this is not supported by the
##' underlying R package we use for concurrency.
##'
##' @param verbose If supplied, show the DAG state at the start and end, along with all node start/end.
##' Also shown are any stdout prints from the individual nodes, but these are only visible once the
##' compute node has completed.
##'
##' @param namespace The namespace to charge for any cloud costs during the execution of the
##' task graph. This can be null only when all nodes have \code{local}, or when \code{compute}
##' is called with \code{force_all_local}.
##'
##' @param force_all_local While individual nodes can be marked with \code{local=TRUE}
##' to not be executed on TileDB cloud, this flag overrides the default \code{local=FALSE}
##' for *all* nodes in the task graph.
##'
##' @return The value of the computation.
##'
##' @family {manual-layer functions}
##' @export
setGeneric("compute",
           function(node, timeout_seconds=NULL, verbose=FALSE, namespace=NULL, force_all_local=FALSE)
           standardGeneric("compute"))
setMethod("compute", signature(node = "Node"), function(node, timeout_seconds=NULL, verbose=FALSE,
  namespace=NULL, force_all_local=FALSE)
{
  node$compute(timeout_seconds=timeout_seconds, verbose=verbose, namespace=namespace, force_all_local=force_all_local)
})

##' Test/debug entrypoint for local/sequential compute.
##'
##' Runs all nodes in a correct dependency ordering, but all within the context of the same
##' process, and all locally. See also the Task Graphs vignette.
##'
##' @param node Nominally, produced by \code{delayed}, \code{delayed_generic_udf}, etc.
##'
##' @return The value of the computation.
##'
##' @family {manual-layer functions}
##' @export
setGeneric("compute_sequentially",
           function(node)
           standardGeneric("compute_sequentially"))
setMethod("compute_sequentially", signature(node = "Node"), function(node) {
  node$compute_sequentially()
})

##' Get arguments for a delayed function, as a list.
##'
##' @param node The object whose args are being set -- nominally, produced by
##' \code{delayed}, \code{delayed_generic_udf}, etc.
##'
##' @family {manual-layer functions}
##' @export
setGeneric("delayed_args",
           function(node)
           standardGeneric("delayed_args"))
setMethod("delayed_args", signature(node = "Node"), function(node) {
  node$get_args()
})

##' Set arguments for a delayed function.
##'
##' Args can be set when \code{delayed} is called, or afterward using this function.
##'
##' @param node The object whose args are being set -- nominally, produced by
##' \code{delayed}, \code{delayed_generic_udf}, etc.
##'
##' @param value A list of arguments to the delayed function, e.g. \code{list(a,b,c)}.
##'
##' @family {manual-layer functions}
##' @export
setGeneric("delayed_args<-",
           function(node, value)
           standardGeneric("delayed_args<-"))
setMethod("delayed_args<-", signature(node = "Node"), function(node, value) {
  # The R '<-' logic gives us a list with single name "value"
  stopifnot(`argument must be a list object` = is.list(value))
  node$set_args(value)
  node # '<-' generics must return the object
})

setMethod("show", signature(object = "Node"), function(object) {
  object$show()
})
