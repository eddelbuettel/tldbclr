# ================================================================
# DAG is an R reference class. We need references since we have a mutable DAG of nodes
# and we must be able to update them as their computation progresses.
#
# Nominally this is invoked by the terminal node's compute() method.
#
# Notes on dev/debug: Please see the developer vignette about task graphs.
# ================================================================

##' @export
##' @importFrom future plan multicore multisession
DAG <- R6::R6Class(
  'DAG',
  public = list(
    all_nodes      = NULL,
    initial_nodes  = NULL,
    terminal_node  = NULL,

    # ================================================================
    # INITIALIZATION METHODS

    initialize = function(terminal_node) {
      self$populate(terminal_node)
    },

    # Nodes already have a graph structure. For example:
    #   a <- delayed(function()    {    9  },                    name='a', local=FALSE)
    #   b <- delayed(function(x)   {  10*x },    args=list(a),   name='b', local=FALSE)
    #   c <- delayed(function(x)   { 100*x },    args=list(a),   name='c', local=FALSE)
    #   d <- delayed(function(...) { sum(...) }, args=list(b,c), name='d', local=FALSE)
    # has DAG like
    #    a
    #   / \
    #  v   v
    #  b   c
    #   \ /
    #    v
    #    d
    #
    # What we do here is locate the initial nodes and the terminal node.
    populate = function(terminal_node) {
      if (!is(terminal_node, "Node")) {
        stop("Terminal node must be an instance of the Node class; got ", class(terminal_node))
      }
      self$all_nodes <- list()
      self$initial_nodes <- list()
      self$populate_aux(terminal_node)
      if (length(self$all_nodes) == 0) {
        stop("Task graph is empty")
      }
      if (length(self$initial_nodes) == 0) {
        stop("Task graph has a cycle")
      }

      self$terminal_node <- terminal_node

      # This makes our print output non-frustrating for the user to read
      self$sort_nodes_topologically()
    },

    # This is a recursive helper function for populate().
    populate_aux = function(node) {
      if (!is.null(self$all_nodes[[node$id]])) {
        # In R, just "return" does not actually ... return. :^/
        return()
      }

      self$all_nodes[[node$id]] <- node

      if (node$is_initial()) {
        self$initial_nodes[[node$id]] <- node
      }

      for (arg in node$args) {
        if (is(arg, "Node")) {
          self$populate_aux(arg)
        }
      }
    },

    # This is important so our status display is non-frustrating for the user.
    # This assumes a cyclicity check has already been done.
    #
    # See https://en.wikipedia.org/wiki/Topological_sorting
    #
    # Note that this algorithm requires a destroyable copy of the graph, and it
    # requires both forward and reverse arrows. For purpose of this function we
    # use two R named-lists-of-named-lists as forward and reverse adjacency
    # matrices.
    sort_nodes_topologically = function() {
      if (is.null(self$initial_nodes) || length(self$initial_nodes) == 0) {
        stop("initial nodes must be identified before topological sort")
      }

      # Forward: from each arg to its calling node. E.g. if b = f(a), then row 'a' has column 'b'.
      # Reverse: rom each node to its arg. E.g. if b = f(a), then row 'b' has column 'a'.
      # Note forward and reverse are simply transposes of one another but it's easiest to
      # keep two copies.
      fwd_adj_mx <- spmx$new()
      rev_adj_mx <- spmx$new()
      for (node in self$all_nodes) {
        for (arg in node$args) {
          if (is(arg, "Node")) {
            fwd_adj_mx$add(arg$id, node$id)
            rev_adj_mx$add(node$id, arg$id)
          }
        }
      }
      # sorted_nodes ← Empty list that will contain the sorted elements
      sorted_nodes <- list()
      # initial_nodes ← Set of all nodes with no incoming edge
      initial_nodes <- self$initial_nodes

      # while initial_nodes is not empty do
      while (length(initial_nodes) > 0) {
        # remove node from initial_nodes
        node <- initial_nodes[[1]]
        initial_nodes[[1]] <- NULL

        # add node to sorted_nodes
        sorted_nodes[[node$id]] <- node

        # for each other node on a forward edge from this node, i.e. for us,
        # other nodes having this one as an argument:
        for (other_id in names(fwd_adj_mx$get_row(node$id))) {
          other <- self$all_nodes[[other_id]]

          # remove that edge from the graph
          fwd_adj_mx$remove(node$id, other$id)
          rev_adj_mx$remove(other$id, node$id)

          # if other has no other incoming edges then
          if (length(rev_adj_mx$get_row(other$id)) == 0) {
            # insert other into initial_nodes
            initial_nodes[[other$id]] <- other
          }
        }
      }

      # return sorted_nodes (a topologically sorted order)
      self$all_nodes <- sorted_nodes
    },

    # ================================================================
    # COMPUTE

    # This is a synchronous wait.  If more inspection is desired, then one can do
    # dag <- terminal_node$make_dag(), then dag$poll() and show(dag) / str(dag)
    # repeatedly in order better to visualize the flow of computation through the
    # graph.
    #
    # The namespace here is one to be applied to the entire DAG. Individual nodes may
    # have been constructed with their own overrides, which will be honored on node$poll().
    compute = function(timeout_seconds=NULL, verbose=FALSE, namespace=NULL, force_all_local=FALSE) {
      self$reset()

      if (verbose) show(self)

      # This is crucial for our use of the future package -- we don't get parallelism by default
      #future::plan(future::multisession)
      future::plan(future::multicore)

      start <- Sys.time()
      while (self$poll(verbose=verbose, namespace=namespace, force_all_local=force_all_local) == FALSE) {
        now <- Sys.time()
        elapsed_seconds <- as.numeric(now-start, units='secs')
        if (!is.null(timeout_seconds)) {
          if (elapsed_seconds > timeout_seconds) {
            stop("Timeout")
            break
          }
        }

        Sys.sleep(0.1)
      }

      if (verbose) show(self)
      self$terminal_node$result
    },

    # This is for reruns in case of transient node failures.
    reset = function() {
      for (node in self$all_nodes) {
        node$reset()
      }
    },

    # This *must* be called periodically to update nodes and launch dependents.
    # Our poll-driven DAGs won't auto-run without this being invoked
    # periodically.
    #
    # The namespace must be non-null for cloud execution. For all-local runs it can be null.
    # We check this at compute time.
    poll = function(verbose=FALSE, namespace=NULL, force_all_local=FALSE) {
      terminal_done <- self$terminal_node$poll(verbose=verbose, namespace=namespace, force_local=force_all_local)
    },

    # ================================================================
    # DEBUG/DISPLAY

    get_display_names_for_nodes = function(nodes) {
      sapply(nodes, function(node) { node$name })
    },

    show_node_list = function(description, nodes) {
      count <- length(nodes)
      display_names <- as.vector(self$get_display_names_for_nodes(nodes))
      display_names_string <- paste(display_names, collapse=", ")
      cat(description, " (", count, ") ", display_names_string, "\n", sep="")
    },

    show = function() {
      self$show_node_list("All      nodes:   ", self$all_nodes)
      self$show_node_list("Initial  nodes:   ", self$initial_nodes)
      self$show_node_list("Terminal node:    ", list(self$terminal_node))
      cat("Dependencies:\n")
      for (node in self$all_nodes) {
        if (length(node$args) > 0) {
          deps <- node$args[sapply(node$args, function(arg) {is(arg, "Node")})]
          self$show_node_list(paste0("  ", node$name), deps)
        } else {
          self$show_node_list(paste0("  ", node$name), list())
        }
      }
      cat("Statuses:\n")
      for (node in self$all_nodes) {
        node$show_status()
      }
    },

    # This is (for now) a dev/debug function. It prints out data suitable
    # for piping to `dot -T pdf > viz.pdf` for visualizing complex DAGs.
    show_dot = function() {
      cat("digraph {\n")
      self$show_dot_aux(self$terminal_node)
      cat("}\n")
    },

    # Recursive helper function for show_dot.
    show_dot_aux = function(node) {
      if (is(node, "Node")) {
        for (arg in node$args) {
          if (is(arg, "Node")) {
            cat(arg$name, "->", node$name, "\n")
            self$show_dot_aux(arg)
          }
        }
      }
    }

  )
)

# ================================================================
# This is a little sparse adjacency matrix solely for the use of the topological sorter.
# It's a named list of named lists of 1s, e.g. if there is an edge from 'a' to 'b'
# then rows is list(a=list(b=1)).
spmx <- R6::R6Class(
  'spmx',
  public = list(
    rows = list(),

    add = function(from, to) {
      if (!(from %in% names(self$rows))) {
        self$rows[[from]] <- list()
      }
      self$rows[[from]][[to]] <- 1
    },

    get_row = function(from) {
      self$rows[[from]]
    },

    remove = function(from, to) {
      self$rows[[from]][[to]] <- NULL
    },

    show = function() {
      for (rid in names(self$rows)) {
        cat(rid, ": ", paste(names(self$rows[[rid]]), collapse=", "), "\n")
      }
    }

  )
)

# ================================================================
# GENERICS

setMethod("show", signature(object = "DAG"), function(object) {
  object$show()
})
