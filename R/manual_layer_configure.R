
.loadConfig <- function() {
    ## should we use rappdirs? follow the XDG_CONFIG_HOME?
    homedir <- Sys.getenv("HOME")
    if (homedir == "") {
        warning("No HOME environment variable.")  # Windows ?
        return(NULL)
    }
    cfgfile <- file.path(homedir, ".tiledb", "cloud.json")
    if (!file.exists(cfgfile)) {
        #message("No config file 'cloud.json' found.")
        return(invisible(NULL))
    }
    cfg <- jsonlite::fromJSON(cfgfile)
}

.storeConfig <- function(homedir=Sys.getenv("HOME")) {
    if (homedir == "") {
        stop("No HOME environment variable or homedir value.", call. =FALSE)  # Windows ?
    }
    cfgfile <- file.path(homedir, ".tiledb", "cloud.json")

    cfgdata <- jsonlite::toJSON(.pkgenv[["config"]], auto_unbox=TRUE, pretty=TRUE)

    write(cfgdata, cfgfile)

    verbose <- getOption("verbose", "false")
    if (verbose) {
      cat("Wrote", cfgfile, "\n")
    }
}

##' Configure TileDB Cloud
##'
##' Provide the setup configuration for the TileDB Cloud package.
##'
##' It considers four different environment variables:
##' \code{TILEDB_REST_TOKEN}, \code{TILEDB_REST_HOST},
##' \code{TILEDB_REST_USERNAME}, and \code{TILEDB_REST_PASSWORD}.
##'
##' It also reads a configuration file from a file \code{.tiledb/cloud.json}.
##'
##' To operate, \emph{either} an API token has to be provided and will
##' be used, \emph{or} the username and password combination will be
##' used to log in with a new session.
##'
##' @return A named vector with configuration values is returned.
##' @export
configure <- function() {

    ## Start with environment variables; R returns "" if unset by default
    token      <- Sys.getenv("TILEDB_REST_TOKEN")
    host       <- Sys.getenv("TILEDB_REST_HOST")
    username   <- Sys.getenv("TILEDB_REST_USERNAME")
    password   <- Sys.getenv("TILEDB_REST_PASSWORD")
    verify_ssl <- TRUE

    cfg <- .loadConfig()                # load a config file (if one found)
    if (!is.null(cfg)) {
        if ("username" %in% names(cfg))  username <- cfg$username
        if ("password" %in% names(cfg))  password <- cfg$password
        if (host == "")  host <- gsub("/v1(/)?$", "", cfg$host) # scrubs trailing /v1 or /v1/
        # If the previous login was without a session-token requested then the cloud-config
        # JSON file will have username and password but no API key.
        if (token == "") {
          if (!is.null(cfg$api_key) && length(cfg$api_key) > 0 && !is.null(cfg$api_key[[1]])) {
              token <- cfg$api_key[[1]]
          }
        }
        verify_ssl <- cfg$verify_ssl
    }

    ## Check token or username set again?  done in .onAttach

    ## Fallback defaults
    if (host == "") host <- "https://api.tiledb.com"

    # We use jsonlite to persist sessions. In turn, jsonlite::toJSON will *not*
    # retain names if the configuration is a named vector. We must use a named
    # list.  Example: toJSON(c(a=1,b=2)) is '[1,2]' (bad) but
    # toJSON(list(a=1,b=2),auto_unbox=TRUE) is '{"a":1,"b":2}' (good).
    configuration <- list(api_key    = token,
                          username   = username,
                          password   = password,
                          host       = host,
                          verify_ssl = verify_ssl,
                          logged_in  = "FALSE")
    configuration
}
