.loadConfig <- function() {
    ## should we use rappdirs? follow the XDG_CONFIG_HOME?
    homedir <- Sys.getenv("HOME")
    if (homedir == "") stop("Need to define HOME environment variable.")  # Windows ?
    cfgfile <- file.path(homedir, ".tiledb", "cloud.json")
    if (!file.exists(cfgfile)) {
        #message("No config file 'cloud.json' found.")
        return(invisible(NULL))
    }
    cfg <- jsonlite::fromJSON(cfgfile)
}

##' Configure TileDB Cloud
##'
##' Provide the setup configuration for the TileDB Cloud package.
##'
##' It considers four different environment variables:
##' \code{TILEDB_REST_TOKEN}, \code{TILEDB_REST_HOST},
##' \code{TILEDB_REST_USERNAME}, and \code{TILEDB_REST_PASSWORD}.
##'
##' It also reads a configuration file from a file
##' \code{.tiledb/httpshocloud.json}
##'
##' To operate, \emph{either} an API token has to be provided and will
##' be used, \emph{or} the username and password combination will be
##' used to login with a new session.
##'
##' @return A named vector with configuration values is returned.
##'
config <- function() {

    ## start with environment variables; R returns "" if unset by default
    token      <- Sys.getenv("TILEDB_REST_TOKEN")
    host       <- Sys.getenv("TILEDB_REST_HOST")
    username   <- Sys.getenv("TILEDB_REST_USERNAME")
    password   <- Sys.getenv("TILEDB_REST_PASSWORD")
    verify_ssl <- TRUE

    cfg <- .loadConfig()
    if (!is.null(cfg)) {
        if ("username" %in% names(cfg))  username <- cfg$username
        if ("password" %in% names(cfg))  password <- cfg$password
        if (host == "")  host <- gsub("/v1(/)?$", "", cfg$host) # scrubs trailing /v1 or /v1/
        if (token == "" && !is.null(cfg$api_key[[1]])) token <- cfg$api_key[[1]]
        verify_ssl <- cfg$verify_ssl
    }

    ## check token or username set again?  done in .onAttach

    ## fallback defaults
    if (host == "") host <- "https://api.tiledb.com"

    configuration <- c(api_key    = token,
                       username   = username,
                       password   = password,
                       host       = host,
                       verify_ssl = verify_ssl,
                       logged_in  = "FALSE")
}

