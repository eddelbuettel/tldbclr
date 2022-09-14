
.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
    ## Set configuration values from environment variable and/or a config file
    .pkgenv[["config"]] <- configure()
}

.onAttach <- function(libname, pkgname) {
    if (!.isGood()) {
        packageStartupMessage(paste("The TileDB Cloud R integration needs either an API ",
                                    "key, or a username and password. See 'help(config)'."))
    } else {
        ## log in by default (provided sufficient settings)
        login()
    }
}

## Unexported helper functions to get and set values, and to check combinations
.getConfigValue <- function(key) {
    names <- names(.pkgenv[["config"]])
    if (! key %in% names) stop("Key '", key, "' not found.", call. = FALSE)
    .pkgenv[["config"]][[key]]
}

.setConfigValue <- function(key, value) {
    names <- names(.pkgenv[["config"]])
    if (! key %in% names) stop("Key '", key, "' not found.", call. = FALSE)
    .pkgenv[["config"]][[key]] <- value
}

.isGood <- function() {
    tok <- .getConfigValue("api_key")
    usr <- .getConfigValue("username")
    pwd <- .getConfigValue("password")
    good <- tok != "" || (usr != "" && pwd != "")
}

##' @import R6
##' @import base64enc
##' @import httr
NULL
