##' TileDB Cloud Login Helper
##'
##' This function can be used to override the default setup made at
##' package load.
##'
##' It can operate in two modes. Either, a username and a password are
##' supplied as environment variable \code{TILEDB_REST_USERNAME} and
##' \code{TILEDB_REST_PASSWORD}.  As an alternative, an access token
##' can be supplied via \code{TILEDB_REST_TOKEN}.  The values are used
##' to instantiate a new API client object. If no token was supplied a
##' new session is requested and the token assigned to that session is
##' used.
##'
##' Funtion arguments are optional, and can be used to override the
##' default configuration values obtained by \code{config()} from
##' either the environment variables or the configuration file.
##'
##' @param username A character value with the username, if present
##' password is also needed.
##'
##' @param password A character value with the password, if present
##' username is also needed.
##'
##' @param api_key A character value with the access token, it can be
##' used instead of username and password.
##'
##' @param host A character value with remote host to connect to.
##'
##' @param remember_me A boolean to select a session with for 24 hours
##' instead of 8 hours, used only when a new session is requested.
##'
##' @return Nothing is returned, the function is called for a side effect
##' of storing the values in the package environment
##'
login <- function(username, password, api_key, host, remember_me=TRUE) {
    if (missing(username)) username <- .getConfigValue("username")
    if (missing(password)) password <- .getConfigValue("password")
    if (missing(api_key))  api_key  <- .getConfigValue("api_key")
    if (missing(host))     host     <- .getConfigValue("host")

                                        #print(c(usr=username,pwd=password,tok=token))
    good <- api_key != "" || (username != "" && password != "")
    if (!good) {
        warning("Need either 'username' + 'password', or 'api_key', ",
                "to create login session.")
        return(invisible(NULL))
    }

    cl <- ApiClient$new(basePath=paste(host, "v1", sep="/"),
                        accessToken=api_key,
                        username=username,
                        password=password)

    api <- UserApi$new(cl)
    api$apiClient$apiKeys['X-TILEDB-REST-API-KEY'] <- api_key

    ## if there is not api token key, request one
    if (api_key == "") {
        ## request a session
        sess <- api$GetSession(remember.me = remember_me)

        ## and proceed with the assigned token
        api_key <- sess$token
        api$apiClient$apiKeys['X-TILEDB-REST-API-KEY'] <- api_key
        ## and store it
        .setConfigValue("api_key", api_key)
    }

    ## use as a possible test
    res <- api$GetUser()
    cat("GetUser() got name", res$name, "\n")

    ## we do not store username and password, but update
    .setConfigValue("username", "")
    .setConfigValue("password", "")
    .setConfigValue("logged_in", "TRUE")

    ## cache api and client instances
    .pkgenv[["api"]] <- api
    .pkgenv[["cl"]]  <- cl

    invisible()
}
