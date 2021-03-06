# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.0.4
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title UDFLanguage
#'
#' @description UDFLanguage Class
#'
#' @format An \code{R6Class} generator object
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UDFLanguage <- R6::R6Class(
    "UDFLanguage",
    public = list(
        initialize = function(...) {
            local.optional.var <- list(...)
            val <- unlist(local.optional.var)
            enumvec <- .parse_UDFLanguage()

            stopifnot(length(val) == 1L)

            if (!val %in% enumvec)
                stop("Use one of the valid values: ",
                    paste0(enumvec, collapse = ", "))
            private$value <- val
        },
        toJSON = function() {
            jsonlite::toJSON(private$value, auto_unbox = TRUE)
        },
        fromJSON = function(UDFLanguageJson) {
            private$value <- jsonlite::fromJSON(UDFLanguageJson,
                simplifyVector = FALSE)
            self
        },
        toJSONString = function() {
            as.character(jsonlite::toJSON(private$value,
                auto_unbox = TRUE))
        },
        fromJSONString = function(UDFLanguageJson) {
            private$value <- jsonlite::fromJSON(UDFLanguageJson,
                simplifyVector = FALSE)
            self
        }
    ),
    private = list(
        value = NULL
    )
)

# add to utils.R
.parse_UDFLanguage <- function(vals) {
    res <- gsub("^\\[|\\]$", "",
        "[python]"
    )
    unlist(strsplit(res, ", "))
}


