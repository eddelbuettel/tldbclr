# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.0.4
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title PublicShareFilter
#'
#' @description PublicShareFilter Class
#'
#' @format An \code{R6Class} generator object
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PublicShareFilter <- R6::R6Class(
    "PublicShareFilter",
    public = list(
        initialize = function(...) {
            local.optional.var <- list(...)
            val <- unlist(local.optional.var)
            enumvec <- .parse_PublicShareFilter()

            stopifnot(length(val) == 1L)

            if (!val %in% enumvec)
                stop("Use one of the valid values: ",
                    paste0(enumvec, collapse = ", "))
            private$value <- val
        },
        toJSON = function() {
            jsonlite::toJSON(private$value, auto_unbox = TRUE)
        },
        fromJSON = function(PublicShareFilterJson) {
            private$value <- jsonlite::fromJSON(PublicShareFilterJson,
                simplifyVector = FALSE)
            self
        },
        toJSONString = function() {
            as.character(jsonlite::toJSON(private$value,
                auto_unbox = TRUE))
        },
        fromJSONString = function(PublicShareFilterJson) {
            private$value <- jsonlite::fromJSON(PublicShareFilterJson,
                simplifyVector = FALSE)
            self
        }
    ),
    private = list(
        value = NULL
    )
)

# add to utils.R
.parse_PublicShareFilter <- function(vals) {
    res <- gsub("^\\[|\\]$", "",
        "[exclude, only]"
    )
    unlist(strsplit(res, ", "))
}


