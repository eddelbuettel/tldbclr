# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.2.19
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title UDFActions
#'
#' @description UDFActions Class
#'
#' @format An \code{R6Class} generator object
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UDFActions <- R6::R6Class(
    "UDFActions",
    public = list(
        initialize = function(...) {
            local.optional.var <- list(...)
            val <- unlist(local.optional.var)

            # MANUAL EDIT AFTER OPENAPI AUTOGEN
            .check_openapi_enum("UDFActions", val, .parse_UDFActions())

            private$value <- val
        },
        toJSON = function() {
            jsonlite::toJSON(private$value, auto_unbox = TRUE)
        },
        fromJSON = function(UDFActionsJson) {
            private$value <- jsonlite::fromJSON(UDFActionsJson,
                simplifyVector = FALSE)
            self
        },
        toJSONString = function() {
            as.character(jsonlite::toJSON(private$value,
                auto_unbox = TRUE))
        },
        fromJSONString = function(UDFActionsJson) {
            private$value <- jsonlite::fromJSON(UDFActionsJson,
                simplifyVector = FALSE)
            self
        }
    ),
    private = list(
        value = NULL
    )
)

# add to utils.R
.parse_UDFActions <- function(vals) {
    res <- gsub("^\\[|\\]$", "",
        "[fetch_udf, share_udf]"
    )
    unlist(strsplit(res, ", "))
}


