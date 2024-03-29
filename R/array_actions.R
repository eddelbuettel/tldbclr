# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.2.19
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title ArrayActions
#'
#' @description ArrayActions Class
#'
#' @format An \code{R6Class} generator object
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ArrayActions <- R6::R6Class(
    "ArrayActions",
    public = list(
        initialize = function(...) {
            local.optional.var <- list(...)
            val <- unlist(local.optional.var)

            # MANUAL EDIT AFTER OPENAPI AUTOGEN
            .check_openapi_enum("ArrayActions", val, .parse_ArrayActions())

            private$value <- val
        },
        toJSON = function() {
            jsonlite::toJSON(private$value, auto_unbox = TRUE)
        },
        fromJSON = function(ArrayActionsJson) {
            private$value <- jsonlite::fromJSON(ArrayActionsJson,
                simplifyVector = FALSE)
            self
        },
        toJSONString = function() {
            as.character(jsonlite::toJSON(private$value,
                auto_unbox = TRUE))
        },
        fromJSONString = function(ArrayActionsJson) {
            private$value <- jsonlite::fromJSON(ArrayActionsJson,
                simplifyVector = FALSE)
            self
        }
    ),
    private = list(
        value = NULL
    )
)

# add to utils.R
.parse_ArrayActions <- function(vals) {
    res <- gsub("^\\[|\\]$", "",
        "[read, write, edit, read_array_logs, read_array_info, read_array_schema]"
    )
    unlist(strsplit(res, ", "))
}


