# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.0.4
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title QueryReader
#'
#' @description QueryReader Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field layout  \link{Layout} [optional]
#'
#' @field subarray  \link{Subarray} [optional]
#'
#' @field readState  \link{ReadState} [optional]
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
QueryReader <- R6::R6Class(
  'QueryReader',
  public = list(
    `layout` = NULL,
    `subarray` = NULL,
    `readState` = NULL,
    initialize = function(
        `layout`=NULL, `subarray`=NULL, `readState`=NULL, ...
    ) {
      local.optional.var <- list(...)
      if (!is.null(`layout`)) {
        stopifnot(R6::is.R6(`layout`))
        self$`layout` <- `layout`
      }
      if (!is.null(`subarray`)) {
        stopifnot(R6::is.R6(`subarray`))
        self$`subarray` <- `subarray`
      }
      if (!is.null(`readState`)) {
        stopifnot(R6::is.R6(`readState`))
        self$`readState` <- `readState`
      }
    },
    toJSON = function() {
      QueryReaderObject <- list()
      if (!is.null(self$`layout`)) {
        QueryReaderObject[['layout']] <-
          self$`layout`$toJSON()
      }
      if (!is.null(self$`subarray`)) {
        QueryReaderObject[['subarray']] <-
          self$`subarray`$toJSON()
      }
      if (!is.null(self$`readState`)) {
        QueryReaderObject[['readState']] <-
          self$`readState`$toJSON()
      }

      QueryReaderObject
    },
    fromJSON = function(QueryReaderJson) {
      QueryReaderObject <- jsonlite::fromJSON(QueryReaderJson)
      if (!is.null(QueryReaderObject$`layout`)) {
        layoutObject <- Layout$new()
        layoutObject$fromJSON(jsonlite::toJSON(QueryReaderObject$layout, auto_unbox = TRUE, digits = NA))
        self$`layout` <- layoutObject
      }
      if (!is.null(QueryReaderObject$`subarray`)) {
        subarrayObject <- Subarray$new()
        subarrayObject$fromJSON(jsonlite::toJSON(QueryReaderObject$subarray, auto_unbox = TRUE, digits = NA))
        self$`subarray` <- subarrayObject
      }
      if (!is.null(QueryReaderObject$`readState`)) {
        readStateObject <- ReadState$new()
        readStateObject$fromJSON(jsonlite::toJSON(QueryReaderObject$readState, auto_unbox = TRUE, digits = NA))
        self$`readState` <- readStateObject
      }
      self
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`layout`)) {
        sprintf(
        '"layout":
        %s
        ',
        jsonlite::toJSON(self$`layout`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`subarray`)) {
        sprintf(
        '"subarray":
        %s
        ',
        jsonlite::toJSON(self$`subarray`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`readState`)) {
        sprintf(
        '"readState":
        %s
        ',
        jsonlite::toJSON(self$`readState`$toJSON(), auto_unbox=TRUE, digits = NA)
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(QueryReaderJson) {
      QueryReaderObject <- jsonlite::fromJSON(QueryReaderJson)
      self$`layout` <- Layout$new()$fromJSON(jsonlite::toJSON(QueryReaderObject$layout, auto_unbox = TRUE, digits = NA))
      self$`subarray` <- Subarray$new()$fromJSON(jsonlite::toJSON(QueryReaderObject$subarray, auto_unbox = TRUE, digits = NA))
      self$`readState` <- ReadState$new()$fromJSON(jsonlite::toJSON(QueryReaderObject$readState, auto_unbox = TRUE, digits = NA))
      self
    }
  )
)

