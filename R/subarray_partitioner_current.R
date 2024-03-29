# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.2.19
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title SubarrayPartitionerCurrent
#'
#' @description SubarrayPartitionerCurrent Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field subarray  \link{Subarray} [optional]
#'
#' @field start  integer [optional]
#'
#' @field end  integer [optional]
#'
#' @field splitMultiRange  character [optional]
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SubarrayPartitionerCurrent <- R6::R6Class(
  'SubarrayPartitionerCurrent',
  public = list(
    `subarray` = NULL,
    `start` = NULL,
    `end` = NULL,
    `splitMultiRange` = NULL,
    initialize = function(
        `subarray`=NULL, `start`=NULL, `end`=NULL, `splitMultiRange`=NULL, ...
    ) {
      local.optional.var <- list(...)
      if (!is.null(`subarray`)) {
        stopifnot(R6::is.R6(`subarray`))
        self$`subarray` <- `subarray`
      }
      if (!is.null(`start`)) {
        stopifnot(is.numeric(`start`), length(`start`) == 1)
        self$`start` <- `start`
      }
      if (!is.null(`end`)) {
        stopifnot(is.numeric(`end`), length(`end`) == 1)
        self$`end` <- `end`
      }
      if (!is.null(`splitMultiRange`)) {
        self$`splitMultiRange` <- `splitMultiRange`
      }
    },
    toJSON = function() {
      SubarrayPartitionerCurrentObject <- list()
      if (!is.null(self$`subarray`)) {
        SubarrayPartitionerCurrentObject[['subarray']] <-
          self$`subarray`$toJSON()
      }
      if (!is.null(self$`start`)) {
        SubarrayPartitionerCurrentObject[['start']] <-
          self$`start`
      }
      if (!is.null(self$`end`)) {
        SubarrayPartitionerCurrentObject[['end']] <-
          self$`end`
      }
      if (!is.null(self$`splitMultiRange`)) {
        SubarrayPartitionerCurrentObject[['splitMultiRange']] <-
          self$`splitMultiRange`
      }

      SubarrayPartitionerCurrentObject
    },
    fromJSON = function(SubarrayPartitionerCurrentJson) {
      SubarrayPartitionerCurrentObject <- jsonlite::fromJSON(SubarrayPartitionerCurrentJson)
      if (!is.null(SubarrayPartitionerCurrentObject$`subarray`)) {
        subarrayObject <- Subarray$new()
        subarrayObject$fromJSON(jsonlite::toJSON(SubarrayPartitionerCurrentObject$subarray, auto_unbox = TRUE, digits = NA))
        self$`subarray` <- subarrayObject
      }
      if (!is.null(SubarrayPartitionerCurrentObject$`start`)) {
        self$`start` <- SubarrayPartitionerCurrentObject$`start`
      }
      if (!is.null(SubarrayPartitionerCurrentObject$`end`)) {
        self$`end` <- SubarrayPartitionerCurrentObject$`end`
      }
      if (!is.null(SubarrayPartitionerCurrentObject$`splitMultiRange`)) {
        self$`splitMultiRange` <- SubarrayPartitionerCurrentObject$`splitMultiRange`
      }
      self
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`subarray`)) {
        sprintf(
        '"subarray":
        %s
        ',
        jsonlite::toJSON(self$`subarray`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`start`)) {
        sprintf(
        '"start":
          %d
                ',
        self$`start`
        )},
        if (!is.null(self$`end`)) {
        sprintf(
        '"end":
          %d
                ',
        self$`end`
        )},
        if (!is.null(self$`splitMultiRange`)) {
        sprintf(
        '"splitMultiRange":
          "%s"
                ',
        self$`splitMultiRange`
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(SubarrayPartitionerCurrentJson) {
      SubarrayPartitionerCurrentObject <- jsonlite::fromJSON(SubarrayPartitionerCurrentJson)
      self$`subarray` <- Subarray$new()$fromJSON(jsonlite::toJSON(SubarrayPartitionerCurrentObject$subarray, auto_unbox = TRUE, digits = NA))
      self$`start` <- SubarrayPartitionerCurrentObject$`start`
      self$`end` <- SubarrayPartitionerCurrentObject$`end`
      self$`splitMultiRange` <- SubarrayPartitionerCurrentObject$`splitMultiRange`
      self
    }
  )
)

