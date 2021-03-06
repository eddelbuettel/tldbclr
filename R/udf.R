# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.0.4
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title UDF
#'
#' @description UDF Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field udf_info_name  character [optional]
#'
#' @field language  \link{UDFLanguage} [optional]
#'
#' @field version  character [optional]
#'
#' @field image_name  character [optional]
#'
#' @field ranges  \link{UDFRanges} [optional]
#'
#' @field subarray  \link{UDFSubarray} [optional]
#'
#' @field exec  character [optional]
#'
#' @field exec_raw  character [optional]
#'
#' @field buffers  list( character ) [optional]
#'
#' @field result_format  \link{UDFResultType} [optional]
#'
#' @field task_name  character [optional]
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UDF <- R6::R6Class(
  'UDF',
  public = list(
    `udf_info_name` = NULL,
    `language` = NULL,
    `version` = NULL,
    `image_name` = NULL,
    `ranges` = NULL,
    `subarray` = NULL,
    `exec` = NULL,
    `exec_raw` = NULL,
    `buffers` = NULL,
    `result_format` = NULL,
    `task_name` = NULL,
    initialize = function(
        `udf_info_name`=NULL, `language`=NULL, `version`=NULL, `image_name`=NULL, `ranges`=NULL, `subarray`=NULL, `exec`=NULL, `exec_raw`=NULL, `buffers`=NULL, `result_format`=NULL, `task_name`=NULL, ...
    ) {
      local.optional.var <- list(...)
      if (!is.null(`udf_info_name`)) {
        stopifnot(is.character(`udf_info_name`), length(`udf_info_name`) == 1)
        self$`udf_info_name` <- `udf_info_name`
      }
      if (!is.null(`language`)) {
        stopifnot(R6::is.R6(`language`))
        self$`language` <- `language`
      }
      if (!is.null(`version`)) {
        stopifnot(is.character(`version`), length(`version`) == 1)
        self$`version` <- `version`
      }
      if (!is.null(`image_name`)) {
        stopifnot(is.character(`image_name`), length(`image_name`) == 1)
        self$`image_name` <- `image_name`
      }
      if (!is.null(`ranges`)) {
        stopifnot(R6::is.R6(`ranges`))
        self$`ranges` <- `ranges`
      }
      if (!is.null(`subarray`)) {
        stopifnot(R6::is.R6(`subarray`))
        self$`subarray` <- `subarray`
      }
      if (!is.null(`exec`)) {
        stopifnot(is.character(`exec`), length(`exec`) == 1)
        self$`exec` <- `exec`
      }
      if (!is.null(`exec_raw`)) {
        stopifnot(is.character(`exec_raw`), length(`exec_raw`) == 1)
        self$`exec_raw` <- `exec_raw`
      }
      if (!is.null(`buffers`)) {
        stopifnot(is.vector(`buffers`), length(`buffers`) != 0)
        sapply(`buffers`, function(x) stopifnot(is.character(x)))
        self$`buffers` <- `buffers`
      }
      if (!is.null(`result_format`)) {
        stopifnot(R6::is.R6(`result_format`))
        self$`result_format` <- `result_format`
      }
      if (!is.null(`task_name`)) {
        stopifnot(is.character(`task_name`), length(`task_name`) == 1)
        self$`task_name` <- `task_name`
      }
    },
    toJSON = function() {
      UDFObject <- list()
      if (!is.null(self$`udf_info_name`)) {
        UDFObject[['udf_info_name']] <-
          self$`udf_info_name`
      }
      if (!is.null(self$`language`)) {
        UDFObject[['language']] <-
          self$`language`$toJSON()
      }
      if (!is.null(self$`version`)) {
        UDFObject[['version']] <-
          self$`version`
      }
      if (!is.null(self$`image_name`)) {
        UDFObject[['image_name']] <-
          self$`image_name`
      }
      if (!is.null(self$`ranges`)) {
        UDFObject[['ranges']] <-
          self$`ranges`$toJSON()
      }
      if (!is.null(self$`subarray`)) {
        UDFObject[['subarray']] <-
          self$`subarray`$toJSON()
      }
      if (!is.null(self$`exec`)) {
        UDFObject[['exec']] <-
          self$`exec`
      }
      if (!is.null(self$`exec_raw`)) {
        UDFObject[['exec_raw']] <-
          self$`exec_raw`
      }
      if (!is.null(self$`buffers`)) {
        UDFObject[['buffers']] <-
          self$`buffers`
      }
      if (!is.null(self$`result_format`)) {
        UDFObject[['result_format']] <-
          self$`result_format`$toJSON()
      }
      if (!is.null(self$`task_name`)) {
        UDFObject[['task_name']] <-
          self$`task_name`
      }

      UDFObject
    },
    fromJSON = function(UDFJson) {
      UDFObject <- jsonlite::fromJSON(UDFJson)
      if (!is.null(UDFObject$`udf_info_name`)) {
        self$`udf_info_name` <- UDFObject$`udf_info_name`
      }
      if (!is.null(UDFObject$`language`)) {
        languageObject <- UDFLanguage$new()
        languageObject$fromJSON(jsonlite::toJSON(UDFObject$language, auto_unbox = TRUE, digits = NA))
        self$`language` <- languageObject
      }
      if (!is.null(UDFObject$`version`)) {
        self$`version` <- UDFObject$`version`
      }
      if (!is.null(UDFObject$`image_name`)) {
        self$`image_name` <- UDFObject$`image_name`
      }
      if (!is.null(UDFObject$`ranges`)) {
        rangesObject <- UDFRanges$new()
        rangesObject$fromJSON(jsonlite::toJSON(UDFObject$ranges, auto_unbox = TRUE, digits = NA))
        self$`ranges` <- rangesObject
      }
      if (!is.null(UDFObject$`subarray`)) {
        subarrayObject <- UDFSubarray$new()
        subarrayObject$fromJSON(jsonlite::toJSON(UDFObject$subarray, auto_unbox = TRUE, digits = NA))
        self$`subarray` <- subarrayObject
      }
      if (!is.null(UDFObject$`exec`)) {
        self$`exec` <- UDFObject$`exec`
      }
      if (!is.null(UDFObject$`exec_raw`)) {
        self$`exec_raw` <- UDFObject$`exec_raw`
      }
      if (!is.null(UDFObject$`buffers`)) {
        self$`buffers` <- ApiClient$new()$deserializeObj(UDFObject$`buffers`, "array[character]", loadNamespace("tiledbcloud"))
      }
      if (!is.null(UDFObject$`result_format`)) {
        result_formatObject <- UDFResultType$new()
        result_formatObject$fromJSON(jsonlite::toJSON(UDFObject$result_format, auto_unbox = TRUE, digits = NA))
        self$`result_format` <- result_formatObject
      }
      if (!is.null(UDFObject$`task_name`)) {
        self$`task_name` <- UDFObject$`task_name`
      }
      self
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`udf_info_name`)) {
        sprintf(
        '"udf_info_name":
          "%s"
                ',
        self$`udf_info_name`
        )},
        if (!is.null(self$`language`)) {
        sprintf(
        '"language":
        %s
        ',
        jsonlite::toJSON(self$`language`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`version`)) {
        sprintf(
        '"version":
          "%s"
                ',
        self$`version`
        )},
        if (!is.null(self$`image_name`)) {
        sprintf(
        '"image_name":
          "%s"
                ',
        self$`image_name`
        )},
        if (!is.null(self$`ranges`)) {
        sprintf(
        '"ranges":
        %s
        ',
        jsonlite::toJSON(self$`ranges`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`subarray`)) {
        sprintf(
        '"subarray":
        %s
        ',
        jsonlite::toJSON(self$`subarray`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`exec`)) {
        sprintf(
        '"exec":
          "%s"
                ',
        self$`exec`
        )},
        if (!is.null(self$`exec_raw`)) {
        sprintf(
        '"exec_raw":
          "%s"
                ',
        self$`exec_raw`
        )},
        if (!is.null(self$`buffers`)) {
        sprintf(
        '"buffers":
           [%s]
        ',
        paste(unlist(lapply(self$`buffers`, function(x) paste0('"', x, '"'))), collapse=",")
        )},
        if (!is.null(self$`result_format`)) {
        sprintf(
        '"result_format":
        %s
        ',
        jsonlite::toJSON(self$`result_format`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`task_name`)) {
        sprintf(
        '"task_name":
          "%s"
                ',
        self$`task_name`
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(UDFJson) {
      UDFObject <- jsonlite::fromJSON(UDFJson)
      self$`udf_info_name` <- UDFObject$`udf_info_name`
      self$`language` <- UDFLanguage$new()$fromJSON(jsonlite::toJSON(UDFObject$language, auto_unbox = TRUE, digits = NA))
      self$`version` <- UDFObject$`version`
      self$`image_name` <- UDFObject$`image_name`
      self$`ranges` <- UDFRanges$new()$fromJSON(jsonlite::toJSON(UDFObject$ranges, auto_unbox = TRUE, digits = NA))
      self$`subarray` <- UDFSubarray$new()$fromJSON(jsonlite::toJSON(UDFObject$subarray, auto_unbox = TRUE, digits = NA))
      self$`exec` <- UDFObject$`exec`
      self$`exec_raw` <- UDFObject$`exec_raw`
      self$`buffers` <- ApiClient$new()$deserializeObj(UDFObject$`buffers`, "array[character]", loadNamespace("tiledbcloud"))
      self$`result_format` <- UDFResultType$new()$fromJSON(jsonlite::toJSON(UDFObject$result_format, auto_unbox = TRUE, digits = NA))
      self$`task_name` <- UDFObject$`task_name`
      self
    }
  )
)

