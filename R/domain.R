# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.0.4
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title Domain
#'
#' @description Domain Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field type  \link{Datatype} 
#'
#' @field tileOrder  \link{Layout} 
#'
#' @field cellOrder  \link{Layout} 
#'
#' @field dimensions  list( \link{Dimension} ) 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Domain <- R6::R6Class(
  'Domain',
  public = list(
    `type` = NULL,
    `tileOrder` = NULL,
    `cellOrder` = NULL,
    `dimensions` = NULL,
    initialize = function(
        `type`, `tileOrder`, `cellOrder`, `dimensions`, ...
    ) {
      local.optional.var <- list(...)
      if (!missing(`type`)) {
        stopifnot(R6::is.R6(`type`))
        self$`type` <- `type`
      }
      if (!missing(`tileOrder`)) {
        stopifnot(R6::is.R6(`tileOrder`))
        self$`tileOrder` <- `tileOrder`
      }
      if (!missing(`cellOrder`)) {
        stopifnot(R6::is.R6(`cellOrder`))
        self$`cellOrder` <- `cellOrder`
      }
      if (!missing(`dimensions`)) {
        stopifnot(is.vector(`dimensions`), length(`dimensions`) != 0)
        sapply(`dimensions`, function(x) stopifnot(R6::is.R6(x)))
        self$`dimensions` <- `dimensions`
      }
    },
    toJSON = function() {
      DomainObject <- list()
      if (!is.null(self$`type`)) {
        DomainObject[['type']] <-
          self$`type`$toJSON()
      }
      if (!is.null(self$`tileOrder`)) {
        DomainObject[['tileOrder']] <-
          self$`tileOrder`$toJSON()
      }
      if (!is.null(self$`cellOrder`)) {
        DomainObject[['cellOrder']] <-
          self$`cellOrder`$toJSON()
      }
      if (!is.null(self$`dimensions`)) {
        DomainObject[['dimensions']] <-
          lapply(self$`dimensions`, function(x) x$toJSON())
      }

      DomainObject
    },
    fromJSON = function(DomainJson) {
      DomainObject <- jsonlite::fromJSON(DomainJson)
      if (!is.null(DomainObject$`type`)) {
        typeObject <- Datatype$new()
        typeObject$fromJSON(jsonlite::toJSON(DomainObject$type, auto_unbox = TRUE, digits = NA))
        self$`type` <- typeObject
      }
      if (!is.null(DomainObject$`tileOrder`)) {
        tileOrderObject <- Layout$new()
        tileOrderObject$fromJSON(jsonlite::toJSON(DomainObject$tileOrder, auto_unbox = TRUE, digits = NA))
        self$`tileOrder` <- tileOrderObject
      }
      if (!is.null(DomainObject$`cellOrder`)) {
        cellOrderObject <- Layout$new()
        cellOrderObject$fromJSON(jsonlite::toJSON(DomainObject$cellOrder, auto_unbox = TRUE, digits = NA))
        self$`cellOrder` <- cellOrderObject
      }
      if (!is.null(DomainObject$`dimensions`)) {
        self$`dimensions` <- ApiClient$new()$deserializeObj(DomainObject$`dimensions`, "array[Dimension]", loadNamespace("tiledbcloud"))
      }
      self
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`type`)) {
        sprintf(
        '"type":
        %s
        ',
        jsonlite::toJSON(self$`type`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`tileOrder`)) {
        sprintf(
        '"tileOrder":
        %s
        ',
        jsonlite::toJSON(self$`tileOrder`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`cellOrder`)) {
        sprintf(
        '"cellOrder":
        %s
        ',
        jsonlite::toJSON(self$`cellOrder`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`dimensions`)) {
        sprintf(
        '"dimensions":
        [%s]
',
        paste(sapply(self$`dimensions`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox=TRUE, digits = NA)), collapse=",")
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(DomainJson) {
      DomainObject <- jsonlite::fromJSON(DomainJson)
      self$`type` <- Datatype$new()$fromJSON(jsonlite::toJSON(DomainObject$type, auto_unbox = TRUE, digits = NA))
      self$`tileOrder` <- Layout$new()$fromJSON(jsonlite::toJSON(DomainObject$tileOrder, auto_unbox = TRUE, digits = NA))
      self$`cellOrder` <- Layout$new()$fromJSON(jsonlite::toJSON(DomainObject$cellOrder, auto_unbox = TRUE, digits = NA))
      self$`dimensions` <- ApiClient$new()$deserializeObj(DomainObject$`dimensions`, "array[Dimension]", loadNamespace("tiledbcloud"))
      self
    }
  )
)

