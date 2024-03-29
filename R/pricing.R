# TileDB Storage Platform API
#
# TileDB Storage Platform REST API
#
# The version of the OpenAPI document: 2.2.19
# 
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title Pricing
#'
#' @description Pricing Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field id  character [optional]
#'
#' @field array_uuid  character [optional]
#'
#' @field pricing_name  character [optional]
#'
#' @field pricing_type  \link{PricingType} [optional]
#'
#' @field product_name  character [optional]
#'
#' @field product_statement_descriptor  character [optional]
#'
#' @field product_unit_label  \link{PricingUnitLabel} [optional]
#'
#' @field currency  \link{PricingCurrency} [optional]
#'
#' @field aggregate_usage  \link{PricingAggregateUsage} [optional]
#'
#' @field interval  \link{PricingInterval} [optional]
#'
#' @field divided_by  integer [optional]
#'
#' @field charge  numeric [optional]
#'
#' @field activated  character [optional]
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Pricing <- R6::R6Class(
  'Pricing',
  public = list(
    `id` = NULL,
    `array_uuid` = NULL,
    `pricing_name` = NULL,
    `pricing_type` = NULL,
    `product_name` = NULL,
    `product_statement_descriptor` = NULL,
    `product_unit_label` = NULL,
    `currency` = NULL,
    `aggregate_usage` = NULL,
    `interval` = NULL,
    `divided_by` = NULL,
    `charge` = NULL,
    `activated` = NULL,
    initialize = function(
        `id`=NULL, `array_uuid`=NULL, `pricing_name`=NULL, `pricing_type`=NULL, `product_name`=NULL, `product_statement_descriptor`=NULL, `product_unit_label`=NULL, `currency`=NULL, `aggregate_usage`=NULL, `interval`=NULL, `divided_by`=NULL, `charge`=NULL, `activated`=NULL, ...
    ) {
      local.optional.var <- list(...)
      if (!is.null(`id`)) {
        stopifnot(is.character(`id`), length(`id`) == 1)
        self$`id` <- `id`
      }
      if (!is.null(`array_uuid`)) {
        stopifnot(is.character(`array_uuid`), length(`array_uuid`) == 1)
        self$`array_uuid` <- `array_uuid`
      }
      if (!is.null(`pricing_name`)) {
        stopifnot(is.character(`pricing_name`), length(`pricing_name`) == 1)
        self$`pricing_name` <- `pricing_name`
      }
      if (!is.null(`pricing_type`)) {
        stopifnot(R6::is.R6(`pricing_type`))
        self$`pricing_type` <- `pricing_type`
      }
      if (!is.null(`product_name`)) {
        stopifnot(is.character(`product_name`), length(`product_name`) == 1)
        self$`product_name` <- `product_name`
      }
      if (!is.null(`product_statement_descriptor`)) {
        stopifnot(is.character(`product_statement_descriptor`), length(`product_statement_descriptor`) == 1)
        self$`product_statement_descriptor` <- `product_statement_descriptor`
      }
      if (!is.null(`product_unit_label`)) {
        stopifnot(R6::is.R6(`product_unit_label`))
        self$`product_unit_label` <- `product_unit_label`
      }
      if (!is.null(`currency`)) {
        stopifnot(R6::is.R6(`currency`))
        self$`currency` <- `currency`
      }
      if (!is.null(`aggregate_usage`)) {
        stopifnot(R6::is.R6(`aggregate_usage`))
        self$`aggregate_usage` <- `aggregate_usage`
      }
      if (!is.null(`interval`)) {
        stopifnot(R6::is.R6(`interval`))
        self$`interval` <- `interval`
      }
      if (!is.null(`divided_by`)) {
        stopifnot(is.numeric(`divided_by`), length(`divided_by`) == 1)
        self$`divided_by` <- `divided_by`
      }
      if (!is.null(`charge`)) {
        stopifnot(is.numeric(`charge`), length(`charge`) == 1)
        self$`charge` <- `charge`
      }
      if (!is.null(`activated`)) {
        self$`activated` <- `activated`
      }
    },
    toJSON = function() {
      PricingObject <- list()
      if (!is.null(self$`id`)) {
        PricingObject[['id']] <-
          self$`id`
      }
      if (!is.null(self$`array_uuid`)) {
        PricingObject[['array_uuid']] <-
          self$`array_uuid`
      }
      if (!is.null(self$`pricing_name`)) {
        PricingObject[['pricing_name']] <-
          self$`pricing_name`
      }
      if (!is.null(self$`pricing_type`)) {
        PricingObject[['pricing_type']] <-
          self$`pricing_type`$toJSON()
      }
      if (!is.null(self$`product_name`)) {
        PricingObject[['product_name']] <-
          self$`product_name`
      }
      if (!is.null(self$`product_statement_descriptor`)) {
        PricingObject[['product_statement_descriptor']] <-
          self$`product_statement_descriptor`
      }
      if (!is.null(self$`product_unit_label`)) {
        PricingObject[['product_unit_label']] <-
          self$`product_unit_label`$toJSON()
      }
      if (!is.null(self$`currency`)) {
        PricingObject[['currency']] <-
          self$`currency`$toJSON()
      }
      if (!is.null(self$`aggregate_usage`)) {
        PricingObject[['aggregate_usage']] <-
          self$`aggregate_usage`$toJSON()
      }
      if (!is.null(self$`interval`)) {
        PricingObject[['interval']] <-
          self$`interval`$toJSON()
      }
      if (!is.null(self$`divided_by`)) {
        PricingObject[['divided_by']] <-
          self$`divided_by`
      }
      if (!is.null(self$`charge`)) {
        PricingObject[['charge']] <-
          self$`charge`
      }
      if (!is.null(self$`activated`)) {
        PricingObject[['activated']] <-
          self$`activated`
      }

      PricingObject
    },
    fromJSON = function(PricingJson) {
      PricingObject <- jsonlite::fromJSON(PricingJson)
      if (!is.null(PricingObject$`id`)) {
        self$`id` <- PricingObject$`id`
      }
      if (!is.null(PricingObject$`array_uuid`)) {
        self$`array_uuid` <- PricingObject$`array_uuid`
      }
      if (!is.null(PricingObject$`pricing_name`)) {
        self$`pricing_name` <- PricingObject$`pricing_name`
      }
      if (!is.null(PricingObject$`pricing_type`)) {
        # MANUAL EDIT AFTER OPENAPI AUTOGEN
        # For enums, OpenAPI autogen (1) generates a constructor which requires being called
        # with one arguent; (2) generates callsites (such as here) that calls that constructor
        # with zero arguments.
        #
        #pricing_typeObject <- PricingType$new()
        #pricing_typeObject$fromJSON(jsonlite::toJSON(PricingObject$pricing_type, auto_unbox = TRUE, digits = NA))
        pricing_typeObject <- PricingType$new(PricingObject$pricing_type)
        self$`pricing_type` <- pricing_typeObject
      }
      if (!is.null(PricingObject$`product_name`)) {
        self$`product_name` <- PricingObject$`product_name`
      }
      if (!is.null(PricingObject$`product_statement_descriptor`)) {
        self$`product_statement_descriptor` <- PricingObject$`product_statement_descriptor`
      }
      if (!is.null(PricingObject$`product_unit_label`)) {
        # MANUAL EDIT AFTER OPENAPI AUTOGEN
        # For enums, OpenAPI autogen (1) generates a constructor which requires being called
        # with one arguent; (2) generates callsites (such as here) that calls that constructor
        # with zero arguments.
        #
        #product_unit_labelObject <- PricingUnitLabel$new()
        #product_unit_labelObject$fromJSON(jsonlite::toJSON(PricingObject$product_unit_label, auto_unbox = TRUE, digits = NA))
        product_unit_labelObject <- PricingUnitLabel$new(PricingObject$product_unit_label)
        self$`product_unit_label` <- product_unit_labelObject
      }
      if (!is.null(PricingObject$`currency`)) {
        # MANUAL EDIT AFTER OPENAPI AUTOGEN
        # For enums, OpenAPI autogen (1) generates a constructor which requires being called
        # with one arguent; (2) generates callsites (such as here) that calls that constructor
        # with zero arguments.
        #
        #currencyObject <- PricingCurrency$new()
        #currencyObject$fromJSON(jsonlite::toJSON(PricingObject$currency, auto_unbox = TRUE, digits = NA))
        currencyObject <- PricingCurrency$new(PricingObject$currency)
        self$`currency` <- currencyObject
      }
      if (!is.null(PricingObject$`aggregate_usage`)) {
        # MANUAL EDIT AFTER OPENAPI AUTOGEN
        # For enums, OpenAPI autogen (1) generates a constructor which requires being called
        # with one arguent; (2) generates callsites (such as here) that calls that constructor
        # with zero arguments.
        #
        #aggregate_usageObject <- PricingAggregateUsage$new()
        #aggregate_usageObject$fromJSON(jsonlite::toJSON(PricingObject$aggregate_usage, auto_unbox = TRUE, digits = NA))
        aggregate_usageObject <- PricingAggregateUsage$new(PricingObject$aggregate_usage)
        self$`aggregate_usage` <- aggregate_usageObject
      }
      if (!is.null(PricingObject$`interval`)) {
        # MANUAL EDIT AFTER OPENAPI AUTOGEN
        # For enums, OpenAPI autogen (1) generates a constructor which requires being called
        # with one arguent; (2) generates callsites (such as here) that calls that constructor
        # with zero arguments.
        #
        #intervalObject <- PricingInterval$new()
        #intervalObject$fromJSON(jsonlite::toJSON(PricingObject$interval, auto_unbox = TRUE, digits = NA))
        intervalObject <- PricingInterval$new(PricingObject$interval)
        self$`interval` <- intervalObject
      }
      if (!is.null(PricingObject$`divided_by`)) {
        self$`divided_by` <- PricingObject$`divided_by`
      }
      if (!is.null(PricingObject$`charge`)) {
        self$`charge` <- PricingObject$`charge`
      }
      if (!is.null(PricingObject$`activated`)) {
        self$`activated` <- PricingObject$`activated`
      }
      self
    },
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
        sprintf(
        '"id":
          "%s"
                ',
        self$`id`
        )},
        if (!is.null(self$`array_uuid`)) {
        sprintf(
        '"array_uuid":
          "%s"
                ',
        self$`array_uuid`
        )},
        if (!is.null(self$`pricing_name`)) {
        sprintf(
        '"pricing_name":
          "%s"
                ',
        self$`pricing_name`
        )},
        if (!is.null(self$`pricing_type`)) {
        sprintf(
        '"pricing_type":
        %s
        ',
        jsonlite::toJSON(self$`pricing_type`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`product_name`)) {
        sprintf(
        '"product_name":
          "%s"
                ',
        self$`product_name`
        )},
        if (!is.null(self$`product_statement_descriptor`)) {
        sprintf(
        '"product_statement_descriptor":
          "%s"
                ',
        self$`product_statement_descriptor`
        )},
        if (!is.null(self$`product_unit_label`)) {
        sprintf(
        '"product_unit_label":
        %s
        ',
        jsonlite::toJSON(self$`product_unit_label`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`currency`)) {
        sprintf(
        '"currency":
        %s
        ',
        jsonlite::toJSON(self$`currency`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`aggregate_usage`)) {
        sprintf(
        '"aggregate_usage":
        %s
        ',
        jsonlite::toJSON(self$`aggregate_usage`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`interval`)) {
        sprintf(
        '"interval":
        %s
        ',
        jsonlite::toJSON(self$`interval`$toJSON(), auto_unbox=TRUE, digits = NA)
        )},
        if (!is.null(self$`divided_by`)) {
        sprintf(
        '"divided_by":
          %d
                ',
        self$`divided_by`
        )},
        if (!is.null(self$`charge`)) {
        sprintf(
        '"charge":
          %d
                ',
        self$`charge`
        )},
        if (!is.null(self$`activated`)) {
        sprintf(
        '"activated":
          "%s"
                ',
        self$`activated`
        )}
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste('{', jsoncontent, '}', sep = "")
    },
    fromJSONString = function(PricingJson) {
      PricingObject <- jsonlite::fromJSON(PricingJson)
      self$`id` <- PricingObject$`id`
      self$`array_uuid` <- PricingObject$`array_uuid`
      self$`pricing_name` <- PricingObject$`pricing_name`
      self$`pricing_type` <- PricingType$new()$fromJSON(jsonlite::toJSON(PricingObject$pricing_type, auto_unbox = TRUE, digits = NA))
      self$`product_name` <- PricingObject$`product_name`
      self$`product_statement_descriptor` <- PricingObject$`product_statement_descriptor`
      self$`product_unit_label` <- PricingUnitLabel$new()$fromJSON(jsonlite::toJSON(PricingObject$product_unit_label, auto_unbox = TRUE, digits = NA))
      self$`currency` <- PricingCurrency$new()$fromJSON(jsonlite::toJSON(PricingObject$currency, auto_unbox = TRUE, digits = NA))
      self$`aggregate_usage` <- PricingAggregateUsage$new()$fromJSON(jsonlite::toJSON(PricingObject$aggregate_usage, auto_unbox = TRUE, digits = NA))
      self$`interval` <- PricingInterval$new()$fromJSON(jsonlite::toJSON(PricingObject$interval, auto_unbox = TRUE, digits = NA))
      self$`divided_by` <- PricingObject$`divided_by`
      self$`charge` <- PricingObject$`charge`
      self$`activated` <- PricingObject$`activated`
      self
    }
  )
)

