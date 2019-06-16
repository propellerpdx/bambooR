#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the all employees, both terminated and active and a number of key data points from the employee table
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @param verbose a logical; indicates if detailed output from httr calls should be provided; default FALSE
#' @return tbl_df
#'
#' @examples
#'
#' user <- 'your_api_user'
#' password <- 'your_password'
#' employees <- get_employees(user=user,password=password)
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @import httr
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' @export
get_bamboo_meta_fields <- function(user = NULL, password = NULL, verbose = FALSE)
{
  httr::GET('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/meta/fields/',
            httr::add_headers(Accept = "application/json"),
            httr::authenticate(user = paste0(user), password = paste0(password)),
            config = httr::config(verbose = verbose)) %>%
    httr::content(., as='text', type='json', encoding='UTF-8') %>%
    jsonlite::fromJSON(., simplifyDataFrame=T) %>%
    dplyr::bind_rows(., built_in_bamboo_fields()) %>%
    dplyr::mutate(coerce_type = dplyr::case_when(type == "date" ~ "as.Date",
                                                 type == "integer" ~ "as.integer",
                                                 type == "currency" ~ "as.numeric",
                                                 type == "percentage" ~ "as.numeric",
                                                 type == "bool" ~ "as.logical",
                                                 type == "timestamp" ~ "as.POSIXct",
                                                 T ~ "as.character"))
}


#' @title Aliases and Data Types for built in BambooHR fields
#'
#' @description Return the aliases and data types for BambooHR fields that are not returned
#'   in the meta data API
#'
#' @details BambooHR has a meta data API which lists all discoverable fields along with their types.
#'   There are additional fields, however, outlined in the
#'   \href{https://www.bamboohr.com/api/documentation/employees.php}{BambooHR API documentation}
#'   which are not listed in the meta data API. This function returns those bult in fields.
#'#'
#' @param NA
#'
#' @return a data frame; containing types and aliases
#'
built_in_bamboo_fields <- function()
{
  dplyr::bind_rows(
    data.frame(type = "integer", alias = "id", stringsAsFactors = FALSE),
    c(type='integer',alias='age'),
    c(type='email',alias='bestEmail'),
    c(type='text',alias='birthday'),
    c(type='list',alias='flsaCode'),
    c(type='text',alias='fullName1'),
    c(type='text',alias='fullName2'),
    c(type='text',alias='fullName3'),
    c(type='text',alias='fullName4'),
    c(type='text',alias='fullName5'),
    c(type='text',alias='displayName'),
    c(type='timestamp',alias='lastChanged'),
    c(type='list',alias='payGroup'),
    c(type='integer',alias='payGroupId'),
    c(type='paid_per',alias='paidPer'),
    c(type='integer',alias='payScheduleId'),
    c(type='list',alias='payFrequency'),
    c(type='bool',alias='includeInPayroll'),
    c(type='bool',alias='timeTrackingEnabled'),
    c(type='sin',alias='sin'),
    c(type='text',alias='stateCode'),
    c(type='employee',alias='supervisor'),
    c(type='integer',alias='supervisorId'),
    c(type='integer',alias='supervisorEId'),
    c(type='text',alias='workPhonePlusExtension'),
    c(type='bool',alias='isPhotoUploaded'),
    c(type='integer',alias='standardHoursPerWeek'),
    c(type='date',alias='bonusDate'),
    c(type='currency',alias='bonusAmount'),
    c(type='list',alias='bonusReason'),
    c(type='text',alias='bonusComment'),
    c(type='date',alias='commissionDate'),
    c(type='date',alias='commisionDate'),
    c(type='currency',alias='commissionAmount'),
    c(type='text',alias='commissionComment'),
    c(type='bool',alias='photoUploaded'),
    c(type='nin',alias='nin'),
    c(type='national_id',alias='nationalId'),
    c(type='list',alias='nationality'),
  )
}
