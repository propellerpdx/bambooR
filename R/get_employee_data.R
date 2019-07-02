#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the given fields for the given employees
#'
#' @param ids data frame or list of Bamboo Employee IDs
#' @param fields Bamboo fields to be requested, can be requested by id or alias outlined in get_bamboo_meta_fields
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @param verbose a logical; indicates if detailed output from httr calls should be provided; default FALSE
#' @return tbl_df
#'
#' @examples
#'
#' ids <- data.frame(id=c(1,6,16,24,44))
#' fields <- c("firstName","hireDate","location","terminationDate","lastChanged")
#' user <- 'your_api_user'
#' password <- 'your_password'
#' employee_data <- get_employee_data(ids=ids, fields = fields, user=user, password=password)
#'
#' @author Evan Downey, \email{edowney@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @import httr
#' @importFrom magrittr %>%
#' @import purrr
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' @export
get_employee_data <- function(ids = NULL, fields=c("firstName","lastName","jobTitle","terminationDate","hireDate","location"), user = NULL, password = NULL, verbose = FALSE)
{
  ##get the meta data for the requested fields
  bamboo_meta_field_list <- get_bamboo_meta_fields(user = paste0(user), password = paste0(password), verbose = verbose)

  fields_meta_data <-
    bamboo_meta_field_list %>%
    dplyr::filter(id %in% fields |
                    alias %in% fields | alias == "id")

  raw_data <-
    ids %>%
    purrr::pmap(~ {
      ## Make the request to BambooHR for the fields requested for this specific id
      data_returned <-
        httr::GET(paste("https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/",..1,"?fields=",paste(fields_meta_data$alias %>% as.list(),collapse = ","),sep=""),
                  httr::authenticate(user=paste0(user), password=paste0(password)),
                  config=httr::config(verbose=verbose),
                  httr::add_headers("Accept" = "application/json")) %>%
        httr::content(., as="text", encoding = "UTF-8") %>%
        jsonlite::fromJSON()

      ## BambooHR sometimes doesn't return requested fields (not sure why, but we have a bug submitted)
      fields_returned <-
        fields_meta_data$alias[tibble::has_name(data_returned, fields_meta_data$alias)]

      ## set null fields to NA
      data_returned %>%
        purrr::modify_at(dplyr::vars(as.character(fields_returned)),~ ifelse(is.null(.),NA,.))
    }) %>%
    dplyr::bind_rows()

  ## Get the types from the meta data API for the requested fields
  fields_meta_data  <-
    bamboo_meta_field_list %>%
    dplyr::filter(alias %in% names(raw_data))

  ## Coerce the data types and return the result
  raw_data %>%
    purrr::modify_at(dplyr::vars(as.character(fields_meta_data[fields_meta_data$coerce_type == "as.Date",]$alias)),~ ifelse(.=="0000-00-00",NA,.)) %>%
    dplyr::mutate_at(dplyr::vars(as.character(fields_meta_data[fields_meta_data$coerce_type == "as.Date",]$alias)), as.Date) %>%
    dplyr::mutate_at(dplyr::vars(as.character(fields_meta_data[fields_meta_data$coerce_type == "as.integer",]$alias)), as.integer) %>%
    dplyr::mutate_at(dplyr::vars(as.character(fields_meta_data[fields_meta_data$coerce_type == "as.numeric",]$alias)), as.numeric) %>%
    dplyr::mutate_at(dplyr::vars(as.character(fields_meta_data[fields_meta_data$coerce_type == "as.POSIXct",]$alias)), ~ paste(substr(.,0,10),substr(.,12,19))) %>%
    dplyr::mutate_at(dplyr::vars(as.character(fields_meta_data[fields_meta_data$coerce_type == "as.POSIXct",]$alias)), as.POSIXct) %>%
    dplyr::mutate_at(dplyr::vars(as.character(fields_meta_data[fields_meta_data$coerce_type == "as.character",]$alias)), as.character)
}
