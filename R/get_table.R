#' Bamboo API get request wrapper for BambooHR tables
#'
#' Submits a get request to retrieve a specified table from the system, valid tables are retreived from the
#' meta data API
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @param verbose a logical; indicates if detailed output from httr calls should be provided; default FALSE
#' @param table_alias the a alis of the table name requested, must be valid from the meta data API
#' @return tbl_df
#'
#' @examples
#'
#' user <- 'your_api_user'
#' password <- 'your_password'
#' verbose <- 'your_preference'
#' table_alias <- 'table_alias'
#' table_data <- get_table(table_alias=table_alias, user=user, password=password, verbose=verbose)
#'
#' @author Evan Downey, \email{edowney@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @import httr
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr
#' @importFrom jsonlite fromJSON
#' @export
get_table <- function(requested_table_alias = NULL, user = NULL, password = NULL, verbose = FALSE)
{
  #get the meta data for the requested table from the BambooHR API
  table_information <-
    get_bamboo_meta_tables(user=user, password=password, verbose=verbose) %>%
    dplyr::filter(table_alias == requested_table_alias) %>%
    dplyr::select(data) %>%
    tidyr::unnest()

  #if the requested_table_alias is not found in the meta data information, return an error
  if(length(table_information) == 0)
  {
    stop(paste0("the field \"",requested_table_alias,"\" is not in the meta data API.", sep = ""))
  }

  #get data for the requested table
  table_data <-
    httr::GET(paste("https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/all/tables/", requested_table_alias, "?format=JSON", sep=""),
              httr::authenticate(user=paste0(user), password=paste0(password)),
              config = httr::config(verbose = verbose),
              httr::add_headers("Accept" = "application/json")) %>%
    httr::content(., as="text", encoding = "UTF-8")

  #if an empty data set is returned, then return NA
  if(table_data %in% c("[]",""))
  {
    table_data <- NA
  }
  else
  {
    #otherwise parse out the JSON
    table_data <-
      table_data %>%
      jsonlite::fromJSON()

    #if it's not a data frame yet, bind the lists returned into one
    if(!is.data.frame(table_data))
    {
      table_data <-
        table_data %>%
        dplyr::bind_rows()
    }

    #get a list of fields that may have nested data frames
    data_frame_fields <-
      table_data %>%
      dplyr::select_if(is.data.frame) %>%
      names()

    #if there are nested data frames, pull out the value field of them and set it equal to the variable name
    #for example in the compensation table, the rate field is a data frame containing two fields value and currency, this code sets rate = rate$value and drops currency
    if(length(data_frame_fields) > 0)
    {
      table_data <-
        table_data %>%
        purrr::modify_at(dplyr::vars(as.character(data_frame_fields)), ~.$value)
    }

    #for some reason the BambooHR meta data API does not include id (unique primary key for the table) or employeeId (unique employee number) in the return information for some tables.
    # this code tells the type conversion code later to convert them both to integers if they exist in the return data
    if("id" %in% names(table_data))
    {
      table_information <-
        table_information %>%
        dplyr::bind_rows(.,
                         c(alias="id",type="int"))
    }
    if("employeeId" %in% names(table_data))
    {
      table_information <-
        table_information %>%
        dplyr::bind_rows(.,
                         c(alias="employeeId",type="int"))
    }

    #sometimes the BambooHR meta data API tells us to expect fields that don't come back in the return data. This code cleans that up.
    table_information <-
      table_information %>%
      dplyr::filter(alias %in% names(table_data))
    
    #for some reason certain tables are now coming as list values, if they are then unnest them
    if("list" %in% sapply(table_data, class)) {
      table_data <-
        table_data %>% 
        tidyr::unnest(cols = names(table_data))
    }
    
    str(table_data)
  
    #finally coerce the return values to R data types, cleaning up weird BambooHR default choices (like null dates are set to 000-00-00)
#    table_data %>%
#      purrr::modify_at(dplyr::vars(as.character(table_information[table_information$type == "date",]$alias)),~ ifelse(.=="0000-00-00" | .=="NULL",NA,.)) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "date",]$alias)), as.Date) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "int",]$alias)), as.integer) %>%
#      purrr::modify_at(dplyr::vars(as.character(table_information[table_information$type == "list",]$alias)), ~ ifelse(.=="",NA,.)) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "list",]$alias)), as.factor) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "currency",]$alias)), ~ sub("USD", "", .)) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "currency",]$alias)), as.numeric) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "percentage",]$alias)), ~ sub("%", "", .)) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "percentage",]$alias)), ~ as.numeric(.)/100) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "timestamp",]$alias)), ~ paste(substr(.,0,10),substr(.,12,19))) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "timestamp",]$alias)), as.POSIXct) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "text",]$alias)), as.character) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "textarea",]$alias)), as.character) %>%
#      dplyr::mutate_at(dplyr::vars(as.character(table_information[table_information$type == "bool",]$alias)), as.logical)
  }
}
