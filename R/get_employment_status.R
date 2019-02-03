#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the the employment status history for all employees
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
#' employment_status <- get_employment_status(user=user,password=password)
#'
#' @author Evan Downey, \email{edowney@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @import httr
#' @importFrom magrittr %>%
#' @import purrr
#' @import dplyr
#' @importFrom lubridate ymd
#' @importFrom lubridate ymd_hms
#' @import stringr
#' @importFrom jsonlite fromJSON
#' @export
get_employment_status <- function(user=NULL,password=NULL,verbose=FALSE) {
  df <- httr::GET("https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/all/tables/employmentStatus",
                  httr::add_headers("Accept" = "application/json"),
                  httr::authenticate(user=paste0(user), password=paste0(password))) %>%
    httr::content(., as="text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    dplyr::mutate(employmentStatus = replace(employmentStatus, employmentStatus == "Left Propeller", "terminated"),
                  id=as.integer(employeeId),
                  date=as.Date(date)) %>%
    dplyr::filter(!(id %in% get_bad_bambooIDs()))
  return(df)
}
