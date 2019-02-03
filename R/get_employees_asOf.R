#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the all employees as of a past date
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @param asOf Date to get employees
#' @param verbose a logical; indicates if detailed output from httr calls should be provided; default FALSE
#' @return tbl_df
#'
#' @examples
#'
#' user <- 'your_api_user'
#' password <- 'your_password'
#' asOf <- 'date'
#' employees <- get_employees_asOf(user=user,password=password,asOf=asOf)
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
get_employees_asOf <- function(user=NULL,password=NULL,asOf=Sys.Date(),verbose=FALSE){
  e <- get_employment_status(paste0(user), paste0(password)) %>%
    dplyr::filter(date<=asOf) %>%
    dplyr::group_by(id) %>% dplyr::filter(date==max(date)) %>% dplyr::ungroup(id) %>%
    dplyr::filter(employmentStatus %in% c("Full Time","Part-Time") | (asOf==date)) %>%
    dplyr::select(id)

  return(e)
}
