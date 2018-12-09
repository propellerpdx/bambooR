#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the all employees, both terminated and active and a number of key data points from the employee table
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
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
#' @import purrr
#' @import dplyr
#' @importFrom lubridate ymd
#' @importFrom lubridate ymd_hms
#' @import stringr
#' @import jsonlite
#' @export
get_employees <- function(user=NULL,password=NULL){
  employees <- httr::GET('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/meta/users/',
                         httr::add_headers(Accept = "application/json"),
                         httr::authenticate(user=paste0(user), password=paste0(password))) %>%
    httr::content(.,as='text',type='json',encoding='UTF-8') %>%
    jsonlite::fromJSON(.,simplifyDataFrame=F) %>%
    purrr::map_df(., `[`, c('employeeId')) %>%
    dplyr::bind_rows()
  df <- employees$employeeId %>%
    purrr::map(., function(x) paste0('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/',x,'?fields=firstName,lastName,location,department,hireDate,jobTitle,Supervisor,supervisorEId,workEmail,customProratedUtilizationHours,customUtilizationGoalof1900hours,employmentHistoryStatus,terminationDate,lastChanged')) %>%
    purrr::map(., function(x) httr::GET(x,
                                        httr::add_headers(Accept = "application/json"),
                                        httr::authenticate(user=paste0(user), password=paste0(password)))) %>%
    purrr::map(.,function(x) httr::content(x,as='text',type='json',encoding='UTF-8')) %>%
    purrr::map(., function(x) stringr::str_replace_all(x,'0000-00-00','')) %>%
    purrr::map(., function(x) stringr::str_replace_all(x,'1900-01-01','')) %>%
    purrr::map(., function(x) stringr::str_replace_all(x,'null',paste0('\"\"'))) %>%
    purrr::map(.,function(x) jsonlite::fromJSON(x,simplifyDataFrame=T)) %>%
    dplyr::bind_rows()
  df <- df %>%
    dplyr::mutate(Name = paste0(firstName,' ',lastName)) %>%
    dplyr::mutate_at(dplyr::vars(colnames(df)[stringr::str_detect(names(df),'Date')]),dplyr::funs(lubridate::ymd(.))) %>%
    dplyr::mutate_at(dplyr::vars('customProratedUtilizationHours','customUtilizationGoalof1900hours'),dplyr::funs(as.numeric(.))) %>%
    dplyr::mutate_at(dplyr::vars('id','supervisorEId'),dplyr::funs(as.integer(.)))
  df$lastChanged <- lubridate::ymd_hms(df$lastChanged)
  return(df)
}
