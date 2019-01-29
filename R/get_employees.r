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
#' @import purrr
#' @import dplyr
#' @importFrom lubridate ymd
#' @importFrom lubridate ymd_hms
#' @import stringr
#' @importFrom jsonlite fromJSON
#' @export
get_employees <- function(user=NULL,password=NULL,verbose=FALSE){
  employees <- httr::GET('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/meta/users/',
                         httr::add_headers(Accept = "application/json"),
                         httr::authenticate(user=paste0(user), password=paste0(password)),
                         config=config(verbose=verbose)) %>%
    httr::content(.,as='text',type='json',encoding='UTF-8') %>%
    jsonlite::fromJSON(.,simplifyDataFrame=F) %>%
    purrr::map_df(., `[`, c('employeeId')) %>%
    dplyr::bind_rows()
  df <- employees$employeeId %>%
    purrr::map(., function(x) paste0('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/',x,'?fields=customHarvestID,firstName,lastName,location,department,hireDate,jobTitle,Supervisor,supervisorEId,workEmail,customProratedUtilizationHours,customUtilizationGoalof1900hours,employmentHistoryStatus,terminationDate,lastChanged')) %>%
    purrr::map(., function(x) httr::GET(x,
                                        httr::add_headers(Accept = "application/json"),
                                        httr::authenticate(user=paste0(user), password=paste0(password)),
                                        config=config(verbose=verbose))) %>%
    purrr::map(.,function(x) httr::content(x,as='text',type='json',encoding='UTF-8')) %>%
    purrr::map(., function(x) stringr::str_replace_all(x,'0000-00-00','')) %>%
    purrr::map(., function(x) stringr::str_replace_all(x,'1900-01-01','')) %>%
    purrr::map(., function(x) stringr::str_replace_all(x,'null',paste0('\"\"'))) %>%
    purrr::map(.,function(x) jsonlite::fromJSON(x,simplifyDataFrame=T)) %>%
    dplyr::bind_rows()
  df <- df %>%
    dplyr::mutate(Employee_Name = paste0(firstName,' ',lastName)) %>%
    dplyr::mutate(lastChanged = lubridate::ymd_hms(lastChanged)) %>%
    dplyr::mutate_at(dplyr::vars(colnames(df)[stringr::str_detect(names(df),'Date')]),dplyr::funs(lubridate::ymd(.))) %>%
    dplyr::mutate_at(dplyr::vars('customProratedUtilizationHours','customUtilizationGoalof1900hours'),dplyr::funs(as.numeric(.))) %>%
    dplyr::mutate_at(dplyr::vars('id','supervisorEId','customHarvestID'),dplyr::funs(as.integer(.))) %>%
    dplyr::filter(terminationDate >= lubridate::floor_date(lubridate::today(),unit='year') | is.na(terminationDate)) %>%
    dplyr::filter(id != '73') %>%
    dplyr::rename('Employee_bambooID'='id',
                  'Employee_harvestID'='customHarvestID',
                  'Employee_Location'='location',
                  'Employee_Department'='department',
                  'Employee_hireDate' = 'hireDate',
                  'Employee_jobTitle' = 'jobTitle',
                  'Employee_Manager' = 'Supervisor',
                  'Employee_managerID'='supervisorEId',
                  'Employee_Email' = 'workEmail',
                  'Employee_terminationDate'='terminationDate',
                  'Employee_updatedDate'='lastChanged') %>%
    dplyr::select(Employee_Name,Employee_bambooID,Employee_harvestID,Employee_hireDate,Employee_terminationDate,Employee_updatedDate,Employee_Location,Employee_Department,Employee_jobTitle,Employee_Email,Employee_Manager,Employee_managerID)
  return(df)
}
