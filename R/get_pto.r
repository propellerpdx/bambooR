#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the all employees time off by date
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @return tbl_df
#'
#' @examples
#'
#' user <- 'your_api_user'
#' password <- 'your_password'
#' employees <- get_pto(user=user,password=password)
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
get_pto <- function(user=NULL,password=NULL){
  time_off <- httr::GET(paste0('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/time_off/requests/?status=approved&start=',lubridate::floor_date(lubridate::today(),'year'),'&end=',lubridate::ceiling_date(lubridate::today(),'year')-1),
                        httr::add_headers(Accept = "application/json"),
                        httr::authenticate(user=paste0(user), password=paste0(password))) %>%
    httr::content(.,as='text',type='json',encoding='UTF-8') %>%
    jsonlite::fromJSON(.,simplifyDataFrame=F)
  time_off_days <- dplyr::bind_cols(
    time_off %>%
      purrr::map(.,'dates') %>%
      purrr::map(.,names) %>%
      purrr::map2_df(.x=.,.y=time_off %>% purrr::map(., `[`, c('id','employeeId')), function(x=.x,y=.y) unlist(x) %>% dplyr::as_data_frame(.) %>% dplyr::mutate(id=unlist(y)[1],employeeId=unlist(y)[2])) %>% dplyr::rename('Date'='value'),
    time_off %>%
      purrr::map(.,'dates') %>%
      purrr::map_df(., function(x) unlist(x) %>% dplyr::as_data_frame(.)) %>% dplyr::rename('Hours'='value'))
  time_off <- dplyr::bind_cols(time_off %>%
                                 purrr::map_df(., `[`, c('id','employeeId')),
                               time_off %>%
                                 purrr::map(.,'status') %>%
                                 purrr::map_df(., `[`, c('lastChanged','status')),
                               time_off %>%
                                 purrr::map(.,'type') %>%
                                 purrr::map_df(., `[`, c('id','name')),
                               time_off %>%
                                 purrr::map(.,'amount') %>%
                                 purrr::map_df(., `[`, c('unit','amount')))
  check <- dplyr::inner_join(time_off,time_off_days) %>%
    dplyr::mutate(Spent_Date = lubridate::ymd(Date),
                  Updated_Date = lubridate::ymd(lastChanged)) %>%
    dplyr::rename('Bamboo_ID'='id','Employee_ID'='employeeId','Status'='status','PTO_Type'='name','Hours_Unit'='unit','Total_Amount'='amount') %>%
    dplyr::select(Bamboo_ID,Employee_ID,Spent_Date,Updated_Date,Status,PTO_Type,Total_Amount,Hours)  return(time_off)
}
