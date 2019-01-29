#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the all employees time off by date
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
get_pto <- function(user=NULL,password=NULL,verbose=FALSE){
  time_off <- httr::GET(paste0('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/time_off/requests/?status=approved&start=',lubridate::floor_date(lubridate::today(),'year'),'&end=',lubridate::ceiling_date(lubridate::today(),'year')-1),
                        httr::add_headers(Accept = "application/json"),
                        httr::authenticate(user=paste0(user), password=paste0(password)),
                        config=config(verbose=verbose)) %>%
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
  time_off <- dplyr::inner_join(time_off,time_off_days) %>%
    dplyr::rename('PTO_ID'='id','Spent_Date'='Date','PTO_updatedDate'='lastChanged','Employee_bambooID'='employeeId','PTO_Status'='status','PTO_Type'='name','PTO_timesUnit'='unit','PTO_Time'='amount') %>%
    dplyr::select(PTO_ID,Employee_bambooID,Spent_Date,PTO_updatedDate,PTO_Status,PTO_Type,PTO_timesUnit,PTO_Time,Hours) %>%
    dplyr::mutate_at(dplyr::vars('PTO_ID','Employee_bambooID'),dplyr::funs(as.integer(.))) %>%
    dplyr::mutate_at(dplyr::vars(colnames(df)[stringr::str_detect(names(df),'Date')]),dplyr::funs(lubridate::ymd(.))) %>%
    dplyr::mutate_at(dplyr::vars('PTO_Time','Hours'),dplyr::funs(as.numeric(.)))
  return(time_off)
}

