#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the custom bench table for all employees
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @return tbl_df
#'
#' @examples
#'
#' user <- 'your_api_user'
#' password <- 'your_password'
#' bench <- get_bench(user=user,password=password)
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
get_bench <- function(user=NULL,password=NULL){
  employees <- httr::GET('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/meta/users/',
                         httr::add_headers(Accept = "application/json"),
                         httr::authenticate(user=paste0(user), password=paste0(password))) %>%
    httr::content(.,as='text',type='json',encoding='UTF-8') %>%
    jsonlite::fromJSON(.,simplifyDataFrame=F) %>%
    purrr::map_df(., `[`, c('employeeId')) %>%
    dplyr::bind_rows()
  df <- employees$employeeId %>%
    purrr::map(., function(x) paste0('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/',x,'/tables/customBenchTime')) %>%
    purrr::map(., function(x) httr::GET(x,
                                      httr::add_headers(Accept = "application/json"),
                                      httr::authenticate(user=paste0(user), password=paste0(password)))) %>%
    purrr::map(.,function(x) httr::content(x,as='text',type='json',encoding='UTF-8')) %>%
    purrr::map(.,function(x) jsonlite::fromJSON(x,simplifyDataFrame=T)) %>%
    purrr::flatten_df() %>%
    dplyr::select(-id) %>%
    dplyr::mutate_at(dplyr::vars(colnames(df)[stringr::str_detect(names(df),'date')]),dplyr::funs(lubridate::ymd(.))) %>%
    dplyr::mutate_at(dplyr::vars(c('customHours')),dplyr::funs(as.numeric(.)))
}
