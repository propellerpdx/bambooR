#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the all employees utilization targets
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @return tbl_df
#'
#' @examples
#'
#' user <- 'your_api_user'
#' password <- 'your_password'
#' employees <- get_utilization(user=user,password=password,employee_id=c(1,2,3,4))
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @import httr
#' @importFrom magrittr %>%
#' @import purrr
#' @import dplyr
#' @import jsonlite
#' @export
get_utilization <- function(user=NULL,password=NULL, employee_id=NULL){
  if(is.null(employee_id)){
    employee_id <- httr::GET('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/meta/users/',
                             httr::add_headers(Accept = "application/json"),
                             httr::authenticate(user=paste0(user), password=paste0(password))) %>%
      httr::content(.,as='text',type='json',encoding='UTF-8') %>%
      jsonlite::fromJSON(.,simplifyDataFrame=F) %>%
      purrr::map_df(., `[`, c('employeeId')) %>%
      dplyr::bind_rows() %>%
      .$employeeId
  }
  df <- employee_id %>%
    purrr::map(., function(x) paste0('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/',x,'/tables/customUtilization/')) %>%
    purrr::map(., function(x) httr::GET(x,
                                        httr::add_headers(Accept = "application/json"),
                                        httr::authenticate(user=paste0(user), password=paste0(password)))) %>%
    purrr::map(., function(x) httr::content(x,as='text',type='json',encoding='UTF-8')) %>%
    purrr::map(., function(x) jsonlite::fromJSON(x,simplifyDataFrame=T)) %>%
    purrr::flatten() %>%
    dplyr::bind_rows() %>%
    dplyr::select(dplyr::one_of(c('id','employeeId','customYear','customPrimaryUtilizationTarget','customSecondaryUtilizationTarget')))
  return(df)
}
