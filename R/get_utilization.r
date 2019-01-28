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
get_utilization <- function(user=NULL,password=NULL, employee_id=NULL,year=NULL){
  # Get all employees by default if targets for a particular employee weren't requested
  if(is.null(employee_id)){
    employees <- bambooR::get_employees(user=user,
                                        password=password)
    Employee_bambooID <- employees$Employee_bambooID
  }

  df <- Employee_bambooID %>%
    purrr::map(., function(x) paste0('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/',x,'/tables/customUtilization/')) %>%
    purrr::map(., function(x) httr::GET(x,
                                        httr::add_headers(Accept = "application/json"),
                                        httr::authenticate(user=paste0(user), password=paste0(password)))) %>%
    purrr::map(., function(x) httr::content(x,as='text',type='json',encoding='UTF-8')) %>%
    purrr::map(., function(x) jsonlite::fromJSON(x,simplifyDataFrame=T)) %>%
    purrr::flatten() %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Year=as.numeric(customYear),
                  primaryUtilization_Target=as.numeric(customPrimaryUtilizationTarget),
                  secondaryUtilization_Target=as.numeric(customSecondaryUtilizationTarget),
                  primaryUtilization_Waved=as.numeric(customPrimaryUtilizationWaved),
                  secondaryUtilization_Waved=as.numeric(customSecondaryUtilizationWaved),
                  primaryUtilizaiton_Proration=as.numeric(stringr::str_replace(customPrimaryUtilizationProration,'%',''))) %>%
    dplyr::rename('Bamboo_utilizationID'='id','Employee_bambooID'='employeeId')

  df <- dplyr::left_join(df,employees %>% dplyr::select(Employee_bambooID,Employee_hireDate)) %>%
    dplyr::select(Bamboo_utilizationID,Employee_bambooID,Employee_hireDate,primaryUtilizaiton_Proration,Year,primaryUtilization_Target,secondaryUtilization_Target,primaryUtilization_Waved,secondaryUtilization_Waved)

  if(is.null(year)==F){
    df <- df %>%
      dplyr::filter(Year==as.numeric(year))
  }

  return(df)
}

