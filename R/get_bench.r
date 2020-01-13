#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the custom bench table for all employees
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @param employee_ids an optional list; specifies the employees for which bench
#'   time is requested; defaults to c('all') which gets all employee bench time
#' @param year integer; the year for which bench records are desired; optional,
#'   defaults to NULL
#' @param verbose a logical; indicates if detailed output from httr calls should
#'   be provided; default FALSE
#' @return tbl_df
#'
#' @examples
#' \dontrun{
#' user <- 'your_api_user'
#' password <- 'your_password'
#' bench <- get_bench(user=user,password=password)
#'}
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @export
get_bench <-
  function(user=NULL,
           password=NULL,
           employee_ids=c('all'),
           year=NULL,
           verbose=FALSE){
  df <-
    employee_ids %>%
    purrr::map(.,
               function(x)
                 paste0(
                   'https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/employees/',
                   x,
                   '/tables/customBenchTime'
                 )) %>%
    purrr::map(.,
               function(x)
                 httr::GET(
                   x,
                   httr::add_headers(Accept = "application/json"),
                   httr::authenticate(user = paste0(user), password =
                                        paste0(password)),
                   config = config(verbose = verbose)
                 )) %>%
    purrr::map(.,
               function(x)
                 httr::content(
                   x,
                   as = 'text',
                   type = 'json',
                   encoding = 'UTF-8'
                 )) %>%
    purrr::map(.,
               function(x)
                 jsonlite::fromJSON(x, simplifyDataFrame = T)) %>%
    purrr::flatten_df() %>%
    dplyr::select(-id) %>%
    dplyr::mutate_at(dplyr::vars(colnames(df)[stringr::str_detect(names(df), 'date')]),
                     dplyr::funs(lubridate::ymd(.))) %>%
    dplyr::mutate_at(dplyr::vars(c('customHours')),
                     dplyr::funs(as.numeric(.))) %>%
    dplyr::rename(
      'Employee_bambooID' = 'employeeId',
      'Bench_startDate' = 'customStartdate',
      'Bench_endDate' = 'customEnddate1',
      'Bench_hoursCap' = 'customHours'
    )

  # Filter to only include records that touch the requested year if a year was
  # specified
  # The only scenario the below doesn't cover is a situation where the bench
  # record covers the entire year, which seems unrealistic
  if(!is.null(year)){
    df <-
      dplyr::filter(
        df,
        lubridate::year(Bench_startDate) == year |
          lubridate::year(Bench_endDate) == year |
          (lubridate::year(Bench_startDate) <= year &
             is.na(Bench_endDate)))
  }

return(df)
}
