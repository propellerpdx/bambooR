#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the all employees time off by date
#'
#' @param user Bamboo api user id, register in Bamboo "API Keys"
#' @param password Bamboo login password
#' @param startDate date; start date of the range for which PTO records are desired; *required*; defaults to NULL
#' @param endDate date; end date of the range for which PTO records are desired; *required*; defaults to NULL
#' @param year int; year for which PTO records are desired; defaults to current year
#' @param verbose logical; indicates if detailed output from httr calls should be provided; default FALSE
#' @return tbl_df
#'
#' @examples
#' \dontrun{
#' user <- 'your_api_user'
#' password <- 'your_password'
+#' startDate = lubridate::ymd("20190101")
+#' endDate = lubridate::ymd("20191231")
+#' employees <- get_pto(user = user, password = password, startDate = startDate, endDate = endDate)
#'}
#'
#' @author Mark Druffel, \email{mdruffel@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @export
get_pto <- function(user=NULL,
                    password=NULL,
                    startDate = NULL,
                    endDate = NULL,
                    verbose=FALSE){
  if(is.null(startDate) || !lubridate::is.Date(startDate)) {
    stop("A startDate is required and must be a date!!")
  }
  if(is.null(endDate) || !lubridate::is.Date(endDate)) {
    stop("A endDate is required and must be a date!!")
  }

  time_off <-
    httr::GET(
      paste0(
        'https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/time_off/requests/?status=approved&start=',
        startDate,
        '&end=',
        endDate
      ),
      httr::add_headers(Accept = "application/json"),
      httr::authenticate(user = paste0(user), password = paste0(password)),
      config = config(verbose = verbose)
    ) %>%
    httr::content(.,as='text',type='json',encoding='UTF-8') %>%
    jsonlite::fromJSON(.,simplifyDataFrame=F)

  time_off_days <- dplyr::bind_cols(
    time_off %>%
      purrr::map(., 'dates') %>%
      purrr::map(., names) %>%
      purrr::map2_df(
        .x = .,
        .y = time_off %>% purrr::map(., `[`, c('id', 'employeeId')),
        function(x = .x, y = .y) unlist(x) %>%
          dplyr::as_data_frame(.) %>%
          dplyr::mutate(id = unlist(y)[1], employeeId = unlist(y)[2])) %>%
      dplyr::rename('Date' = 'value'),
    time_off %>%
      purrr::map(.,'dates') %>%
      purrr::map_df(., function(x) unlist(x) %>%
                        dplyr::as_data_frame(.)) %>%
                        dplyr::rename('Hours'='value')
    )

  time_off <-
    dplyr::bind_cols(
      time_off %>%
        purrr::map_df(., `[`, c('id', 'employeeId')),
      time_off %>%
        purrr::map(., 'status') %>%
        purrr::map_df(., `[`, c('lastChanged', 'status')),
      time_off %>%
        purrr::map(., 'type') %>%
        purrr::map_df(., `[`, c('id', 'name')),
      time_off %>%
        purrr::map(., 'amount') %>%
        purrr::map_df(., `[`, c('unit', 'amount'))
    )

  time_off <-
    dplyr::inner_join(time_off, time_off_days) %>%
    dplyr::rename(
      'PTO_ID' = 'id',
      'Spent_Date' = 'Date',
      'PTO_updatedDate' = 'lastChanged',
      'Employee_bambooID' = 'employeeId',
      'PTO_Status' = 'status',
      'PTO_Type' = 'name',
      'PTO_timesUnit' = 'unit',
      'PTO_Time' = 'amount'
    ) %>%
    dplyr::select(
      PTO_ID,
      Employee_bambooID,
      Spent_Date,
      PTO_updatedDate,
      PTO_Status,
      PTO_Type,
      PTO_timesUnit,
      PTO_Time,
      Hours
    ) %>%
    dplyr::mutate_at(dplyr::vars('PTO_ID', 'Employee_bambooID'),
                     dplyr::funs(as.integer(.))) %>%
    dplyr::mutate_at(dplyr::vars(colnames(df)[stringr::str_detect(names(df), 'Date')]),
                     dplyr::funs(lubridate::ymd(.))) %>%
    dplyr::mutate_at(dplyr::vars('PTO_Time', 'Hours'),
                     dplyr::funs(as.numeric(.)))

  # Throw out any "shoulder" days off that were part of a range that crossed the start & end dates
  time_off <- dplyr::filter(time_off,
                            .data$Spent_Date >= !!startDate,
                            .data$Spent_Date <= !!endDate)

  return(time_off)
}
