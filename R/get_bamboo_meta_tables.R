#' Bamboo API get request wrapper
#'
#' Submits a get request to retrieve the meta data for all tables defined in the system
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
#' verbose <- 'your_preference'
#'
#' @author Evan Downey, \email{edowney@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @import httr
#' @importFrom magrittr %>%
#' @import dplyr
#' @import tibble
#' @import purrr
#' @importFrom jsonlite fromJSON
#' @export
get_bamboo_meta_tables <- function(user = NULL, password = NULL, verbose = FALSE)
{
  t <-
    httr::GET('https://api.bamboohr.com/api/gateway.php/propellerpdx/v1/meta/tables/',
              httr::add_headers(Accept = "application/json"),
              httr::authenticate(user = paste0(user), password = paste0(password)),
              config = httr::config(verbose = verbose)) %>%
    httr::content(., as='text', type='json', encoding='UTF-8') %>%
    jsonlite::fromJSON(., simplifyDataFrame=FALSE)

  t %>%
    magrittr::set_names(purrr::map_chr(t, names(t[[1]])[1])) %>%
    tibble::enframe(paste("table_",names(t[[1]])[1],sep=""),"data") %>%
    purrr::pmap(~ {
      field_Data <-
        ..2 %>%
        purrr::pluck(2)

      data.frame(
        table_alias = ..1,
        field_Data %>%
          purrr::map_dfr(., magrittr::extract, names(field_Data[[1]])),
        stringsAsFactors = FALSE
      ) %>% tidyr::nest(-table_alias)
    }) %>% dplyr::bind_rows()
}


