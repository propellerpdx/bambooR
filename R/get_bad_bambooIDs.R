#' Bamboo API get request wrapper
#'
#' Consolidated list of test or incomplete Bamboo IDs
#'
#' @return tbl_df
#'
#' @examples
#'
#' bamboo_ids <- get_bad_bambooIDs()
#'
#' @author Evan Downey, \email{edowney@propellerpdx.com}
#' @references \url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
#'
#' @export
get_bad_bambooIDs <- function(){
  return(c(6,59,64,73,109))
}
