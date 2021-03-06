% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bamboo_meta_tables.R
\name{get_bamboo_meta_tables}
\alias{get_bamboo_meta_tables}
\title{Bamboo API get request wrapper}
\usage{
get_bamboo_meta_tables(user = NULL, password = NULL, verbose = FALSE)
}
\arguments{
\item{user}{Bamboo api user id, register in Bamboo "API Keys"}

\item{password}{Bamboo login password}

\item{verbose}{a logical; indicates if detailed output from httr calls should be provided; default FALSE}
}
\value{
tbl_df
}
\description{
Submits a get request to retrieve the meta data for all tables defined in the system
}
\examples{

user <- 'your_api_user'
password <- 'your_password'
verbose <- 'your_preference'

}
\references{
\url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
}
\author{
Evan Downey, \email{edowney@propellerpdx.com}
}
