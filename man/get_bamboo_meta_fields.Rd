% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bamboo_meta_fields.R
\name{get_bamboo_meta_fields}
\alias{get_bamboo_meta_fields}
\title{Bamboo API get request wrapper}
\usage{
get_bamboo_meta_fields(user = NULL, password = NULL, verbose = FALSE)
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
Submits a get request to retrieve the all employees, both terminated and active and a number of key data points from the employee table
}
\examples{

user <- 'your_api_user'
password <- 'your_password'
employees <- get_employees(user=user,password=password)

}
\references{
\url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
}
\author{
Mark Druffel, \email{mdruffel@propellerpdx.com}
}
