% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_utilization.r
\name{get_utilization}
\alias{get_utilization}
\title{Bamboo API get request wrapper}
\usage{
get_utilization(user = NULL, password = NULL,
  employee_ids = c("all"), employees = NULL, year = NULL,
  verbose = FALSE)
}
\arguments{
\item{user}{Bamboo api user id, register in Bamboo "API Keys"}

\item{password}{Bamboo login password}

\item{employee_ids}{an optional list; specifies the employees for which bench time is requested; defaults to c('all') which gets all employee bench time}

\item{year}{a calendar year; restricts the result set to a particular year if provided; default NULL}

\item{verbose}{a logical; indicates if detailed output from httr calls should be provided; default FALSE}
}
\value{
tbl_df
}
\description{
Submits a get request to retrieve the all employees utilization targets
}
\examples{

user <- 'your_api_user'
password <- 'your_password'
employees <- get_utilization(user=user,password=password,employee_id=c(1,2,3,4))

}
\references{
\url{https://www.bamboohr.com/api/documentation/},  \url{https://github.com/r-lib/httr}
}
\author{
Mark Druffel, \email{mdruffel@propellerpdx.com}
}
