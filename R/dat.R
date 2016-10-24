# Documentation for Sample Data

#' Sample Data: GA Hunting Customers
#'
#' A data frame with a random sample of 1000 GA hunting customers
#'
#' @docType data
#' @keywords datasets
#' @name cust
#' @format A data frame with 1000 rows and 4 variables
#' \describe{
#' \item{cust_id}{Customer ID}
#' \item{dob}{Date of Birth}
#' \item{state}{State of Residence}
#' \item{sex}{Gender}
#' }
#' @family Sample Data
NULL

#' Sample Data: GA Hunting Licenses
#'
#' A data frame describing hunting licenses
#'
#' @docType data
#' @keywords datasets
#' @name lic
#' @format A data frame with 32 rows and 4 variables
#' \describe{
#' \item{lic_id}{License ID}
#' \item{description}{License Description}
#' \item{priv}{Privilege Type}
#' \item{duration}{Duration (years, minimum of 1) for License
#' (where duration == 99 corresponds to a lifetime license)}
#' }
#' @family Sample Data
NULL

#' Sample Data: GA Hunting Sales
#'
#' A data frame with GA Hunting sales corresponding to the 1000 customer sample
#'
#' @docType data
#' @keywords datasets
#' @name sale
#' @format A data frame with 2802 rows and 4 variables
#' \describe{
#' \item{cust_id}{Customer ID}
#' \item{lic_id}{License ID}
#' \item{dot}{Date of Transaction}
#' \item{year}{License Year}
#' }
#' @family Sample Data
NULL

#' Sample Data: GA Hunting purchase History
#'
#' A list of data frames corresponding to the 1000 customer sample
#'
#' @docType data
#' @keywords datasets
#' @name track
#' @format A list of data frames, each with 1000 rows and 3 variables
#' \describe{
#' \item{cust_id}{Customer ID}
#' \item{left}{Time remaining on previous license (==0: need to renew,
#' >0: don't need to renew, ==NA: haven't bought before, <0 didn't buy last year)}
#' \item{duration}{Bought a license this year? (>=1: yes, ==0: no)}
#' }
#' @family Sample Data
NULL
