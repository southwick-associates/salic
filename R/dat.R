# Documentation for Sample Data


#' Data: State/Province/Territory Abbreviations for US & Canada
#'
#' A data frame with 72 abbreviations, used in the \code{\link{recode_state}} function
#'
#' @docType data
#' @keywords datasets
#' @name state_abbreviations
#' @format A data frame with 72 rows and 3 variables
#' \describe{
#' \item{name}{State/Province/Territory name}
#' \item{state}{Abbreviation}
#' \item{country}{Country Abbreviation where CN = Canada}
#' }
#' @family Sample Data
NULL


# 3 License Data Tables ---------------------------------------------------


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
