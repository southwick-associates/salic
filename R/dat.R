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

# License Data Tables ---------------------------------------------------

#' Sample Data: 3,000 Customers
#'
#' @docType data
#' @keywords datasets
#' @name cust
#' @format A data frame with 3,000 rows and 4 variables
#' \describe{
#' \item{cust_id}{Customer ID (integer) (primary key)}
#' \item{sex}{Gender (integer) (1=Male, 2=Female)}
#' \item{birth_year}{Year of Birth (integer) (yyyy)}
#' \item{cust_res}{State of Residence based on Address (integer) (1=Res, 0=Nonres)}
#' }
#' @family Sample Data
NULL

#' Sample Data: License Types
#'
#' @docType data
#' @keywords datasets
#' @name lic
#' @format A data frame with 105 rows and 4 variables
#' \describe{
#' \item{lic_id}{License ID (integer) (primary key)}
#' \item{type}{License Type (character) (hunt, fish, combo)}
#' \item{duration}{Duration in years (integer) (1=1yr/short-term, 2=2yr,...,99=lifetime)}
#' \item{lic_res}{License Residency (integer) (1=Res, 0=Nonres, NA=Unspecified)}
#' }
#' @family Sample Data
NULL

#' Sample Data: Sales (2008 through 2019)
#'
#' @docType data
#' @keywords datasets
#' @name sale
#' @format A data frame with 14,790 rows and 5 variables
#' \describe{
#' \item{cust_id}{Customer ID (integer) (foreign key)}
#' \item{lic_id}{License ID (integer) (foreign key)}
#' \item{year}{License Year (integer) (yyyy)}
#' \item{dot}{Transaction Purchase Date (character) (yyyy-mm-dd)}
#' \item{res}{Residency (integer) (1=Res, 0=Nonres)}
#' }
#' @family Sample Data
NULL
