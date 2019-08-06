# Documentation for Sample Data


# Standarized License Data ---------------------------------------------------

#' Sample Data: 15,000 Customers
#'
#' @docType data
#' @keywords datasets
#' @name cust
#' @format A data frame with 3 variables
#' \describe{
#' \item{cust_id}{Customer ID (integer) (primary key)}
#' \item{sex}{Gender (integer) (1=Male, 2=Female)}
#' \item{birth_year}{Year of Birth (integer) (yyyy)}
#' }
#' @family Sample Data
NULL

#' Sample Data: License Types
#'
#' @docType data
#' @keywords datasets
#' @name lic
#' @format A data frame with 3 variables
#' \describe{
#' \item{lic_id}{License ID (integer) (primary key)}
#' \item{type}{License Type (character) (hunt, fish, combo)}
#' \item{duration}{Duration in years (integer) (1=1yr/short-term, 2=2yr,...,99=lifetime)}
#' }
#' @family Sample Data
NULL

#' Sample Data: Sales (Jan 1, 2008 through June 30, 2019)
#'
#' @docType data
#' @keywords datasets
#' @name sale
#' @format A data frame with 5 variables
#' \describe{
#' \item{cust_id}{Customer ID (integer) (foreign key)}
#' \item{lic_id}{License ID (integer) (foreign key)}
#' \item{year}{License Year (integer) (yyyy)}
#' \item{month}{Transaction Purchase Month (integer) (1=Jan, 2=Feb, ..., 12=Dec)}
#' \item{res}{Residency (integer) (1=Res, 0=Nonres)}
#' }
#' @family Sample Data
NULL


# Additional Data ---------------------------------------------------------

#' Sample Data: License  History (Jan 1, 2008 through June 30, 2019)
#' 
#' This is a table that corresponds to the "all_sports" permission for the included
#' license sample data; containing 1 row per customer per year a license was held.
#' Only the variables needed for dashboard production were included (i.e., certain 
#' variables potentially useful for validation, such as duration_run, were removed).
#'
#' @docType data
#' @keywords datasets
#' @name history
#' @format A data frame with 8 variables
#' \describe{
#' \item{cust_id}{Customer ID (integer) (composite key)}
#' \item{year}{License Year (integer) (yyyy) (composite key)}
#' \item{month}{Earliest Transaction Month (integer) (1=Jan, 2=Feb, ..., 12=Dec)}
#' \item{lapse}{Lapsed next Year? (integer) (1=lapse, 0=renew)}
#' \item{R3}{R3 Group this Year (integer) (1=carry, 2=renew, 3=reactivate, 4=recruit)}
#' \item{res}{Residency (integer) (1=Res, 0=Nonres)}
#' \item{sex}{Gender (integer) (1=Male, 2=Female)}
#' \item{birth_year}{Year of Birth (integer) (yyyy)}
#' }
#' @family Sample Data
#' @seealso \code{\link{make_lic_history}}
NULL

#' Sample Data: Dashboard Metrics (Jan 1, 2008 through Dec 31, 2018)
#' 
#' This list holds national/regional dashboard metrics (full-year) for the sample 
#' data (2008 through 2018) and corresponds to "All Hunters & Anglers". Each element 
#' (metric) holds four segments ("tot" = overall, "res" = by Residency, "sex" = by Gender, 
#' "agecat" = by Age Category)
#'
#' @docType data
#' @keywords datasets
#' @name metrics
#' @format A list of 3 metrics (part, part_new, churn) with 4 segments each 
#' (tot, res, sex, agecat): 12 data frames total
#' \describe{
#' \item{part}{Sportspersons per Year}
#' \item{part_new}{New Sportspersons (i.e., recruits) per Year}
#' \item{churn}{Churn (i.e., turnover) per Year}
#' }
#' @family Sample Data
#' @seealso \code{\link{est_part}} \code{\link{est_churn}}
NULL


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
