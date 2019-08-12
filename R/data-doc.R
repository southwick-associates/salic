# Documentation for Sample Data

# Standarized License Data ---------------------------------------------------

#' Sample Data: 25,000 Customers
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
#' \item{duration}{Duration in years (integer) (1=1yr/short-term, 2=2yr,..., 99=lifetime)}
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
#' \item{month}{Transaction Purchase Month (integer) (1=Jan, 2=Feb, ...)}
#' \item{res}{Residency (integer) (1=Res, 0=Nonres)}
#' }
#' @family Sample Data
NULL

# Derived License Data ----------------------------------------------------

#' Sample Data: License  History (Jan 1, 2008 through June 30, 2019)
#' 
#' This is a table that corresponds to the "all_sports" permission for the included
#' license sample data; containing 1 row per customer per year a license was held.
#'
#' @docType data
#' @keywords datasets
#' @name history
#' @format A data frame with 9 variables
#' \describe{
#' \item{cust_id}{Customer ID (integer) (composite key)}
#' \item{year}{License Year (integer) (yyyy) (composite key)}
#' \item{duration_run}{Running Duration in years (integer)}
#' \item{month}{Earliest Transaction Month (integer) (1=Jan, 2=Feb, ...)}
#' \item{res}{Residency (integer) (1=Res, 0=Nonres)}
#' \item{sex}{Gender (integer) (1=Male, 2=Female)}
#' \item{birth_year}{Year of Birth (integer) (yyyy)}
#' \item{R3}{R3 Group this Year (integer) (1=carry, 2=renew, 3=reactivate, 4=recruit)}
#' \item{lapse}{Lapsed next Year? (integer) (1=lapse, 0=renew)}
#' }
#' @family Sample Data
#' @seealso \code{\link{make_lic_history}}
NULL

#' Sample Data: Dashboard Metrics (Jan 1, 2008 through Dec 31, 2018)
#' 
#' This list holds national/regional dashboard metrics (full-year) for the sample 
#' data and corresponds to "Hunters & Anglers" aged 18-64 each year. Each element 
#' (metric) holds four segments ("tot" = overall, "res" = by Residency, "sex" = by Gender, 
#' "agecat" = by Age Category)
#'
#' @docType data
#' @keywords datasets
#' @name metrics
#' @format A list of 3 metrics (participants, recruits, churn) with 4 segments each 
#' (tot, res, sex, agecat): 12 data frames total
#' \describe{
#' \item{part}{Sportspersons per Year}
#' \item{recruit}{New Sportspersons (i.e., recruits) per Year}
#' \item{churn}{Churn (i.e., turnover) per Year}
#' }
#' @family Sample Data
#' @seealso \code{\link{est_part}} \code{\link{est_churn}}
NULL

#' Sample Data: Metrics formatted for Dashboard Input
#' 
#' This data frame holds national/regional dashboard metrics formatted for dashboard input.
#' The results correspond to full-year metrics for the sample data and include permission 
#' breakouts for customers aged 18-64 each year. Three permissions are included: 
#' Hunters & Anglers (group = "all_sports"), Hunters (group = "hunt"), and 
#' Anglers (group = "fish") . 
#'
#' @docType data
#' @keywords datasets
#' @name dashboard
#' @format A data frame with 7 variables
#' \describe{
#' \item{timeframe}{Time period covered ("full-year" or "mid-year")}
#' \item{group}{Permission group ("all_sports", "hunt", "fish")}
#' \item{segment}{Demographic group ("All", "Age", "Gender", "Residency")}
#' \item{year}{Calendar Year (2008, 2009, ..., 2018)}
#' \item{category}{Category breakouts for segment}
#' \item{metric}{Metric Summarized ("participants", "recruits", "churn")}
#' \item{value}{Value of metric for given dimension}
#' }
#' @family Sample Data
#' @seealso \code{\link{format_result}}
NULL

# Additional Data ---------------------------------------------------------

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
