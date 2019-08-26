# Documentation for Sample Data

# Standarized License Data ---------------------------------------------------

#' Salic Sample Data Overview
#' 
#' Salic provides sample data for each stage of dashboard production:
#' \itemize{
#'   \item Standardized License Data
#'     \itemize{
#'       \item Customers: \code{\link{cust}}
#'       \item License Types: \code{\link{lic}}
#'       \item Sales: \code{\link{sale}}
#'     }
#'   \item License History Data: \code{\link{history}}
#'   \item Dashboard Metrics
#'     \itemize{
#'       \item Calculated dashboard metrics (stored in list): \code{\link{metrics}}
#'       \item Metrics formatted for Dashboard Input (single table): \code{\link{dashboard}}
#'     }
#' }
#' 
#' @name data_salic
#' @family Sample Data
#' @seealso Salic Function Reference: \code{\link{salic}}
NULL

#' Sample Data: 30,000 Customers
#'
#' @docType data
#' @keywords datasets
#' @name cust
#' @format A data frame with 3 variables
#' \describe{
#' \item{cust_id}{Customer ID (numeric) (primary key) - Note that IDs can be stored 
#' as character, but this is less efficient.}
#' \item{sex}{Gender (numeric) (1=Male, 2=Female, NA=Unknown)}
#' \item{birth_year}{Year of Birth (numeric) (yyyy, NA=Unknown)}
#' }
#' @family Sample Data
#' @seealso Salic Function Reference: \code{\link{salic}}
NULL

#' Sample Data: License Types
#'
#' @docType data
#' @keywords datasets
#' @name lic
#' @format A data frame with 3 variables
#' \describe{
#' \item{lic_id}{License ID (numeric) (primary key) - Note that IDs can be stored 
#' as character, but this is less efficient.}
#' \item{type}{License Type (character) ("hunt" = hunting license, "fish" = 
#' fishing license,  "combo" = combination hunting/fishing license)}
#' \item{duration}{Duration in years (numeric) (1 = 1yr/short-term, 2 = 2yr,..., 
#' 99 = lifetime)}
#' }
#' @family Sample Data
#' @seealso Salic Function Reference: \code{\link{salic}}
NULL

#' Sample Data: Sales 2008 thru 2018
#'
#' @docType data
#' @keywords datasets
#' @name sale
#' @format A data frame with 5 variables
#' \describe{
#' \item{cust_id}{Customer ID (foreign key)}
#' \item{lic_id}{License ID (foreign key)}
#' \item{year}{License Year (numeric) (yyyy)}
#' \item{month}{Transaction Purchase Month (numeric) (1 = Jan, 2 = Feb, ...)}
#' \item{res}{Residency (numeric) (1 = State Resident, 0 = State Nonresident, 
#' NA = Unknown)}
#' }
#' @family Sample Data
#' @seealso Salic Function Reference: \code{\link{salic}}
NULL

# Derived License Data ----------------------------------------------------

#' Sample Data: License  History
#' 
#' This is a table that corresponds to the "all_sports" permission for the included
#' license sample data; containing 1 row per customer per year a license was held.
#'
#' @docType data
#' @keywords datasets
#' @name history
#' @format A data frame with 9 variables
#' \describe{
#' \item{cust_id}{Customer ID (numeric) (composite key)}
#' \item{year}{License Year (numeric) (yyyy) (composite key)}
#' \item{duration_run}{Running Duration in years (numeric)}
#' \item{month}{Earliest Transaction Month (numeric) (1=Jan, 2=Feb, ...)}
#' \item{res}{Residency (numeric) (1=Res, 0=Nonres)}
#' \item{sex}{Gender (numeric) (1=Male, 2=Female)}
#' \item{birth_year}{Year of Birth (numeric) (yyyy)}
#' \item{R3}{R3 Group this Year (numeric) (1=carry, 2=renew, 3=reactivate, 4=recruit)}
#' \item{lapse}{Lapsed next Year? (numeric) (1=lapse, 0=renew)}
#' }
#' @family Sample Data
#' @seealso Salic Function Reference: \code{\link{salic}}
NULL

#' Sample Data: Dashboard Metrics
#' 
#' This list holds national/regional dashboard metrics (full-year) for the sample 
#' data and corresponds to "All Hunters & Anglers" aged 18-64 each year. Each element 
#' (metric) holds four segments ("tot" = overall, "res" = by Residency, "sex" = by Gender, 
#' "agecat" = by Age Category)
#'
#' @docType data
#' @keywords datasets
#' @name metrics
#' @format A list of 3 metrics (participants, recruits, churn) with 4 segments each 
#' (tot, res, sex, agecat): 12 data frames total
#' \describe{
#' \item{participants}{Sportspersons per Year}
#' \item{recruits}{New Sportspersons (i.e., recruits) per Year}
#' \item{churn}{Churn (i.e., turnover) per Year}
#' }
#' @family Sample Data
#' @seealso Salic Function Reference: \code{\link{salic}}
NULL

#' Sample Data: Metrics formatted for Dashboard Input
#' 
#' This data frame holds national/regional dashboard metrics formatted for dashboard input.
#' The results correspond to full-year metrics for the sample data and include permission 
#' breakouts for customers aged 18-64 each year. Three permissions are included: 
#' All Hunters & Anglers (group = "all_sports"), Hunters (group = "hunt"), and 
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
#' @seealso Salic Function Reference: \code{\link{salic}}
NULL
