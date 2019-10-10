# namespace definitions and package-level documentation
# - see http://r-pkgs.had.co.nz/man.html#man-packages for documenting packages

#' @import dplyr
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @importFrom utils capture.output head tail write.csv
NULL

# certain functions reference global variables (e.g., named variables in data frames)
# - these occur in a number of places in salic (mostly validation functions)
# - R CMD check doesn't like this: declaring them in globalVariables() is needed
# - https://github.com/r-lib/devtools/issues/1714
if(getRversion() >= "2.15.1") utils::globalVariables(c(
    "age", "age_year", "agecat", "birth_year", "change_cust",
    "change_revenue", "change_sales", "customers", "dot", "dot2", "duration", 
    "issue_month", "issue_year",  "lastvar",  "sales", "var_old", "yr_diff",  
    ":=", "old", "cust_id", "month", "year", "lapse", ".", "next_year",
    "yrs_till_next", "duration_run_lag", "year_last", "..fwd_cols",
    "duration_run", "i.lapse", "i.month"
))

#' salic: Prepare Agency Dashboard Data
#' 
#' Run vignette("salic") from the R console for an introduction. See
#' below for a summary of the core functions. 
#' 
#' @section Standardized Data:
#' \itemize{
#'   \item see \code{\link{cust}}, \code{\link{lic}}, \code{\link{sale}} for 
#'   information about expected license data formats
#'   \item use \code{\link{data_check}} to ensure correct formatting for state
#'   license data
#' }
#' 
#' @section License History:
#' \itemize{
#'   \item use \code{\link{rank_sale}} to condense sales to 1 row per customer-year
#'   \item use \code{\link{make_history}} to produce a \code{\link{history}} table
#'    that facilitates flexible querying about customer trends
#'   \item see \code{\link{history_check}} to run checks for the the output of
#'   make_history()
#' }
#' 
#' @section Dashboard Metrics:
#' \itemize{
#'   \item use \code{\link{label_categories}} & \code{\link{recode_agecat}} to
#'   prepare the history table
#'   \item use \code{\link{est_part}} to get participant counts in a 
#'   \code{\link{metrics}} list
#'   \item use \code{\link{scaleup_part}} to account for missing values in 
#'   segments such as gender, age, etc.
#'   \item use  \code{\link{est_churn}} to get churn rates per year
#'   \item use \code{\link{format_result}} to stack metrics into a
#'   \code{\link{dashboard}} data frame
#' }
#' 
#' @docType package
#' @name salic
NULL
