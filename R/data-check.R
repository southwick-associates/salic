# Checks for Data

# done: does it have the required columns?
# done: are the primary keys correctly set?
# are categorcial variables restricted to the allowed values?
# are values completely populated for variables that can't have missing values?
# are foreign keys covered in primary keys?

# Single Test Functions ---------------------------------------------------

#' Internal Functions: Individual Data Checks
#' 
#' These functions are intended to be called from \code{\link{data_check_table}}. 
#' Prints a warning message on a failed check.
#' 
#' @inheritParams data_check_table
#' @family functions to check data format
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(lic)
#' 
#' bind_rows(lic, lic) %>% 
#'     data_primary_key("lic", "lic_id")
#'     
#' select(lic, -duration) %>%
#'     data_required_vars("lic", c("lic_id", "type", "duration"))
data_primary_key <- function(df, df_name, primary_key) {
    unique_keys <- length(unique(df[[primary_key]]))
    msg <- paste0(
        df_name, ": Primary key (", primary_key, ") not unique: ", unique_keys,  
        " keys and ", nrow(df), " rows"
    )
    if (unique_keys < nrow(df)) warning(msg, call. = FALSE)
}

#' @rdname data_primary_key
#' @export
data_required_vars <- function(df, df_name, required_vars) {
    not_included <- dplyr::setdiff(required_vars, colnames(df))
    msg <- paste0(
        df_name, ": ", length(not_included), " Missing variable(s): ",  
        paste(not_included, collapse = ", ")
    )
    if (length(not_included) > 0) warning(msg, call. = FALSE)
}

data_allowed_values <- function(df, df_name, allowed_values) {
    
}
data_no_missing <- function(df, df_name, no_missing) {
    
}

# Single-Table Functions ---------------------------------------------------

#' Check format for data tables
#' 
#' Any failing checks will produce warnings.
#' 
#' @param df data frame: table to check
#' @param df_name character: name of relevant data table (e.g., "cust", "lic", or "sale")
#' @param primary_key character: name of variable that acts as primary key (if applicable)
#' @param required_vars character: variables that should be included
#' @param allowed_values list: named list with allowed values for specific variables
#' @param no_missing character: name of variables that shouldn't contain missing values
#' @family functions to check data format
#' @keywords internal
#' @import dplyr
#' @export
#' @examples
#' library(dplyr)
#' data(lic)
#' 
#' # one table
#' x <- select(lic, -duration)
#' data_check_lic(x)
#' 
#' x <- bind_rows(x, x)
#' data_check_lic(x)
data_check_table <- function(
    df, df_name, primary_key, required_vars, allowed_values, no_missing
) {
    data_primary_key(df, df_name, primary_key)
    data_required_vars(df, df_name, required_vars)
}

#' @rdname data_check_table
#' @export
data_check_lic <- function(
    df, df_name = "lic", primary_key = "lic_id", 
    required_vars = c("lic_id", "type", "duration"), 
    allowed_values = list(type = c("fish", "hunt", "combo"), duration = 1:99),
    no_missing = "duration"
) {
    data_check_table(df, df_name, primary_key, required_vars)
}
data_check_cust <- function() {
    
}
data_check_sale <- function() {
    
}

# Multi-Table Functions ---------------------------------------------------

#' Internal Function: Check key coverage between two tables
#' 
#' Intended to be called from \code{\link{data_check}}, prints a warning if
#' there are rows in df_foreign with keys that aren't included in df_primary
#' 
#' @param df_foreign data frame: table that stores key as foreign
#' @param df_primary data frame: table that stores key as primary
#' @param key character: name of variable that acts as key
#' @family functions to check data format
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(sale, cust)
#' data_foreign_key(sale, cust, "cust_id")
#' 
#' cust <- filter(cust, cust_id > 5)
#' data_foreign_key(sale, cust, "cust_id")
data_foreign_key <- function(df_foreign, df_primary, key) {
    foreign_name <- deparse(substitute(df_foreign))
    primary_name <- deparse(substitute(df_primary))
    missing_keys <- dplyr::anti_join(df_foreign, df_primary, by = key)
    
    msg <- paste0(
        primary_name, ": missing 1 or more ", key, 
        " values that are present in the ", foreign_name,  " table"
    )
    if (nrow(missing_keys) > 0) warning(msg, call. = FALSE)
}

# Check standardized Data
data_check <- function(cust, lic, sale) {
    
}