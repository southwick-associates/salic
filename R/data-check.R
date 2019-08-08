# Checks for Data

# done: does it have the required columns?
# done: are the primary keys correctly set?
# are categorcial variables restricted to the allowed values?
# are values completely populated for variables that can't have missing values?

# Single Test Functions ---------------------------------------------------

#' Internal Functions: Individual Data Checks
#' 
#' These functions are intended to be called from \code{\link{data_check_table}}
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
    if (unique_keys < nrow(df)) {
        warning(df_name, ": Primary key not unique: ", unique_keys, 
                " keys and ", nrow(df), " rows", call. = FALSE)
    }
}

#' @rdname data_primary_key
#' @export
data_required_vars <- function(df, df_name, required_vars) {
    not_included <- dplyr::setdiff(required_vars, colnames(df))
    if (length(not_included) > 0) {
        warning(df_name, ": ", length(not_included), " Missing column(s): ",  
                paste(not_included, collapse = ", "), call. = FALSE)
    }
}

data_allowed_values <- function(df, df_name, allowed_values) {
    
}
data_no_missing <- function(df, df_name, no_missing) {
    
}

# Table Check Functions ---------------------------------------------------

#' Check format for data tables
#' 
#' Any failing checks will produce warnings.
#' 
#' @param df data frame: table to check
#' @param df_name character: name of relevant data table (e.g., "lic", "sale", etc.)
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
# call any number of data_check_[table] functions
data_check <- function(...) {
    
}