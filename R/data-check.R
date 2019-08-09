# Checks for Data

# Internal Functions ---------------------------------------------------

#' Internal Functions: Individual Data Checks
#' 
#' These functions are intended to be called from \code{\link{data_check_table}}. 
#' Prints a warning message on a failed check.
#' 
#' \itemize{
#'   \item data_primary_key: check that primary keys are unique and non-missing
#'   \item data_required_vars: check that required variables are included
#'   \item data_allowed_values: check that variables are restriced to allowed values
#' }
#' 
#' @inheritParams data_check_table
#' @family functions to check data format
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(lic)
#' 
#' # primary keys not unique
#' bind_rows(lic, lic) %>% 
#'     data_primary_key("lic", "lic_id")
#'     
#' # primary keys missing
#' lic$lic_id[1] <- NA
#' data_primary_key(lic, "lic", "lic_id")
#' 
#' # missing required variables
#' select(lic, -duration) %>%
#'     data_required_vars("lic", c("lic_id", "type", "duration"))
#' 
#' # includes values that aren't allowed
#' allowed_values <- list(type = c("hunt", "fish"), duration = 1)
#' data_allowed_values(lic, "lic", allowed_values)
data_primary_key <- function(df, df_name, primary_key) {
    # uniqueness condition
    unique_keys <- length(unique(df[[primary_key]]))
    msg <- paste0(
        df_name, ": Primary key (", primary_key, ") not unique: ", unique_keys,  
        " keys and ", nrow(df), " rows"
    )
    if (unique_keys < nrow(df)) warning(msg, call. = FALSE)
    
    # non-missing condition
    msg <- paste0(
        df_name, ": Primary key (", primary_key, ") contains missing values"
    )
    if (any(is.na(df[[primary_key]]))) warning(msg, call. = FALSE)
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


#' Internal Function: Check allowed values for a single variable
#' 
#' This is intended to be called from \code{\link{data_allowed_values}}. 
#' Prints a warning message on a failed check.
#' 
#' @param x vector: variable to check
#' @param x_name character: name of variable to print in warning
#' @param allowed_values vector: values the variable is allowed to take
#' @family functions to check data format
#' @keywords internal
#' @export
#' @examples
#' data(lic)
#' variable_allowed_values(lic$type, "lic-type", c("hunt", "fish", "combo"))
#' 
#' x <- c(NA, "hi", 1, lic$type)
#' variable_allowed_values(x, "lic-type", c("hunt", "fish", "combo"))
variable_allowed_values <- function(x, x_name, allowed_values) {
    not_allowed <- dplyr::setdiff(unique(x), allowed_values)
    msg <- paste0(
        x_name, ": contains values that aren't allowed: ", 
        paste(not_allowed, collapse = ", ")
    )
    if (length(not_allowed) > 0) warning(msg, call. = FALSE)
}

#' @rdname data_primary_key
#' @export
data_allowed_values <- function(df, df_name, allowed) {
    vars <- names(allowed)
    for (i in vars) {
        variable_allowed_values(
            df[[i]], paste(df_name, i, sep = "-"), allowed[[i]])
    }
}

# User-facing Functions ---------------------------------------------------

#' Check key coverage between two tables
#' 
#' Intended to be called from \code{\link{data_check}}, prints a warning if
#' there are rows in df_foreign with keys that aren't included in df_primary
#' 
#' @param df_foreign data frame: table that stores key as foreign
#' @param df_primary data frame: table that stores key as primary
#' @param key character: name of variable that acts as key
#' @family functions to check data format
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

### TODO - START HERE ###

#' Check formatting rules for standardized data tables
#' 
#' Prints a warning for every formatting rule that is flagged for the specified table. 
#' Table-specific versions are convenience functions that call data_check_table()
#' with specified arguments.
#' 
#' @param df data frame: table to check
#' @param df_name character: name of relevant data table ("cust", "lic", or "sale")
#' @param primary_key character: name of variable that acts as primary key 
#' (which should be unique and non-missing)
#' @param required_vars character: variables that should be included
#' @param allowed_values list: named list with allowed values for specific variables
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

#' @rdname data_check_table
#' @export
data_check_cust <- function() {
    
}

#' @rdname data_check_table
#' @export
data_check_sale <- function() {
    
}

#' Check standardized data (cust, lic, sale) formatting rules 
#' 
#' This function is simply a wrapper for several calls to \code{\link{data_check_table}} and 
#' \code{\link{data_foreign_key}} to check formatting rules for all standardized 
#' data tables
#' 
#' @param cust data frame: customer table
#' @param lic data frame: license types table
#' @param sale data frame: transactions table
#' @family functions to check data format
#' @import dplyr
#' @export
#' @examples
#' #examples
data_check <- function(cust, lic, sale) {
    data_check_cust(cust)
    data_check_lic(lic)
    data_check_sale(sale)
    data_foreign_key(sale, cust, "cust_id")
    data_foreign_key(sale, lic, "lic_id")
}
