# functions for labelling numeric variables


# Vector Functions --------------------------------------------------------

#' Convert numeric variable to factor
#' 
#' These are convenience functions for use with license data. They are
#' wrappers for \code{\link[base]{factor}} with specified values.
#' @param x numeric: Input numeric vector
#' @param levels numeric: Levels for input numeric vector
#' @param labels labels: Labels to use for output factor vector
#' @param ... Other arguments passed to \code{\link[base]{factor}}
#' @export
#' @family functions for labelling numeric variables
#' @examples
#' library(salic)
#' library(dplyr)
#' data(cust, sale, package = "salic")
#' sale2 <- left_join(sale, cust) %>% 
#'     mutate(birth_year = lubridate::year(dob)) %>% 
#'     recode_agecat()
#' sale2 %>%
#'     select(-agecat) %>%
#'     mutate(agecat = factor_age(age))
factor_var <- function(x, levels, labels, ...) {
    factor(x, levels = levels, labels = labels, ...)
}

#' @rdname factor_var
#' @export
factor_age <- function(
    x,  levels = 1:7, 
    labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    ...
) {
    factor(x, levels = levels, labels = labels, ...)
}

#' @rdname factor_var
#' @export
factor_sex <- function( x, levels = 1:2, labels = c("Male", "Female"), ...) {
    factor(x, levels = levels, labels = labels, ...)
}

#' @rdname factor_var
#' @export
factor_res <- function(x, levels = c(1,0), labels = c("Resident", "Nonresident"), ...) {
    factor(x, levels = levels, labels = labels, ...)
}

#' @rdname factor_var
#' @export
factor_R3 <- function(x, levels = 1:4,  
                      labels = c("Carry", "Retain", "Reactivate", "Recruit"), ...) {
    factor(x, levels = levels, labels = labels, ...)
}


# Data Frame Functions ----------------------------------------------------

#' Convert numeric to factor in data frame and check
#' 
#' These are convenience functions for license data, similar to \code{\link{factor_var}}
#' but operate on data frames (useful for piping) and produce a check summary.
#' @param df data frame: Input data frame
#' @param var numeric: Numeric variable for input
#' @param suppress_check logical: If TRUE, does not print a coding summary
#' @inheritParams factor_var
#' @export
#' @family functions for labelling numeric variables
#' @examples
#' library(salic)
#' library(dplyr)
#' data(cust, sale, package = "salic")
#' sale2 <- left_join(sale, cust) %>% 
#'     mutate(birth_year = lubridate::year(dob)) %>% 
#'     recode_agecat()
#' sale2 <- sale2 %>%
#'     select(-agecat) %>%
#'     df_factor_age()
df_factor_var <- function(df, var, levels, labels, suppress_check = TRUE, ...) {
    df$var_old <- df[[var]]
    df[[var]] <- factor(df[[var]], levels = levels, labels = labels, ...)
    if (!suppress_check) {
        count(df, .data[[var]], var_old) %>% 
            data.frame() %>% print(row.names = FALSE)
    }
    select(df, -var_old)
}

#' @rdname df_factor_var
#' @export
df_factor_sex <- function(df, var = "sex", levels = 1:2, 
                          labels = c("Male", "Female"), ...
) {
    df_factor_var(df, var, levels, labels, ...)
}

#' @rdname df_factor_var
#' @export
df_factor_res <- function(df, var = "res", levels = c(1,0), 
                          labels = c("Resident", "Nonresident"), ...
) {
    df_factor_var(df, var, levels, labels, ...)
}

#' @rdname df_factor_var
#' @export
df_factor_R3 <- function(df, var = "R3", levels = 1:4, 
                         labels = c("Carry", "Retain", "Reactivate", "Recruit"), ...
) {
    df_factor_var(df, var, levels, labels, ...)
}

#' @rdname df_factor_var
#' @export
df_factor_age <- function(
    df, var = "age", levels = 1:7,    
    labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"), 
    ...
) {
    df_factor_var(df, var, levels, labels, ...)
}
