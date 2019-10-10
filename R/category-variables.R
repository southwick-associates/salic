# functions for working with category variables

# For Production Code -------------------------------------------------------

#' Create age category based on year (of sale) and birth year
#' 
#' Uses 'birth_year' (from cust) and 'year' (from sale) to identify
#' age for each license year
#' 
#' @param dat data frame: Input table
#' @param age_labs character: labels to use for age category
#' @param age_breaks numeric: breaks for age category passed to 
#' \code{\link[base]{cut}}
#' @param max_age numeric: maximum allowed age. Anything above will be set to missing.
#' @param suppress_check logical: If TRUE, does not print a coding summary
#' @family functions for working with category variables
#' @seealso Salic Function Reference: \code{\link{salic}}
#' @export
#' @examples 
#' library(dplyr)
#' data(history)
#' x <- recode_agecat(history, suppress_check = FALSE)
recode_agecat <- function(
    dat, 
    age_labs = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),  
    age_breaks = c(-Inf, 17, 24, 34, 44, 54, 64, Inf), 
    max_age = 110,
    suppress_check = TRUE
) {
    # make variables
    dat <- dat %>% mutate(
        age_year = year - birth_year, 
        age_year = ifelse(age_year > max_age, NA, age_year), 
        age = cut(age_year, breaks = age_breaks, labels = FALSE), 
        agecat = cut(age_year, breaks = age_breaks, labels = age_labs)
    )
    # check output
    if (!suppress_check) {
        cat("\nCategory Coding Summary:\n")
        count(dat, age_year, age, agecat) %>% 
            data.frame() %>% 
            print(row.names = FALSE)
    }
    dat
}

#' Convert multiple numeric variables to factors
#' 
#' This is a convenience function to convert the expected set of numeric
#' category variables to factors. It's basically a wrapper for
#'  \code{\link{df_factor_var}}.
#'  
#' @inheritParams df_factor_var
#' @param categories character: vector of variable names to convert to factor
#' (if present)
#' @param ... additional arguments passed to \code{\link{df_factor_var}}
#' @export
#' @family functions for working with category variables
#' @seealso Salic Function Reference: \code{\link{salic}}
#' @examples 
#' library(dplyr)
#' data(history)
#' x <- label_categories(history, suppress_check = FALSE)
label_categories <- function(df, categories = c("R3", "sex", "res"), ...) {
    vars <- intersect(categories, colnames(df))
    for (i in vars) {
        df_factor_i <- get(paste0("df_factor_", i)) # df_factor_age, etc.
        df <- df_factor_i(df, ...)
    }
    df
}

# Vector-based Labelling --------------------------------------------------------
# These act on vectors (typically variables in a data frame)

#' Convert numeric variable to factor
#' 
#' These are convenience functions for use with license data. They are
#' wrappers for \code{\link[base]{factor}} with specified values.
#' @param x numeric: Input numeric vector
#' @param levels numeric: Levels for input numeric vector
#' @param labels labels: Labels to use for output factor vector
#' @param suppress_check logical: If TRUE, does not print a coding summary
#' @param ... Other arguments passed to \code{\link[base]{factor}}
#' @export
#' @family functions for working with category variables
#' @examples
#' library(dplyr)
#' data(history)
#' x <- history %>% mutate(
#'     R3 = factor_R3(R3, suppress_check = FALSE),
#'     sex = factor_sex(sex, suppress_check = FALSE),
#'     res = factor_res(res, suppress_check = FALSE)
#' )
factor_var <- function(x, levels, labels, suppress_check = TRUE, ...) {
    new <- factor(x, levels = levels, labels = labels, ...)
    if (!suppress_check) {
        dplyr::bind_cols(new = new, old = x) %>% 
            dplyr::count(new, old) %>%
            print(n = Inf)
    }
    new
}

#' @rdname factor_var
#' @export
factor_age <- function(
    x,  levels = 1:7, 
    labels = c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    ...
) {
    factor_var(x, levels = levels, labels = labels, ...)
}

#' @rdname factor_var
#' @export
factor_sex <- function( x, levels = 1:2, labels = c("Male", "Female"), ...) {
    factor_var(x, levels = levels, labels = labels, ...)
}

#' @rdname factor_var
#' @export
factor_res <- function(x, levels = c(1,0), labels = c("Resident", "Nonresident"), ...) {
    factor_var(x, levels = levels, labels = labels, ...)
}

#' @rdname factor_var
#' @export
factor_R3 <- function(x, levels = 1:4,  
                      labels = c("Carry", "Renew", "Reactivate", "Recruit"), ...) {
    factor_var(x, levels = levels, labels = labels, ...)
}

# Data Frame-based Labelling ----------------------------------------------------
# These act on data frames, which is convenient for piping

#' Convert numeric to factor in data frame and check
#' 
#' These are convenience functions for license data, similar to \code{\link{factor_var}}
#' but operate on data frames (useful for piping) and produce a check summary.
#' @param df data frame: Input data frame
#' @param var character: Name of numeric variable to convert
#' @inheritParams factor_var
#' @export
#' @family functions for working with category variables
#' @examples
#' library(dplyr)
#' data(history)
#' x <- history %>%
#'     df_factor_R3(suppress_check = FALSE) %>%
#'     df_factor_res(suppress_check = FALSE) %>%
#'     df_factor_sex(suppress_check = FALSE)
df_factor_var <- function(df, var, levels, labels, suppress_check = TRUE, ...) {
    df$var_old <- df[[var]]
    df[[var]] <- factor(df[[var]], levels = levels, labels = labels, ...)
    if (!suppress_check) {
        count(df, new = .data[[var]], old = var_old) %>% 
            print(n = Inf)
        cat("\n")
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
                         labels = c("Carry", "Renew", "Reactivate", "Recruit"), ...
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
