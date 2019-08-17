# license history functions

# Preparing Sale ----------------------------------------------------------

#' Filter sales to 1 row per customer per year.
#'
#' Intended to run before \code{\link{make_lic_history}} to ensure that 
#' multi-year/lifetime sales are accounted for.  
#' The default (and intended purpose) is to pick the maximum "duration" value 
#' per customer-year. Optionally, it will also pick the minimum value of month 
#' (useful in mid-year dashboards) if first_month = TRUE. 
#' 
#' @param sale data frame: Input sales data
#' @param rank_var character: name of variable(s) to use for ranking
#' @param grp_var character: name of variable(s) used for grouping
#' @param first_month logical: If TRUE, also runs \code{\link{join_first_month}}
#' to ensure the output contains the earliest month by grp_var 
#' (useful for mid-year dashboards)
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @import dplyr
#' @importFrom utils tail
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' 
#' sale_unranked <- left_join(sale, lic)
#' sale_ranked <- rank_sale(sale_unranked)
#'     
#' # check sale ranking - highest duration will always be picked
#' left_join(
#'     count(sale_ranked, duration), 
#'     distinct(sale_unranked, cust_id, year, duration) %>% count(duration), 
#'     by = "duration",
#'     suffix = c(".ranked", ".unranked")
#' )
#' 
#' # with earliest month included
#' sale_ranked <- rank_sale(sale_unranked, first_month = TRUE)
#' left_join(
#'     count(sale_ranked, month), 
#'     distinct(sale_unranked, cust_id, year, month) %>% count(month), 
#'     by = "month",
#'     suffix = c(".ranked", ".unranked")
#' )
rank_sale <- function(
    sale, rank_var = "duration", grp_var = c("cust_id", "year"), first_month = FALSE
) {
    if (!all(rank_var %in% colnames(sale))) {
        stop(
            "All rank_var variable(s) (", paste(rank_var, collapse = ", "), 
            ") must be included in sale", call. = FALSE
        )
    }
    setDT(sale)
    setorderv(sale, rank_var) # order ascending
    ranked <- sale
    ranked <- ranked[, tail(.SD, 1), by = grp_var] # pick last
    
    if (first_month) {
        if (!"month" %in% colnames(sale)) return(sale)
        ranked <- join_first_month(ranked, sale, grp_var)
    }
    as_tibble(ranked)
}

#' Internal Function: Join earliest sale month by customer-year
#'
#' This function is only intended to be run as part of
#' \code{\link{rank_sale}}; ensures the earliest month value gets recorded in 
#' the license history table (useful for mid-year results).
#' 
#' @param sale_ranked data.table: Input ranked sales data
#' @param sale_unranked data.table: Input unranked sales data
#' @inheritParams rank_sale
#' @import dplyr
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @importFrom utils head
#' @family license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' sale_unranked <- left_join(sale, lic)
#' sale_ranked <- rank_sale(sale_unranked)
#' 
#' data.table::setDT(sale_unranked)
#' data.table::setDT(sale_ranked)
#' sale_ranked <- join_first_month(sale_ranked, sale_unranked) %>%
#'     as_tibble()
#' 
#' # check month ranking - earliest month will always be picked
#' left_join(
#'     count(sale_ranked, month), 
#'     distinct(sale_unranked, cust_id, year, month) %>% count(month), 
#'     by = "month",
#'     suffix = c(".ranked", ".unranked")
#' )
join_first_month <- function(
    sale_ranked, sale_unranked, grp_var = c("cust_id", "year")
) {
    if (!"month" %in% colnames(sale_unranked)) {
        stop("month variable must be included in sale_unranked", call. = FALSE)
    }
    setorderv(sale_unranked, "month") # order ascending
    sale_unranked <- sale_unranked[, .(month = head(month, 1)), by = grp_var]
    
    sale_ranked[, month := NULL]
    setkeyv(sale_ranked, grp_var)
    setkeyv(sale_unranked, grp_var)
    sale_ranked[sale_unranked, nomatch = 0]
}

#' Internal Function: Check years range & sort
#' 
#' Prints a warning any of the specified years aren't included in the dataset and 
#' returns a vector of availabe years, sorted ascending. The sort ensures correct 
#' ordering in subsequent license history calculations, which include iterations
#' by year that would produce incorrect results if not sorted.
#' 
#' This function is intended to be called from \code{\link{make_lic_history}},
#' \code{\link{identify_R3}}, or \code{\link{identify_lapse}}.
#' 
#' @param df data frame: table that contains "year" variable
#' @param func_name character: name of function to print in warning
#' @inheritParams make_lic_history
#' @family license history functions
#' @keywords internal
#' @export
#' @examples
#' data(sale)
#' prep_yrs(c(2010, 2008, 2015), sale, "my_function")
#' prep_yrs(c(2007, 2015, 2010), sale, "my_function") # print a warning
prep_yrs <- function(yrs, df, func_name) {
    yrs <- sort(yrs)
    if (any(!yrs %in% unique(df$year))) {
        yrs_specified <- yrs 
        yrs <- dplyr::intersect(yrs, unique(df$year))
        warning(
            "Certain yrs in ", func_name, " are missing from the input table:\n", 
            "- Years specified: ", paste(yrs_specified, collapse = ", "), "\n",
            "- Years used:      ", paste(yrs, collapse = ", "), call. = FALSE
        )
    }
    yrs
}

# Making History -----------------------------------------------------------

#' Make a License History table with 1 row per customer per year
#'
#' Details my dude
#' 
#' @param sale_ranked data frame: Sales table from which license history will be made
#' @param yrs numeric: Years in sales data (column 'year') from which
#' to create license history
#' @param carry_vars character: additional variables to carry over from previous year
#' (for multi-year and lifetime licenses).
#' @param yrs_lapse numeric: years to include in lapse calculation (defaults to yrs). 
#' If NULL, lapse will not be calculated (useful for mid-year results)
#' @param include_R3 logical: If TRUE, also calculate R3
#' @import dplyr
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(sale, lic)
#' sale_ranked <- left_join(sale, lic) %>% rank_sale()
#' 
#' carry_vars = c("res", "month")
#' history <- sale_ranked %>%
#'    make_history(2008:2018, carry_vars, show_diagnostics = TRUE)
make_history <- function(
    sale_ranked, yrs, carry_vars = NULL, yrs_lapse = yrs, 
    include_R3 = TRUE, show_diagnostics = FALSE
) {
    yrs <- prep_yrs(yrs, sale_ranked, "make_lic_history()")
    slct_cols <- c("cust_id", "year", "duration", carry_vars)
    data_required_vars(sale_ranked, "make_history()", slct_cols, use_error = TRUE)
    x <- list()
    
    for (i in seq_along(yrs)) {
        x[[i]] <- sale_ranked %>%
            filter(year == yrs[i]) %>% 
            select(slct_cols) %>%
            mutate(duration_run = duration)
        if (i == 1) {
            x[[i]] <- mutate(x[[i]], year_last = NA_integer_)
        } else {
            x[[i]] <- x[[i]] %>%
                forward_duration(x[[i-1]], yrs[i], carry_vars) %>%
                forward_vars(carry_vars)
            x[[i-1]] <- x[[i-1]] %>%
                make_lapse(x[[i]], yrs_lapse)
        }
    }
    x <- x %>%
        lapply(function(x) filter(x, !is.na(duration_run), duration_run > 0)) %>%
        bind_rows() %>%
        make_R3(include_R3, yrs) %>%
        mutate(duration_run = as.integer(.data$duration_run))
    if (!show_diagnostics) {
        x <- select(x, -.data$duration_run_lag, -.data$duration,  
                    -.data$year_last, -.data$yrs_since)
    }
    x
}

#' @rdname history_internal
#' @export
forward_duration <- function(df, df_last, current_year, carry_vars) {
    full_join(
        df, 
        select(df_last, .data$cust_id, .data$duration_run, .data$year_last, carry_vars),    
        by = "cust_id", suffix = c("", "_lag")
    ) %>% mutate(  
        duration_run = pmax(.data$duration, .data$duration_run_lag - 1, na.rm = TRUE),  
        year = current_year, 
        year_last = ifelse(.data$duration_run_lag >= 1, .data$year - 1, .data$year_last)
    )
}

#' @rdname history_internal
#' @export
forward_vars <- function(df, carry_vars = NULL) {
    if (is.null(carry_vars)) {
        return(df)
    }
    forward_one <- function(df, var) {
        var_lag <- sym(paste0(var, "_lag"))
        var <- sym(var)
        df %>% mutate(
            !! var := case_when( 
                !is.na(!! var) | .data$duration_run_lag <= 1 ~ !! var, 
                TRUE ~ !! var_lag 
            )
        ) %>% select(- !! var_lag)
    }
    for (i in carry_vars) df <- forward_one(df, i)
    df
}

#' @rdname history_internal
#' @export
make_lapse <- function(df_last, df, yrs_lapse) {
    if (is.null(yrs_lapse)) {
        return(df) 
    }
    df <- df %>%
        filter(.data$duration_run >= 1) %>% 
        mutate(lapse = 0L) %>%
        select(.data$cust_id, .data$lapse)
    
    df_last %>%
        left_join(df, by = "cust_id") %>% 
        mutate(
            lapse = ifelse(is.na(.data$lapse), 1L, .data$lapse),
            lapse = ifelse(.data$year >= yrs_lapse[length(yrs_lapse)], 
                           NA_integer_, .data$lapse)
        )
}

#' @rdname history_internal
#' @export
make_R3 <- function(df, include_R3, yrs) {
    if (!include_R3) {
        return(df)
    }
    df %>% mutate(
        yrs_since = .data$year - .data$year_last,
        R3 = case_when(
            .data$year <= yrs[5] ~ NA_integer_, # 1st 5 yrs shouldn't be identified
            is.na(.data$yrs_since) | .data$yrs_since > 5 ~ 4L, # recruited
            .data$yrs_since == 1 & .data$duration_run_lag > 1 ~ 1L, # carried
            .data$yrs_since == 1 ~ 2L, # renewed
            TRUE ~ 3L # otherwise reactivated
        )
    )
}

#' Internal Functions: Making license history
#' 
#' These functions are intended to be called from \code{\link{make_history}}. 
#' 
#' @param df data frame: current year table
#' @param df_last data frame: previous year table
#' @param current_year numeric: year of current year table
#' @inheritParams make_history
#' @family license history functions
#' @keywords internal
#' @name history_internal
#' @import dplyr
#' @examples
#' # example
NULL

# Deprecated --------------------------------------------------------

#' Make a License History table with 1 row per customer per year
#'
#' The license history table accounts for multi-year/lifetime licenses directly by including
#' a row for every year a license is held. The input data frame (sale_ranked) must 
#' have at least 3 columns (cust_id, year, duration) and should only have 1 record 
#' per customer per year (ensured by running \code{\link{rank_sale}} beforehand).
#' 
#' The output data frame includes the following variables:
#' \itemize{
#' \item \emph{cust_id}
#' \item \emph{year}
#' \item \emph{duration_run}: A duration variable that accounts for multi-year/lifetimes, 
#' also used in downstream R3 calculations.
#' \item \emph{carry_vars}: One or more optional variables to include, 
#' based on the corresponding argument (typically carry_vars = c("month", "res")).
#' For multi-year/lifetimes, carry_vars inherit values from the previous year.
#' }
#' Development Note: This function is largely a wrapper for \code{\link{carry_duration}} 
#' and \code{\link{carry_variables}}
#' 
#' @param sale_ranked data frame: Sales table from which license history will be made
#' @param yrs numeric: Years in sales data (column 'year') from which
#' to create license history
#' @param carry_vars character: additional variables to carry over from previous year
#' (for multi-year and lifetime licenses).
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' sale_unranked <- left_join(sale, lic)
#' sale_ranked <- rank_sale(sale_unranked, first_month = TRUE)
#' history <- sale_ranked %>%
#'     make_lic_history(2008:2019, carry_vars = c("month", "res"))
#' 
#' # check a sample of several customers
#' check_history_samp(history)
make_lic_history <- function(sale_ranked, yrs, carry_vars = NULL) {
    yrs <- prep_yrs(yrs, sale_ranked, "make_lic_history()")
    x <- sale_ranked[c("cust_id", "year", "duration", carry_vars)] %>%
        filter(.data$year %in% yrs)
    split(x, x$year) %>%
        carry_duration(yrs) %>%
        carry_variables(yrs, carry_vars) %>%
        bind_rows() %>% 
        rename(duration_run = duration)
}


#' Internal Function: Carry multi-year/lifetime durations forward
#' 
#' This function is intended to be called from \code{\link{make_lic_history}}.
#' 
#' @param sale_split list: ranked sale table split by year
#' @inheritParams make_lic_history
#' @import dplyr
#' @family deprecated license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(sale, lic)
#' 
#' sale_unranked <- left_join(lic, sale)
#' sale_ranked <- rank_sale(sale_unranked) %>%
#'     select(cust_id, year, duration)
#' 
#' split(sale_ranked, sale_ranked$year) %>%     
#'     carry_duration(2008:2019)
carry_duration <- function(sale_split, yrs) {
    if (any(!c("cust_id", "year", "duration") %in% colnames(sale_split[[1]]))) {
        stop(
            "All 3 variables (cust_id, year, duration) needed ", 
            "for make_lic_history()", call. = FALSE
        ) 
    }
    for (i in 2L:length(yrs)) {
        # carry forward previous year
        sale_split[[i]] <- sale_split[[i-1]] %>%
            filter(.data$duration != 1) %>%
            mutate(lag_duration = .data$duration - 1) %>%
            select(.data$cust_id, .data$lag_duration) %>%
            
            # join to current year & pick highest duration
            full_join(sale_split[[i]], by = "cust_id") %>%
            mutate(
                duration = pmax(.data$duration, .data$lag_duration, na.rm = TRUE) %>%
                    as.integer(),
                year = yrs[i]
            ) %>%
            select(-.data$lag_duration)
    }
    sale_split
}

#' Internal Function: Carry specified variables forward
#' 
#' This function is intended to be called from \code{\link{make_lic_history}}.
#' 
#' @inheritParams make_lic_history
#' @inheritParams carry_duration
#' @import dplyr
#' @family deprecated license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(sale, lic)
#' 
#' sale_unranked <- left_join(lic, sale)
#' sale_ranked <- rank_sale(sale_unranked)
#' 
#' split(sale_ranked, sale_ranked$year) %>%     
#'     carry_duration(2008:2019) %>%
#'     carry_variables(2008:2019, c("month", "res"))
carry_variables <- function(sale_split, yrs, carry_vars) {
    if (is.null(carry_vars)) {
        return(sale_split)
    }
    if (any(!carry_vars %in% colnames(sale_split[[1]]))) {
        stop(
            "All carry_vars (", paste(carry_vars, collapse = ", "), 
            ") needed for make_lic_history()", call. = FALSE
        )
    }
    # replace missings (of var) with value from previous year (if available)
    for (var in carry_vars) {
        for (i in 2L:length(yrs)) {
            sale_split[[i]] <- filter(sale_split[[i]], is.na(.data[[var]])) %>%
                left_join(
                    select(sale_split[[i-1]], .data$cust_id, lastvar = .data[[var]]),  
                    by = "cust_id"
                ) %>%
                mutate(!! var := .data$lastvar) %>%
                select(-.data$lastvar) %>%
                bind_rows(filter(sale_split[[i]], !is.na(.data[[var]])))
        }
    }
    sale_split
}

#' Identify R3 group each year
#'
#' Intended to be called following \code{\link{make_lic_history}}, creates the
#' categorical variable, R3 (1=carried, 2=renewed, 3=reactivated, 4=recruited), 
#' where "retained" consists of carried + renewed (hence R3: retain, reactivate, recruit).
#' 
#' @param lic_history data frame: output of \code{\link{make_lic_history}} 
#' @param yrs numeric: years used for identifying R3 (note that the first 5 years 
#' will all have missing values for R3 output)
#' @param show_summary logical: if TRUE, print a tabular summary that shows R3 category
#' by the number of years since a license was held.
#' @param show_check_vars logical: if TRUE, include output variables used in summary
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' x <- select(history, -R3) %>%
#'     identify_R3(show_summary = TRUE)
#' 
#' # calculate % of license holders who are recruits
#' group_by(x, year, R3) %>% 
#'     summarise(n = n()) %>% 
#'     mutate(pct = n / sum(n)) %>%
#'     filter(R3 == 4, year != 2019)
identify_R3 <- function(
    lic_history, yrs = sort(unique(lic_history$year)),
    show_summary = FALSE, show_check_vars = FALSE
) {
    yrs <- prep_yrs(yrs, lic_history, "identify_R3()")
    if (length(yrs) <= 5) {
        warning(
            "No R3 identification performed: only ", length(yrs), 
            " yrs specified (requires 6 or more)", call. = FALSE
        )
        return(lic_history)
    }
    # get lag year for R3 identification
    dt <- data.table(lic_history)
    cols <- c("year", "duration_run")
    anscols <- paste("lag", cols, sep = "_")
    dt[order(year), (anscols) := shift(.SD, 1), by = cust_id, .SDcols = cols]
    
    lic_history <- as_tibble(dt) %>%
        mutate(
            yrs_since = .data$year - .data$lag_year,
            R3 = case_when(
                .data$year <= yrs[5] ~ NA_integer_, # 1st 5 yrs shouldn't be identified
                is.na(.data$yrs_since) | .data$yrs_since > 5 ~ 4L, # recruited
                .data$yrs_since == 1 & .data$lag_duration_run > 1 ~ 1L, # carried
                .data$yrs_since == 1 ~ 2L, # renewed
                TRUE ~ 3L # otherwise reactivated
            )
        ) 
    if (show_summary) {
        check_identify_R3(lic_history, yrs) %>% print()
    }
    if (!show_check_vars) {
        lic_history <- select(lic_history, -.data$lag_duration_run, -.data$yrs_since)
    }
    select(lic_history, -.data$lag_year)
}

#' Identify lapse group each year
#'
#' This is intended to be called following \code{\link{make_lic_history}} 
#' and codes lapse (0 = renews next year, 1 = lapses next year).
#' 
#' @inheritParams identify_R3
#' @param yrs numeric: years used for identifying lapse (note that the last year will
#' have all missing values for lapse output)
#' @param show_summary logical: if TRUE, include a tabular summary that shows lapse 
#' identification based on how many years until the next license is held.
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' # excluding partial year 2019 since churn is only estimated for full-year results
#' history <- filter(history, year != 2019)
#' 
#' x <- select(history, -lapse) %>%
#'     identify_lapse(show_summary = TRUE) 
#' 
#' # calculate annual churn rate
#' # (note that a given year's churn is based on lapse summary from the previous year)
#' group_by(x, year) %>% 
#'     summarise(mean(lapse)) %>%
#'     mutate(year = year + 1)
identify_lapse <- function(
    lic_history, yrs = sort(unique(lic_history$year)), 
    show_summary = FALSE, show_check_vars = FALSE
) {
    yrs <- prep_yrs(yrs, lic_history, "identify_lapse()")
    
    # issue warning if low final year
    cnt <- count(lic_history, .data$year) %>%
        filter(.data$year %in% yrs) %>%
        mutate(pct_change = (.data$n - lag(.data$n)) / lag(.data$n) * 100)
    last_change <- cnt$pct_change[length(yrs)] %>% round(1)
    
    if (last_change < -20) {
        warning(
            "There is a large drop in the final year specified: ", last_change, 
            "%\n- Please ensure all specified yrs are complete for lapse identification.", 
            call. = FALSE
        )
    }
    # get lead year for lapse identification
    dt <- data.table(lic_history)
    cols <- "year"
    anscols <- paste("lead", cols, sep = "_")
    dt[order(year), (anscols) := shift(.SD, 1, type = "lead"), by = cust_id, .SDcols = cols]
    
    lic_history <- as_tibble(dt) %>%
        mutate(lapse = case_when( 
            .data$year >= max(yrs) ~ NA_integer_, 
            .data$lead_year == (.data$year + 1) ~ 0L, # renewed 
            TRUE ~ 1L # lapsed 
        ))
    if (show_summary) {
        check_identify_lapse(lic_history) %>% print()
    }
    if (!show_check_vars) {
        lic_history <- select(lic_history, -.data$lead_year)
    }
    lic_history
}


# Checking & Summarizing --------------------------------------------------

#' Sample the output of \code{\link{make_lic_history}}
#'
#' View a sample of customers from license history table to check the 
#' year over year dynamics (outputs a list split by customer ID).
#' 
#' @param n_samp numeric: number of customers to view
#' @param buy_min numeric: minimum number of license purchases for customers to include
#' @param buy_max numeric: maximum number of license purchases for customers to include
#' @inheritParams identify_R3
#' @import dplyr
#' @family deprecated license history functions
#' @export
#' @examples
#' data(history)
#' check_history_samp(history, n_samp = 5)
check_history_samp <- function(lic_history, n_samp = 3, buy_min = 3, buy_max = 8) {
    samp <- lic_history %>%
        count(.data$cust_id) %>%
        filter(.data$n >= buy_min, .data$n <= buy_max) %>%
        sample_n(n_samp) %>%
        left_join(lic_history, by = "cust_id") %>%
        select(-.data$n) %>%
        data.frame()
    split(samp, samp$cust_id)
}

#' Internal Function: Check R3 Identification
#'
#' Intended to be run as part of \code{\link{identify_R3}} (where show_summary = TRUE).
#' Produces a count summary of customers by R3, yrs_since, & lag_duration_run.
#' 
#' @inheritParams identify_R3
#' @import dplyr
#' @family deprecated license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' select(history, -R3) %>%
#'     identify_R3(2008:2019, show_check_vars = TRUE) %>%
#'     check_identify_R3(2008:2019)
check_identify_R3 <- function(lic_history, yrs) {
    if (!"yrs_since" %in% names(lic_history)) {
        warning(
            "yrs_since variable needed for check_identify_R3 ", 
            "(see ?identify_R3", call. = FALSE
        )
        return(invisible())
    }
    lic_history %>%
        filter(.data$year > yrs[5]) %>%
        mutate(R3_label = factor_R3(.data$R3)) %>%
        count(.data$R3, .data$R3_label, .data$yrs_since, .data$lag_duration_run) %>%
        data.frame()
}

#' Internal Function: Check lapse identification
#'
#' Intended to be run as part of \code{\link{identify_lapse}} (where show_summary = TRUE).
#' Produces a count summary of customers by lapse and lead_year.
#' 
#' @inheritParams identify_R3
#' @import dplyr
#' @family deprecated license history functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' select(history, -lapse) %>%
#'     filter(year %in% 2008:2018) %>%
#'     identify_lapse(2008:2018, show_check_vars = TRUE) %>%
#'     check_identify_lapse()
check_identify_lapse <- function(lic_history) {
    if (!"lead_year" %in% names(lic_history)) {
        warning(
            "lead_year variable needed for check_identify_lapse ", 
            "(see ?identify_lapse", call. = FALSE
        )
        return(invisible())
    }
    lapse_summary <- lic_history %>%
        mutate(yrs_till_next = case_when(
            is.na(.data$lapse) ~ "C. Next time held: unknown",
            .data$lead_year - .data$year == 1 ~ "A. Next time held: 1yr (i.e., renewed)",
            TRUE ~ "B. Next time held: >1yr/never (i.e., lapsed)"
        )) %>%
        count(.data$lapse, .data$year, .data$yrs_till_next) %>%
        data.frame()
    lapse_summary %>%
        select(-.data$yrs_till_next) %>%
        split(lapse_summary$yrs_till_next)
}
