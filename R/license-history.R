# license history functions

# Preparing Sale ----------------------------------------------------------

#' Filter sales to 1 row per customer per year.
#'
#' Intended to run before \code{\link{make_history}} to ensure that 
#' multi-year/lifetime sales are accounted for.  
#' The default (and intended purpose) is to pick the maximum "duration" value 
#' per customer-year. Optionally, it will also pick the minimum value of month 
#' (intended for mid-year dashboards) if first_month = TRUE. 
#' 
#' @param sale data frame: Input sales data
#' @param rank_var character: name of variable(s) to use for ranking
#' @param grp_var character: name of variable(s) used for grouping
#' @param first_month logical: If TRUE, also ensures the output contains the 
#' earliest month by grp_var (intended for mid-year dashboards)
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @import dplyr
#' @importFrom utils tail
#' @importFrom utils head
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(lic, sale)
#' 
#' sale_unranked <- inner_join(sale, lic)
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
    sale <- data.table(sale)
    setorderv(sale, rank_var) # order ascending
    ranked <- sale
    ranked <- ranked[, tail(.SD, 1), by = grp_var] # pick last
    
    if (first_month) {
        if (!"month" %in% colnames(sale)) {
            warning("No month variable supplied in rank_sale();",
                    " first_month = TRUE ignored.", call. = FALSE)
            return(as_tibble(ranked))
        }
        setorderv(sale, "month")
        sale <- sale[, .(month = head(month, 1)), by = grp_var]
        ranked[, month := NULL]
        ranked[sale, on = c("cust_id", "year"), `:=`(month = i.month)]
    }
    as_tibble(ranked)
}

#' Internal Function: Check years range & sort
#' 
#' Prints a warning any of the specified years aren't included in the dataset and 
#' returns a vector of availabe years, sorted ascending. The sort ensures correct 
#' ordering in subsequent license history calculations, which include iterations
#' by year that would produce incorrect results if not sorted.
#' 
#' This function is intended to be called from \code{\link{make_history}}
#' 
#' @param df data frame: table that contains "year" variable
#' @param func_name character: name of function to print in warning
#' @inheritParams make_history
#' @family internal license history functions
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

#' Make license history table
#'
#' The license history table accounts for multi-year/lifetime licenses by including
#' 1 row for every year a license is held. The input data frame should only have 1 record 
#' per customer-year (ensured by running \code{\link{rank_sale}} beforehand).
#' 
#' The output data frame includes the following variables:
#' \itemize{
#' \item \emph{cust_id}: Customer ID
#' \item \emph{year}: License Year
#' \item \emph{carry_vars}: One or more optional variables to include; ensures 
#' multi-year/lifetime records are complete in future years.
#' \item \emph{duration_run}: A duration variable that accounts for multi-year/lifetimes.
#' \item \emph{lapse}: Lapse next year? (1=Yes, 0=No). Only included if yrs_lapse != NULL
#' \item \emph{R3}: R3 group this year (1=carried, 2=renewed, 3=reactivated, 4=recruited).  
#' Only included if more than 5 years are present.
#' }
#' Development Note: make_history() uses several \code{\link{history_internal}}
#' functions, all of which make use of package data.table for performance, 
#' see \href{https://github.com/Rdatatable/data.table/wiki}{data.table wiki} &
#' \href{https://github.com/Rdatatable/data.table/wiki/Getting-started}{getting started vignette}
#' for more information.
#' 
#' @param sale_ranked data frame: Sales table from which license history will be made; 
#' must include at least 3 variables (cust_id, year, duration)
#' @param yrs numeric: Years in sales data (column 'year') from which
#' to create license history
#' @param carry_vars character: additional variables to carry over from previous year
#' (for multi-year and lifetime licenses).
#' @param yrs_lapse numeric: years to include in lapse calculation (defaults to yrs). 
#' If NULL, lapse will not be calculated (useful for mid-year results)
#' @param show_diagnostics logical: If TRUE, will include intermediate variables in the
#' output dataset, necessary for running checks: \code{\link{history_check}}.
#' 
#' @import dplyr
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @family license history functions
#' @export
#' @examples
#' library(dplyr)
#' data(sale, lic)
#' sale_ranked <- left_join(sale, lic) %>% rank_sale()
#' history <- make_history(sale_ranked, 2008:2018, "res")
#' 
#' # history includes more rows than sale_ranked if multi-year/lifetimes are present
#' left_join(count(history, year), count(sale_ranked, year), by = "year")
make_history <- function(
    sale_ranked, yrs, carry_vars = NULL, yrs_lapse = yrs, show_diagnostics = FALSE
) {
    yrs <- prep_yrs(yrs, sale_ranked, "make_history()")
    slct_cols <- c("cust_id", "year", "duration", carry_vars)
    data_required_vars(sale_ranked, "make_history()", slct_cols, use_error = TRUE)
    
    sale <- data.table(sale_ranked[slct_cols])
    sale[, `:=`(duration_run = duration)]
    
    x <- list()
    x[[1]] <- sale[year == yrs[1]]
    x[[1]][, `:=`(year_last = NA_integer_)]
    
    for (i in 2:length(yrs)) {
        # prep by joining current & last year
        fwd_cols <- c("cust_id", "duration_run", "year_last", carry_vars)
        x[[i]] <- merge(
            sale[year == yrs[i]], 
            x[[i-1]][, ..fwd_cols],
            by = "cust_id", all = TRUE, suffixes = c("", "_lag")
        )
        forward_duration(x[[i]], yrs[i])
        if (!is.null(carry_vars)) forward_vars(x[[i]], carry_vars)
        if (yrs[i] %in% yrs_lapse) make_lapse(x[[i-1]], x[[i]])
    }
    x <- x %>% 
        # keeping only records that represent a held license
        lapply(function(df) df[!is.na(duration_run) & duration_run > 0]) %>%
        rbindlist(fill = TRUE)
    if (length(yrs) > 5) make_R3(x, yrs)
    if (!show_diagnostics) {
        x[, c("duration_run_lag", "duration", "year_last", "yrs_since") := NULL]
    }
    x[, duration_run := as.integer(duration_run)] # for consistency
    as_tibble(x)
}

#' Internal Functions: Making license history
#' 
#' These functions are only to be called from \code{\link{make_history}}. 
#' They work by side-effects; modifying the input data.table by reference.
#' 
#' @param dt data.table: current year table
#' @param dt_last data.table: previous year table
#' @param current_year numeric: year of current year table
#' @inheritParams make_history
#' @family internal license history functions
#' @keywords internal
#' @name history_internal
#' @rawNamespace import(data.table, except = c(first, between, last))
NULL

#' @rdname history_internal
#' @export
forward_duration <- function(dt, current_year) {
    dt[, `:=`(
        duration_run = pmax(duration, duration_run_lag - 1, na.rm = TRUE),
        year = current_year
    )]
    dt[, `:=`(year_last = ifelse(duration_run_lag >= 1, year - 1, year_last))]
}

#' @rdname history_internal
#' @export
forward_vars <- function(dt, carry_vars) {
    for (var in carry_vars) {
        var_lag <- paste0(var, "_lag")
        dt[, (var) := dplyr::case_when(
            !is.na(get(var)) | duration_run_lag <= 1 ~ get(var),
            TRUE ~ get(var_lag)
        )]
        dt[, (var_lag) := NULL]
    }
}

#' @rdname history_internal
#' @export
make_lapse <- function(dt_last, dt) {
    lapse_ref <- dt[duration_run >= 1, .(cust_id, lapse = 0L)]
    dt_last[lapse_ref, on = "cust_id", `:=`(lapse = i.lapse)]
    dt_last[, `:=`(lapse = ifelse(is.na(lapse), 1L, lapse))]
}

#' @rdname history_internal
#' @export
make_R3 <- function(dt, yrs) {
    dt[, `:=`(
        yrs_since = year - year_last
    )]
    dt[, `:=`(
        R3 = dplyr::case_when(
            year <= yrs[5] ~ NA_integer_, # 1st 5 yrs shouldn't be identified
            is.na(yrs_since) | yrs_since > 5 ~ 4L, # recruited
            yrs_since == 1 & duration_run_lag > 1 ~ 1L, # carried
            yrs_since == 1 ~ 2L, # renewed
            TRUE ~ 3L # otherwise reactivated
        )
    )]
}

# Checking & Summarizing --------------------------------------------------

#' Checking license history
#' 
#' These functions can be called following \code{\link{make_history}} with 
#' show_diagnostics = TRUE.
#' \itemize{
#' \item \emph{check_history_sample}: View a sample of customers from history table 
#' to check year over year dynamics (outputs a list split by customer ID).
#' \item \emph{check_R3}: Produce a count summary of customers by R3, yrs_since, 
#' & duration_run_lag.
#' \item \emph{check_lapse}: Produces a count summary of customers by lapse and lead_year.
#' }
#' 
#' @param history data frame: license history table
#' @param n_samp numeric: number of customers to view
#' @param buy_min numeric: minimum number of license purchases for customers to include
#' @param buy_max numeric: maximum number of license purchases for customers to include
#' @inheritParams make_history
#' @family license history functions
#' @import dplyr
#' @rawNamespace import(data.table, except = c(first, between, last))
#' @name history_check
#' @examples 
#' library(dplyr)
#' data(sale, lic)
#' yrs <- 2008:2018
#' 
#' history <- inner_join(sale, lic) %>% 
#'     rank_sale() %>%
#'     make_history(yrs, "res", show_diagnostics = TRUE)
#' 
#' check_history_sample(history)
#' check_R3(history, 2008:2018)
#' check_lapse(history)
NULL

#' @rdname history_check
#' @export
check_history_sample <- function(history, n_samp = 3, buy_min = 3, buy_max = 8) {
    show_cols <- c("cust_id", "year", "duration_run", "lapse", "R3", "res", "month")
    output_cols <- dplyr::intersect(colnames(history), show_cols)
    
    samp <- history %>%
        count(.data$cust_id) %>%
        filter(.data$n >= buy_min, .data$n <= buy_max) %>%
        sample_n(n_samp) %>%
        left_join(history, by = "cust_id") %>%
        data.frame()
    samp <- samp[output_cols]
    split(samp, samp$cust_id)
}

#' @rdname history_check
#' @export
check_R3 <- function(history, yrs) {
    if (!"yrs_since" %in% names(history)) {
        warning(
            "yrs_since variable needed for check_R3 ", 
            "(see ?make_history(show_diagnostics = TRUE)", call. = FALSE
        )
        return(invisible())
    }
    history %>%
        filter(.data$year > yrs[5]) %>%
        mutate(R3_label = factor_R3(.data$R3)) %>%
        count(.data$R3, .data$R3_label, .data$yrs_since, .data$duration_run_lag) %>%
        data.frame()
}

#' @rdname history_check
#' @export
check_lapse <- function(history) {
    # get lead year for checking
    dt <- data.table(history)
    dt[order(year), lead_year := shift(year, 1, type = "lead"), by = cust_id]
    dt[, yrs_till_next := case_when(
        is.na(lapse) ~ "C. Next time held: unknown",
        lead_year - year == 1 ~ "A. Next time held: 1yr (i.e., renewed)",
        TRUE ~ "B. Next time held: >1yr/never (i.e., lapsed)"
    )]
    
    lapse_summary <- dt %>% 
        count(.data$lapse, .data$year, .data$yrs_till_next) %>%
        data.frame()
    lapse_summary %>%
        select(-.data$yrs_till_next) %>%
        split(lapse_summary$yrs_till_next)
}
