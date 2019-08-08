# functions for building dashboard summary data


# Estimate ------------------------------------------------------------

#' Internal Function: Perform an action (e.g., warning, error) to flag records
#' 
#' The action is only triggered if any values in df[[test_variable]] exceed
#' test_threshold. Intended for use in calculating dashboard metrics.
#'
#' @param df data frame: table containing statistic to check
#' @param test_threshold numeric: if exceeded, while produce warning
#' @param test_variable character: Name of variable in df that contains
#' the test statistic
#' @param action function: function call to perform if threshold is exceeded
#' (intended to be \code{\link[base]{warning}} or \code{\link[base]{stop}}).
#' @param msg character: message to be printed in action
#' @family dashboard functions
#' @keywords internal
#' @import dplyr
#' @export
#' @importFrom utils capture.output
#' @examples
#' library(dplyr)
#' 
#' # produce a warning
#' x <- data.frame(tot = "All", year = 2008:2018, part = rnorm(11, 1000, sd = 100))
#' x <- mutate(x, pct_change = (part - lag(part)) / lag(part) * 100)
#' check_threshold(x, 5)
#' 
#' # this will produce an error
#' # check_threshold(x, 5, action = function(...) stop(..., call. = FALSE))
check_threshold <- function(
    df, test_threshold, test_variable = "pct_change", 
    action = function(...) warning(..., call. = FALSE),
    msg = paste("Threshold of", test_threshold, "for", test_variable, "exceeded:")
) {
    flagged <- filter(df, abs(.data[[test_variable]]) > test_threshold) %>%
        data.frame()
    if (nrow(flagged) > 0) {
        action(msg, "\n", paste(capture.output(print(flagged)), collapse = "\n"), "\n")
    } 
}


#' Estimate participants by year from license history
#' 
#' This function requires a correctly formated history table (see \code{\link{history}}).
#' It produces a simple count of records per year, optionally by segment, 
#' & runs a validation test: pct change per year.
#' 
#' @param history data frame: input license history table
#' @param segment character: defaults to "tot", which indicates no segmentation.
#' Alternatively specifiy other license history variables ("res", "sex", etc.)
#' @param test_threshold numeric: threshold in whole number percentage points 
#' for pct change per year. A warning will be printed if the absolute value
#' of the change for any year exceeds the threshold.
#' @param show_test_stat logical: If TRUE, the output table will include
#' a variable holding the test statistic for each row.
#' @param suppress_warning logical: If TRUE, no test warning will be displayed 
#' (even if threshold is exceeded). Test statistics can still be included by 
#' setting show_test_stat = TRUE.
#' @param outvar character: name of variable that stores metric
#' @return Returns a data frame with 3 variables (segment, "year", outvar), and
#' optionally with 2 extra variables ("change", "pct_change") if show_test_stat = TRUE
#' @family dashboard functions
#' @import dplyr
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' history <- history %>%
#'     label_categories() %>%
#'     recode_agecat() %>%
#'     filter(!agecat %in% c("0-17", "65+"))
#' 
#' # a flag will be raised since 2019 is a partial year
#' est_part(history, show_test_stat = TRUE)
#' 
#' # fix by dropping partial year
#' history <- filter(history, year != 2019)
#' est_part(history)
#' 
#' ### for new recruits
#' history_new <- filter(history, !is.na(R3), R3 == "Recruit")
#' est_recruit(history_new)
#' 
#' ### by segment
#' est_part(history, "agecat")
#' est_part(history, "agecat", test_threshold = 15) # produce a warning
#' 
#' # apply over multiple segments
#' segs <- c("tot", "res", "sex", "agecat")
#' sapply(segs, function(x) est_part(history, x), simplify = FALSE)
#' 
#' # specify test thesholds by segment
#' tests <- c(tot = 20, res = 40, sex = 30, agecat = 40)
#' part <- sapply(segs, function(x) est_part(history, x, tests[x]), simplify = FALSE)
#' 
#' ### scaleup_part()
#' # missing values can cause problem with counts by segment
#' filter(history, is.na(sex))
#' group_by(part$sex, year) %>% 
#'     summarise(part_sex = sum(participants)) %>%
#'     left_join(select(part$tot, year, participants), by = "year")
#' 
#' # fix by scaling segments to total
#' # reasonable assumption if missing at random (less so otherwise)
#' part_scaled <- lapply(part, function(x) scaleup_part(x, part$tot))
#' group_by(part_scaled$sex, year) %>% 
#'     summarise(part_sex = sum(participants)) %>%
#'     left_join(select(part_scaled$tot, year, participants), by = "year")
est_part <- function(
    history, segment = "tot", test_threshold = 20, show_test_stat = FALSE,
    suppress_warning = FALSE, outvar = "participants"
) {
    if (segment == "tot") {
        history <- mutate(history, tot = "All") # for group_by()
    } else {
        # need to drop records where segment value is missing
        history <- filter(history, !is.na(!! as.name(segment)))
    }
    out <- history %>%
        group_by_at(c(segment, "year")) %>%
        summarise(!! outvar := n()) %>%
        mutate(
            change = .data[[outvar]] - lag(.data[[outvar]]),
            pct_change = .data$change / lag(.data[[outvar]]) * 100
        ) %>%
        ungroup()
    if (!suppress_warning) check_threshold(out, test_threshold)
    if (!show_test_stat) out <- select(out, -.data$change, -.data$pct_change) 
    out
}

# convenience function for recruit participation
#' @rdname est_part
#' @export
est_recruit <- function(
    history, segment = "tot", test_threshold = 35, show_test_stat = FALSE,
    suppress_warning = FALSE, outvar = "recruits"
) {
    est_part(history, segment, test_threshold, show_test_stat, 
             suppress_warning, outvar)
}

#' Estimate churn by year from license history
#' 
#' This function requires a correctly formated history table (see \code{\link{history}}).
#' It runs a mean of the lapse value (per year), optionally by segment (and also shifts 
#' year forward by 1 so that churn in current year reflects lapse pct from last year). 
#' It also runs a validation test: pct change per year.
#' @inheritParams est_part
#' @family dashboard functions
#' @import dplyr
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' history <- history %>%
#'     label_categories() %>%
#'     recode_agecat() %>%
#'     filter(!agecat %in% c("0-17", "65+"), year != 2019)
#' est_churn(history)
#' 
#' # apply across all segments
#' segs <- c("tot", "res", "sex", "agecat")
#' churn <- sapply(segs, function(x) est_churn(history, x), simplify = FALSE)
est_churn <- function(
    history, segment = "tot", test_threshold = 30, show_test_stat = FALSE,
    suppress_warning = FALSE, outvar = "churn"
) {
    if (segment == "tot") {
        history <- mutate(history, tot = "All")
    } else {
        history <- filter(history, !is.na(!! as.name(segment)))
    }
    # churn is simply lapse % per year
    out <- history %>%
        group_by_at(c(segment, "year")) %>%
        summarise(!! outvar := mean(lapse)) %>%
        mutate(
            change = .data[[outvar]] - lag(.data[[outvar]]),
            pct_change = .data$change / lag(.data[[outvar]]) * 100
        ) %>%
        ungroup()
    
    # shifting one year forward so current year always has a value
    # hence churn = % of last years buyers who didn't renew this year
    lastyr <- max(out$year)
    out <- mutate(out, year = year + 1) %>%
        filter(year != lastyr + 1)
    if (!suppress_warning) check_threshold(out, test_threshold)
    if (!show_test_stat) out <- select(out, -.data$change, -.data$pct_change) 
    ungroup(out)
}


#' Scale segmented participation counts to total (if needed)
#' 
#' This scaling accounts for missing values in segments, scaling up all counts
#' to ensure the sum matches the total count. It expects 2 tables as input, both
#' produced by \code{\link{est_part}}. If no scaling is needed (i.e., sum(part_segment$part)
#' == sum(part_total$part)) the function will simply return the input df.
#' 
#' @param part_segment data frame: A segmented  participation table
#' produced by \code{\link{est_part}} (e.g., with segment argument set to "res")
#' @param part_total data frame: An overall participation table produced by
#' \code{\link{est_part}}
#' @param test_threshold numeric: threshold in whole number percentage points 
#' which defines the upper limit of acceptable proportion of missing values for 
#' the segment. The function will stop with an error if this threshold
#' is exceeded. Relaxing the threshold can allow the check to pass, but use this
#' with caution since a high percentage of missing values might suggests that 
#' the breakouts aren't representative (e.g., if not missing at random).
#' @inheritParams est_part
#' @family dashboard functions
#' @import dplyr
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' history <- filter(history, year != 2019) %>%
#'     label_categories()
#' 
#' # demonstrate the need for scaling
#' part_total <- est_part(history)
#' part_segment <- est_part(history, "sex", test_threshold = 40)
#' sum(part_segment$participants) == sum(part_total$participants)
#' 
#' # perform scaling
#' part_segment <- scaleup_part(part_segment, part_total)
#' sum(part_segment$participants) == sum(part_total$participants)
#' 
#' # making test threshold more strict
#' part_segment <- est_part(history, "sex")
#' # scaleup_part(part_segment, part_total, test_threshold = 3) # throws error if run
#' 
#' # new recruits
#' history_new <- filter(history, R3 == "Recruit")
#' part_total <- est_recruit(history_new, "tot")
#' part_segment <- est_recruit(history_new, "sex")
#' scaleup_recruit(part_segment, part_total)
scaleup_part <- function(
    part_segment, part_total, test_threshold = 10, show_test_stat = FALSE,
    outvar = "participants"
) {
    # TODO this could potentially be part of defined data format checks
    # and placed at the top of these functions that have very strict format rules
    if (!outvar %in% colnames(part_segment) | !outvar %in% colnames(part_segment)) {
        stop("Missing '", outvar, "' from at least one of the input tables", call. = FALSE)
    }
    if (sum(part_segment[[outvar]]) == sum(part_total[[outvar]])) {
        return # scaling not needed
    }
    if (nrow(part_total) > nrow(part_segment)) {
        warning("Argument part_segment has fewer rows than part_total.\n",
                "Maybe you mixed up the arguments?", call. = FALSE)
    }
    # compute scale factor
    part_total2 <- part_total %>%
        semi_join(part_segment, by = "year") %>%
        group_by(year) %>%
        summarise(total = sum(.data[[outvar]]))
    part_segment2 <- part_segment %>%
        group_by(year) %>%
        summarise(segment = sum(.data[[outvar]]))
    compare <- part_segment2 %>%
        left_join(part_total2, by = "year") %>%
        mutate( 
            total_na = .data$total - .data$segment,
            pct_na = .data$total_na / .data$total * 100,  
            scale_factor = .data$total / .data$segment 
        )
    check_threshold(
        compare, test_threshold, "pct_na",
        action = function(...) stop(..., call. = FALSE)
    )
    # scale to match the total
    compare <- select(compare, .data$year, .data$pct_na, .data$scale_factor)
    out <- part_segment %>%
        left_join(compare, by = "year") %>%
        mutate(!! outvar := as.integer(round(.data[[outvar]] * .data$scale_factor, 0))) %>%
        select(-.data$scale_factor)
    
    # a final check of the scaled total
    # TODO - might not need this if function is tested thoroughly, will leave for now
    #      - probably better to consider what problem it catches and write a test
    diff <- abs(sum(out[[outvar]]) - sum(part_total2$total))
    if (diff > 50) { # allows for a small amount of rounding error
        warning("Something might have gone wrong in scaling since the segment sum of ",
                sum(out[[outvar]]), " is different than the total of ", 
                sum(part_total2$total))
    }
    if (!show_test_stat) out <- select(out, -.data$pct_na) 
    out
}

# convenience function for recruit scaleup
#' @rdname scaleup_part
#' @export
scaleup_recruit <- function(
    part_segment, part_total, test_threshold = 10, show_test_stat = FALSE,
    outvar = "recruits"
) {
    scaleup_part(part_segment, part_total, test_threshold, 
                 show_test_stat, outvar)
}


# Data Format -------------------------------------------------------------

#' Format estimated metrics for input to Dashboard
#' 
#' This function expects a data frame produced by an salic estimation function (
#' \code{\link{est_part}}, \code{\link{est_recruit}}, \code{\link{est_churn}}).
#' It returns a data frame with additional formatting that allows stacking all results
#' into a single table.
#' 
#' @param df data frame: Input table (3 variables) with estimated metrics
#' @param timeframe character: value to store in the 'timeframe' variable of 
#' the output (e.g, 'full-year', 'mid-year')
#' @param group character: value to sore in the 'group' variable of the 
#' output (e.g., 'all_sports', 'fish', 'hunt')
#' @param rename_input character: generic names for input variables as they
#' will appear in the output
#' @family dashboard functions
#' @export
#' @examples
#' library(dplyr)
#' data(metrics)
#' 
#' # format a table
#' metrics$participants$res
#' x <- format_result(metrics$participants$res, "full-year", "all_sports")
#' x
#' 
#' # combine formatted tables
#' y <- format_result(metrics$participants$tot, "full-year", "all_sports")
#' bind_rows(y, x)
#' 
#' # apply formatting across all segments
#' x <- lapply(metrics$participants, function(x) format_result(x, "full-year", "sports"))
#' bind_rows(x)
#'    
#' # apply across all metrics & segments
#' bind_rows(
#'     lapply(metrics$participants, function(x) format_result(x, "full-year", "sports")),
#'     lapply(metrics$recruits, function(x) format_result(x, "full-year", "sports")),
#'     lapply(metrics$churn, function(x) format_result(x, "full-year", "sports"))
#' )
format_result <- function(
    df, timeframe, group, rename_input = c("category", "year", "value")
) {
    # expecting exactly 3 columns in the input data frame
    out <- df
    names(out) <- rename_input
    
    # stored as input variable names >> placed in output variable values
    segment <- colnames(df)[1]
    metric <- colnames(df)[3]
    
    # adding variables to represent structure in a single table
    out$segment <- segment
    out$metric <- metric
    out$timeframe <- timeframe
    out$group <- group
    out$category <- as.character(out$category)
    
    # modify segment names
    out <- out %>% dplyr::mutate(
        segment = dplyr::case_when(
            segment == "tot" ~ "All",
            segment == "res" ~ "Residency",
            segment == "sex" ~ "Gender",
            segment == "agecat" ~ "Age",
            segment == "county" ~ "County"
        )
    )
    out %>% dplyr::select(
        .data$timeframe, .data$group, .data$segment, .data$year,  
        .data$category, .data$metric, .data$value
    )
}
