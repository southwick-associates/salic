# functions for building dashboard summary data


# Estimate ------------------------------------------------------------

#' Helper function: print a warning to flag records
#' 
#' The warning is only printed if the input data frame has more than one row 
#' (i.e., only flagged rows should be passed to warn())
#'
#' @param flagged data frame: table containing flagged rows
#' @param msg character: message to be printed in warming
#' @family dashboard functions
#' @keywords internal
#' @export
#' @importFrom utils capture.output
#' @examples
#' x <- data.frame(x = 1:2, val = c("careful", "a bit high"))
#' warn(x, "I'm a warning")
warn <- function(flagged, msg) {
    flagged <- data.frame(flagged)
    if (nrow(flagged) > 0) {
        warning(msg, "\n", paste(capture.output(print(flagged)), collapse = "\n"), 
                call. = FALSE)
    } 
}

#' Perform an action (e.g., warning, error) to flag records  (for internal salic use)
#' 
#' The action is only triggered if any values in df[[test_variable]] exceed
#' test_threshold. Intended for use in calculating dashboard metrics
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
#' x <- data.frame(tot = "All", year = 2008:2018, part = rnorm(11, 10000, sd = 1000))
#' x <- mutate(x, pct_change = (part - lag(part)) / lag(part) * 100)
#' check_threshold(x, 10)
#' 
#' # this will produce an error
#' # check_threshold(x, 10, action = function(...) stop(..., call. = FALSE))
check_threshold <- function(
    df, test_threshold, test_variable = "pct_change", 
    action = function(...) warning(..., call. = FALSE),
    msg = paste("Threshold of", test_threshold, "for", test_variable, "exceeded:")
) {
    flagged <- filter(df, abs(.data[[test_variable]]) > test_threshold) %>%
        data.frame()
    if (nrow(flagged) > 0) {
        action(msg, "\n", paste(capture.output(print(flagged)), collapse = "\n"))
    } 
}


#' Estimate participants by year from license history
#' 
#' This function requires a correctly formated history table (see \code{\link{history}}).
#' It produces a simple count of records per year, optionally by segment, 
#' & runs a validation test: pct. change per year. A large pct. change in a year
#' can often indicate a problem.
#' 
#' @param history data frame: input license history table
#' @param segment character: defaults to "tot", which indicates no segmentation.
#' Alternatively specifiy other license history variables ("res", "sex", etc.)
#' @param test_threshold numeric: threshold in whole number percentage points 
#' for pct. change per year. A warning will be printed if the absolute value
#' of the change for any year exceeds the threshold.
#' @param include_test_stat logical: If TRUE, the output table will include
#' a variable holding the test statistic for each row.
#' @param suppress_warning logical: If TRUE, no test warning will be displayed 
#' (even if threshold is exceeded). Test statistics can still be included by 
#' setting include_test_stat = TRUE.
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
#' est_part(history, include_test_stat = TRUE)
#' 
#' # fix by dropping partial year
#' history <- filter(history, year != 2019)
#' est_part(history)
#' 
#' # by age category
#' est_part(history, "agecat")
#' est_part(history, "agecat", test_threshold = 35)
#' 
#' # apply over multiple segments
#' segs <- c("tot", "res", "sex", "agecat")
#' sapply(segs, function(x) est_part(history, x), simplify = FALSE)
#' 
#' # specify test thesholds by segment
#' tests <- c(tot = 20, res = 40, sex = 30, agecat = 40)
#' sapply(segs, function(x) est_part(history, x, tests[x]), simplify = FALSE)
est_part <- function(
    history, segment = "tot", test_threshold = 30, include_test_stat = FALSE,
    suppress_warning = FALSE
) {
    if (segment == "tot") {
        history <- mutate(history, tot = "All") # for group_by()
    } else {
        # need to drop records where segment value is missing
        history <- filter(history, !is.na(!! as.name(segment)))
    }
    out <- history %>%
        group_by_at(c(segment, "year")) %>%
        summarise(part = n()) %>%
        mutate(pct_change = (part - lag(part)) / lag(part) * 100) %>%
        ungroup()
    
    if (!suppress_warning) {
        filter(out, abs(pct_change) > test_threshold) %>%
            warn(paste0("Annual % change beyond ", test_threshold, "% in at least one year"))
    }
    if (!include_test_stat) out <- select(out, -pct_change) 
    out
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
#' x <- filter(history, year != 2019) %>%
#'     label_categories()
#' 
#' # demonstrate the need for scaling
#' part_total <- est_part(x)
#' part_segment <- est_part(x, "sex", test_threshold = 40)
#' sum(part_segment$part) == sum(part_total$part)
#' 
#' # perform scaling
#' part_segment <- scaleup_part(part_segment, part_total)
#' sum(part_segment$part) == sum(part_total$part)
#' 
#' # throw an error by making the test threshold more strict
#' scaleup_part(part_segment, part_total, test_threshold = 0.1)
scaleup_part <- function(
    part_segment, part_total, test_threshold = 10, include_test_stat = FALSE
) {
    if (sum(part_segment$part) == sum(part_total$part)) {
        return # scaling not needed
    }
    # compute scale factor by comparing to totals
    part_total <- semi_join(part_total, part_segment, by = "year")
    compare <- part_segment %>%
        group_by(year) %>%
        summarise(part_segment = sum(part)) %>%
        left_join(select(part_total, year, part), by = "year") %>% 
        mutate( 
            pct_na = (part - part_segment) / part * 100,  
            scale_factor = part / part_segment 
        )
    # a high % missing may call missing at random assumption into question
    check_threshold(
        compare, test_threshold, "pct_na",
        action = function(...) stop(..., call. = FALSE)
    )
    # scale to match the total - assumes missing at random (otherwise introduces bias)
    out <- part_segment %>%
        left_join(select(compare, year, pct_na, scale_factor), by = "year") %>%
        mutate(part = round(part * scale_factor, 0) %>% as.integer()) %>%
        select(-scale_factor)
    
    # a final check of the scaled total
    # TODO - might not need this if function is tested thoroughly, will leave for now
    diff <- abs(sum(out$part) - sum(part_total$part))
    if (diff > 50) { # allows for a small amount of rounding error
        warning("Something might have gone wrong in scaling since the segment sum of ",
                sum(out$part), " is different than the total of ", sum(part_total$part))
    }
    if (!include_test_stat) out <- select(out, -pct_na) 
    out
}
