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
#' part <- sapply(segs, function(x) est_part(history, x, tests[x]), simplify = FALSE)
#' 
#' # scale segments to total
#' filter(history, is.na(sex)) # very small number of missing values
#' part_scaled <- lapply(part, function(x) scaleup_part(x, part$tot))
#' left_join(part$sex, part_scaled$sex, by = c("sex", "year")) %>%
#'     filter(part.x != part.y)
#' 
#' # new recruits
#' history_new <- filter(history, !is.na(R3), R3 == "Recruit")
#' part_new <- sapply(segs, function(x) est_part(history_new, x, 50), simplify = FALSE)
#' part_new <- lapply(part_new, function(x) scaleup_part(x, part_new$tot))
est_part <- function(
    history, segment = "tot", test_threshold = 30, show_test_stat = FALSE,
    suppress_warning = FALSE, outvar = "part"
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

#' Estimate new participants by year from license history
#' 
#' This function requires a correctly formated history table (see \code{\link{history}}).
#' It produces a simple count of records per year (filtered by R3 to exclude non-recruits),
#' optionally by segment. It also runs a validation test: pct change per year.
#' 
#' @inheritParams est_part
#' @param recruit_values character: Values of R3 variable that indicate a recruit
#' (passed to \code{\link[dplyr]{filter}} to exclude non-recruits)
#' @family dashboard functions
#' @import dplyr
#' @export
#' @examples
#' library(dplyr)
#' data(history)
#' 
#' history <- filter(history, year != 2019)
#' est_recruit(history)
#' 
#' # works whether or not R3 variable has been labelled
#' count(history, R3)
#' history <- label_categories(history)
#' count(history, R3)
#' est_recruit(history)
est_recruit <- function(
    history, segment = "tot", test_threshold = 30, show_test_stat = FALSE,
    suppress_warning = FALSE, outvar = "recruit", 
    recruit_values = c("Recruit", "4")
) {
    if (!"R3" %in% colnames(history)) {
        stop("R3 variable is needed to identify new participants", call. = FALSE)
    }
    history <- filter(history, .data$R3 %in% recruit_values)
    
    if (nrow(history) == 0) {
        stop(paste0("No recruits (R3 = ", recruit_values, ") in input data frame"))
    }
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
    suppress_warning = FALSE
) {
    if (segment == "tot") {
        history <- mutate(history, tot = "All")
    } else {
        history <- filter(history, !is.na(!! as.name(segment)))
    }
    # churn is simply lapse % per year
    out <- history %>%
        group_by_at(c(segment, "year")) %>%
        summarise(churn = mean(lapse)) %>%
        mutate(
            change = .data$churn - lag(.data$churn),
            pct_change = .data$change / lag(.data$churn) * 100
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
#' # this throws an error by making the test threshold more strict
#' part_segment <- est_part(x, "sex", test_threshold = 40)
#' # scaleup_part(part_segment, part_total, test_threshold = 0.1)
scaleup_part <- function(
    part_segment, part_total, test_threshold = 10, show_test_stat = FALSE,
    outvar = "part"
) {
    if (sum(part_segment$part) == sum(part_total$part)) {
        return # scaling not needed
    }
    # compute scale factor
    part_total <- semi_join(part_total, part_segment, by = "year")
    compare <- part_segment %>%
        group_by(year) %>%
        summarise(part_segment = sum(.data[[outvar]])) %>%
        left_join(select(part_total, .data$year, .data[[outvar]]), by = "year") %>% 
        mutate( 
            pct_na = (.data[[outvar]] - part_segment) / .data[[outvar]] * 100,  
            scale_factor = .data[[outvar]] / part_segment 
        )
    check_threshold(
        compare, test_threshold, "pct_na",
        action = function(...) stop(..., call. = FALSE)
    )
    # scale to match the total
    out <- part_segment %>%
        left_join(select(compare, .data$year, .data$pct_na, .data$scale_factor), 
                  by = "year") %>%
        mutate(!! outvar := as.integer(round(.data[[outvar]] * .data$scale_factor, 0))) %>%
        select(-.data$scale_factor)
    
    # a final check of the scaled total
    # TODO - might not need this if function is tested thoroughly, will leave for now
    #      - probably better to consider what problem it catches and write a test
    diff <- abs(sum(out$part) - sum(part_total$part))
    if (diff > 50) { # allows for a small amount of rounding error
        warning("Something might have gone wrong in scaling since the segment sum of ",
                sum(out$part), " is different than the total of ", sum(part_total$part))
    }
    if (!show_test_stat) out <- select(out, -.data$pct_na) 
    out
}

