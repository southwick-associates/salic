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
#' @param include_test_value logical: If TRUE, the output table will include
#' a variable holding the test value (pct. change per year) for each row.
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
#' est_part(history, include_test_value = TRUE)
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
    history, segment = "tot", test_threshold = 30, include_test_value = FALSE
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
    
    filter(out, abs(pct_change) > test_threshold) %>%
        warn(paste0("Annual % change beyond ", test_threshold, "% in at least one year"))
    
    if (!include_test_value) out <- select(out, -pct_change) 
    out
}

