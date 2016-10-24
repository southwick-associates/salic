# functions for checking license data
# can't really include sample data in package with personal info (names)

# todo - probably drop check_cust2

#' Check Customer IDs
#' @param cust data frame: Customer table
#' @param sale data frame: Sales table with cust_id & issue_date
#' @param count numeric: Number of records to show
#' @import dplyr
#' @family functions for checking license data
#' @export
#' @examples
#' check_cust()
check_cust <- function(cust, sale, count = 10000) {
    catf("---CHECK CUSTOMER ID---")
    catf("Overall counts of unique cust_id vs. unique name-dob:")
    check_cust1(cust) %>% printf()
    check_cust2(cust, sale, count)
}

# overall counts
check_cust1 <- function(cust) {
    cust_ids <- distinct(cust, cust_id) %>% nrow()
    cust_name_dobs <- distinct(cust, first, last, birth_month, birth_day,
                               birth_year) %>% nrow()
    out <- data.frame(cust_ids, cust_name_dobs)
    out$pct_diff <- (out$cust_ids - out$cust_name_dobs) / out$cust_ids * 100
    out$pct_diff <- paste0(as.character(out$pct_diff), "%")
    out
}
# showing first and last records
# might make sense to show middle records (more common names)
check_cust2 <- function(cust, sale, count) {
    z <- left_join(cust, sale, by = "cust_id") %>%
        filter(!is.na(first), !is.na(last)) %>%
        arrange(last, first, birth_year, year)
    # start in ~75% of the customer names (around R or S likely)
    row_start <- round(nrow(z) * 3/4, 0)
    row_end <- row_start + count
    z[row_start:row_end, ]
}

#' Check for duplicates in a table
#'
#' Note - can probably include a check for duplicate counts, or make another
#' function for this - see SD prep-cust code
#' @param x data frame: Table holding records
#' intended to speed up the operation
#' @import dplyr
#' @family functions for checking license data
#' @export
#' @examples
#' library(dplyr)
#' cust <- data.frame(id = 1:3, nm = c("dan k","dan k","dan r"))
#' x <- select(cust, id, nm) %>% distinct()
#' select(x, nm) %>% check_dups()
check_dups <- function(x) {
    all_records <- summarise(x, n()) #%>% collect()
    # this might be slow
    unique_records <- distinct(x) %>% summarise(n()) #%>% collect()
    out <- bind_cols(data.frame(all_records), data.frame(unique_records))
    names(out) <- c("all", "unique")
    mutate(out, pct_dup = pct_round((all - unique) / unique, 3))
}

#' Sample duplicates in a table
#'
#' Intended to be run following get_dups
#' @param dup_all data frame: duplicate values joined to customer table,
#' output of get_dups
#' @inheritParams check_dups
#' @inheritParams get_dups
#' @import dplyr
#' @family functions for checking license data
#' @export
#' @examples
#' library(dplyr)
#' cust <- data.frame(id = 100001:100003, nm = c("dan k","dan k","dan r"))
#' x <- select(cust, id, nm) %>% distinct()
#' select(x, nm) %>% check_dups()
#' get_dups(x, cust, "nm") %>%
#'     samp_dups("nm")
samp_dups <- function(dup_all, id, size = 10) {
    dup <- select(dup_all, dup_id) %>% distinct()
    if (nrow(dup) > size) {
        sample_n(dup, 10) %>%
            left_join(dup_all, by = "dup_id") %>%
            arrange_(id)
    } else if (nrow(dup) > 0) {
        left_join(dup, dup_all, by = "dup_id") %>%
            arrange_(id)
    } else {
        paste("No duplicates for", paste(id, collapse = "-")) %>% print()
    }
}

#' Get duplicates based on an ID
#'
#' Note - can probably remove "full" parameter
#' @param x data frame: Table holding distinct observations
#' @param full data frame: Table holding all observations
#' @param id character: Variable(s) representing identifier to check
#' @import dplyr
#' @family functions for checking license data
#' @export
#' @examples
#' library(dplyr)
#' cust <- data.frame(id = 100001:100003, nm = c("dan k","dan k","dan r"))
#' x <- select(cust, id, nm) %>% distinct()
#' select(x, nm) %>% check_dups()
#' get_dups(x, cust, "nm")
get_dups <- function(x, full, id) {
    dup <- count_(x, id) %>% ungroup() %>% filter(n > 1)
    dup$dup_id <- as.integer(row.names(dup))
    left_join(dup, full, by = id)
}

#' Collect duplicates in a table
#' @param x data frame: Table holding records
#' @import dplyr
#' @family functions for checking license data
#' @export
#' @examples
#' # this example requires a dataset with names populated
#' library(dplyr)
#' x <- select(cust, cust_id, dob, last, first) %>% distinct()
#' dup <- select(x, dob, last, first) %>% show_dups()
show_dups <- function(x) {
    # get column names for checking duplicates
    # (a bit of a silly way to retrieve all column names - but works)
    cols <- head(x, 1) %>% collect() %>% names()
    count_(x, cols) %>% ungroup() %>% filter(n > 1)
}
