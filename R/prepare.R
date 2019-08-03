# functions for preparing license data from states

# certain functions reference global variables (e.g., named variables in data frames)
# - these occur in a number of places in salic (mostly validation functions)
# - R CMD check doesn't like this: declaring them in globalVariables() is needed
# - https://github.com/r-lib/devtools/issues/1714
if(getRversion() >= "2.15.1") utils::globalVariables(c(
    "R3", "age", "age_year", "agecat", "birth_year", "bought", "change_cust",
    "change_revenue", "change_sales", "customers", "dot", "dot2", "duration", 
    "duration_run", "issue_month", "issue_year", "lag_duration_run", "lag_year",
    "lapse", "lastvar", "lead_year", "month", "new_recruit", "old_recruit", 
    "sales", "var_old", "year", "yr_diff", "yrs_since", "yrs_till_next", 
    "cust_id", ":="
))



# Recoding Data -----------------------------------------------------------

#' Recode state abbreviations
#' 
#' This is a convenience function to help standardize odd-looking state abbreviations
#' based on a reference list of allowed abbreviations.
#' 
#' @param dat data frame: input data with a state variable
#' @param state_table data frame: input data holding valid abbreviations
#' @param oldvar character: name of state variable
#' @param newvar character: name of new state variable
#' @import dplyr
#' @family functions for standardizing state license data
#' @export
#' @examples 
#' data(state_abbreviations)
#' dat <- data.frame(state = c("M0", "mo", "Mo", "MR", "YY"))
#' recode_state(dat, state_abbreviations)
recode_state <- function(dat, state_table, oldvar = "state", newvar = "state_new") {
    dat[[newvar]] <- gsub("0", "o", dat[[oldvar]]) %>% toupper()
    dat[[newvar]] <- ifelse(dat[[newvar]] %in% state_table$state, dat[[newvar]], NA)
    dat
}

#' Standardize month (for individual state dashboards)
#' 
#' DON'T USE FOR NAT/REG PREPARATION (where year should be based on sale date)
#' 
#' The output month variable captures transaction year and month in one metric.
#' The month number is relative to the license year: 1 = Jan, 2 = Feb, etc, for
#' current/future years and 0 = Dec, -1 = Nov, ..., for previous years.
#' 
#' @param sale data frame: Input sales table with at lest 2 variables: dot
#' (transaction date in 'yyyy-mm-dd') and year (numeric license year)
#' @param month_range numeric: A vector of months allowed in the output. Defaults
#' to 0:12 since this is a common range of sales. Any months outside the range will be
#' set to either the lowest values (for those less than the range) or the
#' highest value (for those over). This prevents unusual sale months from appearing
#' in results.
#' @import dplyr
#' @return Returns a sales table with a standardized 'month' variable and
#' prints a validation output.
#' @family functions for standardizing state license data
#' @export
#' @examples 
#' # For Southwick analysts: see data preparation scripts
recode_month <- function(sale, month_range = 0:12) {
    # error - don't run if lubridate isn't installed
    if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("lubridate needed for this function to work. Please install it.",
             call. = FALSE)
    }
    # calculate standardized month
    sale <- sale %>% mutate(
        dot2 = lubridate::ymd(dot), 
        issue_month = lubridate::month(dot2), 
        issue_year = lubridate::year(dot2), 
        yr_diff = issue_year - year, 
        month = issue_month + yr_diff * 12
    )
    # enforce specific range (bottom or top coding as necessary)
    if (!is.null(month_range)) {
        sale <- sale %>% mutate(
            month = ifelse(month < min(month_range), min(month_range), month),
            month = ifelse(month > max(month_range), max(month_range), month)
        )
    }
    # check new month specification
    test <- count(sale, year, month, issue_year, issue_month)
    last_year <- max(test$year)
    cat("\nRecoding Summary:\n")
    filter(test, year == (last_year-1)) %>% data.frame() %>% print(row.names = FALSE) 
    
    # finalize by dropping temporary variables
    sale %>%
        select(-yr_diff, -issue_month, -issue_year, -dot2)
}


# Validation --------------------------------------------------------------
# functions for validating license data

#' Check for duplicates in a table
#' 
#' @param x data frame: Table holding records
#' @import dplyr
#' @family functions for validating license data
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

#' Count rows in a text file
#' 
#' This is a quick way of counting lines in a text file, to ensure the final # of rows in 
#' the loaded dataset is correct: 
#'  https://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r
#'  
#' @param file_path character: Path to file
#' @family functions for validating license data
#' @export
#' @examples
#' x <- matrix(1:10, ncol = 5)
#' f <- tempfile("test-matrix")
#' write(x, f)
#' readLines(f)
#' count_lines_textfile(f)
#' unlink(f)
count_lines_textfile <- function(file_path) {
    f_bin <- file(file_path, open="rb")
    nlines <- 0L
    while (length(chunk <- readBin(f_bin, "raw", 65536)) > 0) {
        nlines <- nlines + sum(chunk == as.raw(10L))
    }
    close(f_bin)
    nlines
}

#' Summarize sales, customers, and churn by year
#' 
#' This is a wrapper function that combines \code{\link{summary_sale}} and 
#' \code{\link{summary_churn}}
#' 
#' @param sale data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' data(sale)
#' summary_initial(sale)
summary_initial <- function(sale) {
    # get years for churn summary
    all_yrs <- sort(unique(sale$year))
    
    # produce output data
    sale_out <- summary_sale(sale, suppress_notes = TRUE)
    churn_out <- summary_churn(sale, all_yrs, suppress_notes = TRUE)
    dat_out <- churn_out %>%
        left_join(sale_out, by = "year")
    
    # print output summary
    print_dat(dat_out, "Summarize Sales, Customers, & Churn", paste0(
        "Typical Churn: hunting: 20-45% --- fishing: 35-60%)\n",  
        "(USFWS: https://wsfrprograms.fws.gov/Subpages/LicenseInfo/LicenseIndex.htm"
    ))
}

#' Summarize sales and customers by year
#' 
#' @param x data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @param include_revenue logical: If TRUE, revenue by year will also be shown
#' @param out character: file path to optional output csv
#' @param title character: title for output table
#' @param note character: note for output table
#' @param suppress_notes logical: If TRUE, returns a data frame only
#' @inheritParams pct_round
#' @import dplyr
#' @importFrom utils write.csv
#' @family functions for validating license data
#' @export
#' @examples
#' library(dplyr)
#' data(sale)
#' summary_sale(sale)
#'
#' # example with made-up license revenue
#' sale2 <- mutate(sale, revenue = 30)
#' summary_sale(sale2, rnd = 2, include_revenue = TRUE)
summary_sale <- function(
    x, include_revenue = FALSE, rnd = 1, out = NULL, 
    title = "Summarize Sales & Customers", note = NULL, suppress_notes = FALSE
) {
    pct_round2 <- function(x) pct_round(x, rnd)
    cust <- select(x, cust_id, year) %>% distinct %>% count(year) %>%
        mutate(change_cust = (n - lag(n)) / lag(n)) %>%
        rename(customers = n)
    sale <- count(x, year) %>%
        mutate(change_sales = (n - lag(n)) / lag(n)) %>%
        rename(sales = n)
    if (include_revenue) {
        if (!("revenue" %in% colnames(x))) {
            stop(paste("The input data frame must have a column called 'revenue'",
                       "if include_revenue = TRUE"), call. = FALSE)
        }
        revenue <- group_by(x, year) %>%
            summarise(revenue = sum(revenue, na.rm = TRUE)) %>%
            mutate(change_revenue = (revenue - lag(revenue)) / lag(revenue))
        output <- left_join(cust, sale, by = "year") %>%
            left_join(revenue, by = "year") %>%
            select(year, change_cust, customers, change_sales, sales,
                   change_revenue, revenue)
        out_print <- mutate_at(output, vars(change_cust, change_sales, change_revenue),
                               funs(pct_round2))
    } else {
        output <- left_join(cust, sale, by = "year") %>%
            select(year, change_cust, customers, change_sales, sales)
        out_print <- mutate_at(output, vars(change_cust, change_sales),
                               funs(pct_round2))
    }
    if (!is.null(out)) write.csv(output, file = out, row.names = FALSE)
    
    if (suppress_notes) {
        out_print
    } else {
        note_out <- "https://wsfrprograms.fws.gov/Subpages/LicenseInfo/LicenseIndex.htm"
        if (!is.null(note)) note_out <- paste0(note_out, ")\n(", note)
        print_dat(out_print, title, note_out)
    }
}

#' Summarize churn by year
#'
#' This is a simple calculation that doesn't take into account multi-year
#' licenses, so it should only be used for initial data validation
#' 
#' @param x data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @param years numeric: range of years needed to calculate churn
#' @inheritParams summary_sale
#' @import dplyr
#' @importFrom utils write.csv
#' @family functions for validating license data
#' @export
#' @examples
#' library(dplyr)
#' data(sale, lic)
#' summary_churn(sale, 2012:2018)
#'
#' sale <- left_join(sale, lic)
#' filter(sale, type %in% c("hunt", "combo")) %>%
#'     summary_churn(2012:2018, title = "Hunting Churn", note = "A note")
summary_churn <- function(
    x, years, rnd = 1, out = NULL, title = "Churn by Year", 
    note = NULL, suppress_notes = FALSE
) {
    # get a single row per customer-year for churn calculation
    y <- select(x, cust_id, year) %>%
        filter(year %in% years) %>%
        distinct()
    churn <- vector(mode = "numeric", length = length(years) - 1)
    yrs <- vector(mode = "character", length = length(years) - 1)
    for (i in seq_along(years[-1])) {
        churn[i] <- calc_churn(y, years[i+1]) * 100
        yrs[i] <- years[i+1]
    }
    output <- data.frame(year = yrs, churn)
    out_print <- mutate(output, churn = pct_round(churn, rnd, scale = 1))
    if (!is.null(out)) write.csv(output, file = out, row.names = FALSE)
    note_out <- "typical hunting: 20-45% --- typical fishing: 35-60%"
    if (!is.null(note)) note_out <- paste0(note_out, ")\n(", note)
    
    if (suppress_notes) {
        out_print %>%
            mutate(year = as.integer(as.character(year)))
    } else {
        print_dat(out_print, title, note_out)
    }
}


# Validation Helpers ------------------------------------------------------
# only intended for use by salic functions

#' Round numeric values and print as percent (for internal salic use)
#'
#' This is a helper function for conveniently displaying percentages. It is used
#' in other summary functions in salic.
#' 
#' @param x numeric: Vector of values to display as rounded percentages
#' @param rnd numeric: number of decimals to round pct change results
#' @param scale numeric: scaling paramter - defaults to 100 for showing percentages
#' @family internal helper functions
#' @keywords internal
#' @export
#' @examples
#' x <- data.frame(id = c(1,2,3,4), pop = c(135, 416, 389, 320))
#' x$pct <- pct_round(x$pop / sum(x$pop))
#' x
pct_round <- function(x, rnd = 1, scale = 100) {
    # sprintf is used to insure trailing zeroes are included
    sprintf_param <- paste0("%.", rnd, "f")
    paste0(sprintf(sprintf_param, round(x * scale, rnd)), "%")
}

#' Calculate churn for a single year (for internal salic use)
#'
#' This is a simple calculation that doesn't take into account multi-year
#' licenses, so it should only be used for initial data validation.
#' 
#' @param x data frame: Table holding sales by year
#' @param yr numeric: year to calculate churn (e.g., churn for 2014 uses sales
#' for 2 years: 2013 and 2014)
#' @import dplyr
#' @return Returns a numeric vector of length 1
#' @family internal helper functions
#' @keywords internal
#' @export
#' @examples
#' library(dplyr)
#' data(sale)
#' 
#' select(sale, cust_id, year) %>%
#'     filter(year %in% 2012:2013) %>% 
#'     distinct() %>%
#'     calc_churn(2013)
calc_churn <- function(x, yr) {
    y1 <- filter(x, year == yr-1)
    y2 <- filter(x, year == yr)
    renew <- inner_join(y1, y2, by = "cust_id")
    # churn calculation
    held_license_y1 <- nrow(y1)
    didnt_renew <- held_license_y1 - nrow(renew)
    didnt_renew / held_license_y1
}

#' Format numbers for printing (for internal salic use)
#'
#' Helper function for use in print_dat
#' @param x vector to format
#' @param digits number of digits for rounding
#' @param big.mark character: separator between 1000s
#' @family internal helper functions
#' @keywords internal
#' @export
#' @examples
#' format_num(c(2005, 2006, NA))
#' format_num(c(100000, 131000, 150000, NA))
#' format_num(c(100000, 131000, 150000, NA), big.mark = "")
#' format_num(c(27.456, 25.234, 30.679, NA))
#' format_num(c(27, 25, 30))
#' format_num(c("6.2%", "13.9%", "8.2%", NA))
format_num <- function(x, digits = 1, big.mark = ",") {
    if (is.numeric(x)) {
        # determine whether digits are used
        # only include digits in the absence of whole numbers
        if (all(x %% 1 == 0, na.rm = TRUE)) digits = 0
        
        if (min(x, na.rm = TRUE) > 10000) {
            out <- formatC(x, big.mark = big.mark, digits = 0, format = "f")
        } else if (min(x, na.rm = TRUE) > 100) {
            out <- formatC(x, big.mark = "", digits = 0, format = "f")
        } else {
            out <- formatC(x, big.mark = "", digits = digits, format = "f")
        }
    } else {
        out <- formatC(as.character(x))
    }
    out
}

#' Print a data frame with caption/note (for internal salic use)
#'
#' Intended for showing tables with titles & notes in logged output in doc/
#' @param x data frame: data frame contents to print
#' @param caption character: Optional caption to print
#' @param note character: Optional note(s) to print.
#' for multiple lines of notes
#' @inheritParams format_num
#' @family internal helper functions
#' @keywords internal
#' @export
#' @examples
#' x <- data.frame(yr = c(2005, 2006), cust = c(100000, 131000),
#'     sales = c(567891, 673568), churn = c(NA, 25.23), char = c("test", NA))
#' print_dat(x)
#' print_dat(x, "Customer Sales by Year")
#' print_dat(x, "Customer Sales by Year", "A note!")
#' print_dat(x, "Customer Sales by Year", big.mark = "")
#' print_dat(x, "Customer Sales by Year", digits = 0)
print_dat <- function(x, caption = NULL, note = NULL,
                      digits = 1, big.mark = ",") {
    # print caption and note
    if (!is.null(caption)) cat(paste0(caption, ":\n"))
    if (!is.null(note)) cat(paste0("(", note, ")", "\n"))
    
    # add dashes - if applicable
    dash_len <- function(caption, note) {
        if (!is.null(note)) {
            notes <- unlist(strsplit(note, "\n"))
            note_len <- max(nchar(notes), na.rm = TRUE) + 2
        } else {
            note_len <- 0
        }
        caption_len <- nchar(caption) + 1
        max(caption_len, note_len, na.rm = TRUE)
    }
    if (!is.null(caption) | !is.null(note)) {
        dash_num <- dash_len(caption, note)
        cat(paste0(paste(rep("-", dash_num), collapse = ""), "\n"))
    }
    
    # print data frame (with number formatting)
    format_num2 <- function(x) format_num(x, digits = digits, big.mark = big.mark)
    x <- dplyr::mutate_all(x, "format_num2")
    print(data.frame(x), row.names = FALSE)
}
