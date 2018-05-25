# functions for validating license data


# Helper Functions --------------------------------------------------------

#' Round numeric values and print as percent (internal salic only - not exported)
#'
#' This is a helper function for conveniently displaying percentages. It is used
#' in other summary functions in salic.
#' @param x numeric: Vector of values to display as rounded percentages
#' @param rnd numeric: number of decimals to round pct change results
#' @param scale numeric: scaling paramter - defaults to 100 for showing percentages
#' @family internal helper functions
#' @keywords internal
#' @examples
#' x <- data.frame(id = c(1,2,3,4), pop = c(135, 416, 389, 320))
#' x$pct <- pct_round(x$pop / sum(x$pop))
pct_round <- function(x, rnd = 1, scale = 100) {
    # sprintf is used to insure trailing zeroes are included
    sprintf_param <- paste0("%.", rnd, "f")
    paste0(sprintf(sprintf_param, round(x * scale, rnd)), "%")
}


#' Calculate churn for a single year (internal salic only - not exported)
#'
#' This is a simple calculation that doesn't take into account multi-year
#' licenses, so it should only be used for initial data validation
#' @param x data frame: Table holding sales by year
#' @param year numeric: year to calculate churn (e.g., churn for 2014 uses sales
#' for 2 years: 2013 and 2014)
#' @import dplyr
#' @return Returns a numeric vector of length 1
#' @family internal helper functions
#' @keywords internal
#' @examples
#' library(tidyverse)
#' library(salic)
#' load(sale, lic, package = "salic")
#' 
#' y <- select(sale, cust_id, year) %>%
#'     filter(year %in% 2012:2013) %>% distinct()
#'
#' calc_churn(y, 2013)
calc_churn <- function(x, yr) {
    y1 <- filter(x, year == yr-1)
    y2 <- filter(x, year == yr)
    renew <- inner_join(y1, y2, by = "cust_id")
    # churn calculation
    held_license_y1 <- nrow(y1)
    didnt_renew <- held_license_y1 - nrow(renew)
    didnt_renew / held_license_y1
}


# Validation Functions ----------------------------------------------------

#' Check for duplicates in a table
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
#' This is a quick way of counting lines in a text file, to insure the final # of rows in 
#' the loaded dataset is correct: 
#'  https://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r
#' @param file_path character: Path to file
#' @family functions for validating license data
#' @export
#' @examples
#' count_lines_textfile
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
#' @param sale data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' data(sale, lic, package = "salic")
#' 
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
#' @param x data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @param include_revenue logical: If TRUE, revenue by year will also be shown
#' @param out character: file path to optional output csv
#' @param title character: title for output table
#' @param note character: note for output table
#' @param suppress_notes: If TRUE, returns a data frame only
#' @inheritParams pct_round
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' data(sale, lic, package = "salic")
#' 
#' summary_sale(sale)
#' summary_sale(sale, out = "summary_sale.csv")
#'
#' # example with made-up license revenue
#' sale <- left_join(sale, lic)
#' sale <- mutate(sale, revenue = 30)
#' summary_sale(sale, rnd = 2, include_revenue = T)
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
#' @param x data frame: Table holding sales by year with a minimum of
#' 2 variables (cust_id, year)
#' @param years numeric: range of years needed to calculate churn
#' @inheritParams summary_sale
#' @import dplyr
#' @family functions for validating license data
#' @export
#' @examples
#' library(tidyverse)
#' library(salic)
#' data(sale, lic, package = "salic")
#' 
#' summary_churn(sale, 2004:2013)
#' summary_churn(sale, 2004:2013, out = "summary_churn.csv")
#'
#' library(dplyr)
#' sale <- left_join(sale, lic)
#' filter(sale, priv == "hunt") %>%
#'     summary_churn(2004:2013, title = "Hunting Churn", note = "An additional note")
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

