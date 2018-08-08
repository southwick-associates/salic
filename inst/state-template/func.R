
# 1. Raw ------------------------------------------------------------------

# read_cust("path_to_file", n_max = 1000)
read_cust <- function(f, ...) {
    coltyp <- cols( )
    read_csv(f, col_types = coltyp, progress = FALSE, ...) %>%
        mutate(raw_lic_id = row_number())
}
read_lic <- function(f, ...) {}
read_sale <- function(f, ...) {}

write_raw <- function(df) {
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/raw-__period__.sqlite3"
    # connect, write, disconnect
    con <- dbConnect(RSQLite::SQLite(), f)
    dbWriteTable(con, deparse(substitute(df)), df) # probably use sql statement instead
    dbDisconnect(con)
}

test_overall <- function(sale, threshold = 0.1) {
    # Throw error if the percentage change in customer counts is outside of threshold
    # (need to either pull in existing sales data or existing summary)
}

test_lic <- function(sale, lic) {
    # Identify changes in license types, for example:
    # - new license types (which would require updating table)
    # - different descriptions > maybe throw error
}


# 2. Standardize ----------------------------------------------------------

load_raw <- function(nm, ...) {
    # pull cols of interest from sqlite
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/raw-__period__.sqlite3"
    con <- dbConnect(RSQLite::SQLite(), f)
    tbl(con, nm) %>%
        select(...) %>%
        collect()
    dbDisconnect(con)
}

standardize_sale <- function(df) {
    df %>% mutate(
        dot = , 
        start_date = , 
        end_date = , 
        revenue = ,
        source_sale = "__period__"
    )
}
standardize_cust <- function(df) {}

write_standard <- function(df) {
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/standard.sqlite3"
    # connect, append, disconnect
}

check_sale <- function(df) {}
check_cust <- function(df) {}


# 3. Finalize -------------------------------------------------------------

load_standard <- function(nm, ...) {
    # pull cols of interest from sqlite
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/standard.sqlite3"
}
