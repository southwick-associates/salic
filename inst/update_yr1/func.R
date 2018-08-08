
# 1. Raw ------------------------------------------------------------------

read_lic <- function(f, ...) {
    coltyp <- cols( )
    read_csv(f, col_types = coltyp, progress = FALSE, ...) %>%
        mutate(raw_lic_id = row_number())
}

read_cust <- function(f, ...) {}

read_sale <- function(f, ...) {}

write_raw <- function(df) {
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/raw-__period__.sqlite3"
    # connect, write, disconnect
}

check_overall <- function(sale, threshold = 0.1) {
    # Throw error if the percentage change in customer counts is outside of threshold
    # (need to either pull in existing sales data or existing summary)
}

check_lic <- function(sale, lic) {
    # Identify changes in license types, for example:
    # - new license types (which would require updating table)
    # - different descriptions > maybe throw error
}


# 2. Standardize ----------------------------------------------------------

load_raw <- function(nm, ...) {
    # pull cols of interest from sqlite
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/raw-__period__.sqlite3"
    db <- src_sqlite(f)
    tbl(db, nm) %>%
        select(...) %>%
        collect()
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
