# would be only part of the first round (not updates)

build_standard_cust <- function() {
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/standard.sqlite3"
    con <- dbConnect(RSQLite::SQLite(), f)
    dbSendQuery(con,
        "CREATE TABLE cust(
        cust_id INTEGER,
        sex INTEGER,
        dob TEXT,
        last TEXT,
        first TEXT,
        state TEXT,
        cust_res INTEGER,
        zip TEXT,
        raw_cust_id INTEGER,
        source_cust TEXT)"
    )
    dbDisconnect(con)
}
build_standard_sale <- function() {}


# note: might want WITHOUT ROWID (e.g., if defining primary key), not sure on best practice
# also not yet 100% sure that I want to use primary keys
build_license_cust <- function() {
    f <- "../../../../../Data-sensitive/Data-Dashboards/__state__/standard.sqlite3"
    con <- dbConnect(RSQLite::SQLite(), f)
    dbSendQuery(con,
                "CREATE TABLE cust(
                cust_id NOT NULL INTEGER PRIMARY KEY,
                sex INTEGER,
                birth_year INTEGER,
                cust_res INTEGER,
                county_fips INTEGER,
                raw_cust_id INTEGER,
                source_cust TEXT)"
                )
    dbDisconnect(con)
}
build_license_sale <- function() {}
build_history <- function() {}
