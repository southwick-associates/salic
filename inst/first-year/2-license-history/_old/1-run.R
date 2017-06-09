# run license history


# Prepare Parameters ------------------------------------------------------

# these 2 parameters will likely vary by state
yrs <- "2005:2015" # years stored in data
path <- "code/2-license-history/" # location of Rmd file

# the run_lichist() function runs the parameterized Rmd for different license types
run_lichist <- function(type, yrs = yrs, test = FALSE, priv = FALSE, 
                        path = path) {
    # the output html path is different when test == TRUE
    if (test) {
        output_file = paste0("by-type-test-", type, ".html")
    } else {
        output_file = paste0("by-type-", type, ".html")
    }
    rmarkdown::render(file.path(path, "by-type.Rmd"),
        params = list(type = type, priv = priv, test = test, yrs = yrs), 
        output_file = output_file
    )
}


# Test --------------------------------------------------------------------

# it's helpful to run some tests first (on a sample of data)
# the first argument will vary by state (as privileges vary by state)

run_lichist("trap", test = TRUE, write_out = FALSE)
run_lichist("trap", test = FALSE, write_out = FALSE)

run_lichist("trout", priv = TRUE, test = TRUE, write_out = FALSE)
run_lichist("trout", priv = TRUE, test = FALSE, write_out = FALSE)


# Overall Types -----------------------------------------------------------

# note: the license history code can take a while to run
run_lichist("hunt")
run_lichist("fish")
run_lichist("all_sports")


# Specific Privs ----------------------------------------------------------

run_lichist("deer", priv = TRUE)
run_lichist("sports", priv = TRUE)
run_lichist("conservation_patron", priv = TRUE)
run_lichist("spousal", priv = TRUE)
run_lichist("one_day_fishing", "2011:2015", priv = TRUE)
run_lichist("trout", priv = TRUE)
run_lichist("turkey", priv = TRUE)


# Compact Database --------------------------------------------------------
# this is only needed if tables have been removed/overwritten
# it can also be done manually with DB Browser or similar GUI interface

library(RSQLite)
con <- dbConnect(SQLite(), dbname = "~/Data2/WI_dashboard_2016/lic.sqlite3")
RSQLite::dbListTables(con)
dbGetQuery(con, "VACUUM")
dbDisconnect(con)

# Analyze Database --------------------------------------------------------
# this can be done for efficiency - not sure how much difference it makes

library(RSQLite)
con <- dbConnect(SQLite(), dbname = "~/Data2/WI_dashboard_2016/lic.sqlite3")
RSQLite::dbListTables(con)
dbGetQuery(con, "ANALYZE") # for query efficiency
dbDisconnect(con)

