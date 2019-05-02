# Run License History for each Permission

firstyr <- 2009
lastyr <- 2018
db_license <- "E:/SA/Data-production/Data-Dashboards/__state__/license.sqlite3"
db_history <- "E:/SA/Data-production/Data-Dashboards/__state__/history.sqlite3"
script_dir = "2-license-history"


# Build License History ---------------------------------------------------

# for running parameterized by-permission.R
# generic script but might need state-specific tweaking
run_lichist <- function(
    priv_nm, lic_filter, yrs = firstyr:lastyr, 
    priv_hist = "NONE", test = FALSE, script_name = "by-permission.R"
) {
    params_passed <- TRUE # to disable default script parameters
    file_end <- ifelse(test, "-test.html", ".html")
    rmarkdown::render(
        input = file.path(script_dir, script_name),
        output_file = file.path("log", paste0(priv_nm, file_end)),
        knit_root_dir = getwd()
    )
}

## Test
run_lichist("hunt", 'type %in% c("hunt", "combo")', test = TRUE)
run_lichist("biggame", 'str_detect(priv, "biggame")', test = TRUE)

## Run 
run_lichist("hunt", 'type %in% c("hunt", "combo")')
run_lichist("fish", 'type %in% c("fish", "combo")')
run_lichist("all_sports", 'type %in% c("fish", "hunt", "combo")')

run_lichist("freshwater", 'str_detect(priv, "freshwater")')
run_lichist("saltwater", 'str_detect(priv, "saltwater")')

run_lichist("migratory", 'str_detect(priv, "migratory")')
run_lichist("biggame", 'str_detect(priv, "biggame")')
run_lichist("bear", 'str_detect(priv, "bear")')
run_lichist("turkey", 'str_detect(priv, "turkey")', 2015:lastyr) # incomplete in 2014
run_lichist("waterfowl", 'str_detect(priv, "waterfowl")', 2013:lastyr) # incomplete in 2012



# Summarize ---------------------------------------------------------------

# Produce customer summaries for before and after history database production
# (these summaries aren't needed for production - likely useful for reference/validation)

# for running parameterized summary.R
# this script shouldn't require state-specific tweaking
run_summary <- function(data_src = "db_license", script_name = "summary.R" ) {
    params_passed <- TRUE
    rmarkdown::render(
        input = file.path(script_dir, script_name),
        output_file = file.path("summary", paste0(data_src, ".html")),
        knit_root_dir = getwd()
    )
}
run_summary("db_license") # ignores multi-year/lifetime carry-over
run_summary("db_history")
