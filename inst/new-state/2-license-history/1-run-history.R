# run license history

# this function runs the parameterized Rmd for different license types
run_lichist <- function(
    priv_nm, lic_filter, priv_hist = "NONE", 
    yrs = "2008:2018", life_yr1 = "NONE", test = FALSE, 
    path = "2-license-history" # location of by-permission.Rmd (may vary by state)
) {
    if (test) endfile <- "-test.html" else endfile <- ".html"
    rmarkdown::render(file.path(path, "by-permission.Rmd"),
                      params = list(
                          priv_nm = priv_nm, lic_filter = lic_filter, priv_hist = priv_hist, 
                          yrs = yrs, life_yr1 = life_yr1, test = test
                      ), 
                      output_file = paste0("by-permission-", priv_nm, endfile)
    )
}


# Test --------------------------------------------------------------------
run_lichist("hunt", 'type %in% c("hunt", "trap", "combo")', test = TRUE)
run_lichist("trout", 'priv == "trout"', test = TRUE)
run_lichist("saltwater_info_program", 'priv == "sip"', yrs = "2014:2017", test = TRUE)
run_lichist("combination", 'subtype == "combination"', "all_sports", test = TRUE)


# Run Tables -----------------------------------------------------------
# note: the license history code can take a while to run

# Overall Permissions
run_lichist("hunt", 'type %in% c("hunt", "trap", "combo")')
run_lichist("fish", 'type %in% c("fish", "combo")')
run_lichist("all_sports", 'type %in% c("hunt", "trap", "fish", "combo")')

# Privilege Permissions
run_lichist("saltwater_info_program", 'priv == "sip"', yrs = "2014:2017")
run_lichist("big_game", 'priv %in% c("bg", "sport")')
run_lichist("waterfowl", 'priv %in% c("gwc", "sport")')
run_lichist("trout", 'priv %in% c("trout", "sport")')
run_lichist("commercial_fishing", 'priv == "comm"')

# Subtype Permissions
run_lichist("combination", 'subtype == "combination"', "all_sports")
run_lichist("sportsmans", 'subtype == "sportsmans"', "all_sports")
run_lichist("short_term", 'short_term == "x"', "all_sports")
run_lichist("nuisance_wildlife", 'subtype == "nuisance"', "hunt")
