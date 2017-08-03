# run license history

# this function runs the parameterized Rmd for different license types
run_lichist <- function(
    priv_nm, lic_filter, priv_hist = "NONE", yrs = "2007:2016", test = FALSE, 
    path = "code/2-license-history" # location of by-priv.Rmd (may vary by state)
) {
    rmarkdown::render(file.path(path, "by-priv.Rmd"),
        params = list(
            priv_nm = priv_nm, lic_filter = lic_filter, priv_hist = priv_hist, 
            yrs = yrs,  test = test
        ), 
        output_file = paste0("by-priv-", priv_nm, ".html")
    )
}


# Test --------------------------------------------------------------------
run_lichist("hunt", 'type %in% c("hunt", "trap", "combo")', test = TRUE)
run_lichist("small_game", 'priv == "sg"', test = TRUE)


# Run Tables -----------------------------------------------------------
# note: the license history code can take a while to run

# Overall Permissions
run_lichist("hunt", 'type %in% c("hunt", "trap", "combo")')
run_lichist("fish", 'type %in% c("fish", "combo")')
run_lichist("all_sports", 'type %in% c("hunt", "trap", "fish", "combo")')

# Privilege Permissions
run_lichist("trapping", 'type == "trap"')
run_lichist("small_game", 'priv == "sg"')
run_lichist("migratory_bird", 'priv == "mb"')
run_lichist("firearm_deer", 'priv %in% c("fiream deer", "fiream deer/turkey")')
run_lichist("firearm_spring_turkey", 'priv %in% c("sp turkey", "fiream deer/turkey")')
run_lichist("firearm_fall_turkey", 'priv %in% c("fall turkey", "fiream deer/turkey")')
run_lichist("archery_deer_turkey", 'priv == "bow"')

# Subtype Permissions
# run_lichist("daily_small_game", 'subtype == "sg daily"', "small_game", test = TRUE)
run_lichist("daily_small_game", 'subtype == "sg daily"', "small_game")
run_lichist("sg_and_fish", 'subtype == "sg and fish"', "all_sports")
