# functions for preparing population data

library(dplyr)
library(tidyr)
library(stringr)
library(acs)
library(readxl)

# using package acs to pull estimates
get_acs <- function(year = 2015, state = "OR", span = 5, table = "B01001",
                    county = "*") {
    if (is.null(county)) {
        geo <- geo.make(state = state)
    } else {
        geo <- geo.make(state = state, county = county)
    }
    dat <- acs.fetch(endyear = year, table.number = table, span = span,
                     geography = geo, col.names = "pretty")
    x <- estimate(dat) %>% as.data.frame()
    x$county <- row.names(x)
    gather(x, var, value, -county)
}

# pull & prepare data from a census xlsx file
# this is pretty tedious, but at least scripted
# might be able to access this through package acs (connect to 10-yr census data perhaps)
get_state_pop <- function(path = "~/data2/_CENSUS_ACS/pop_by_state update/nst-est2016-01.xlsx",  
                              yrs = 2010:2016) {
    nms <- c("state", "drop1", "drop2", as.character(2010:2016))
    typs <- c("text", rep("numeric", 9))
    x <- read_excel(path, skip = 9, col_names = nms, col_types = typs)
    x <- x[1:51,]
    mutate(x, state = stringr::str_replace(state, ".", "")) %>%
        select(-drop1, -drop2) %>%
        gather(year, pop_state, -state)
}
get_state_pop_old <- function(path = "~/data2/_CENSUS_ACS/pop_by_state update/st-est00int-01.xls",  
                              yrs = 2000:2009) {
    nms <- c("state", "drop", as.character(yrs), "drop2", "drop3")
    typs <- c("text", rep("numeric", 13))
    x <- read_excel(path, skip = 9, col_names = nms, col_types = typs)
    x <- x[1:51,]
    mutate(x, state = stringr::str_replace(state, ".", "")) %>%
        select(-drop, -drop2, -drop3) %>%
        gather(year, pop_state, -state)
}


# Checking Data -----------------------------------------------------------

# check that breakouts sum to total
check_pop_totals <- function(pop) {
    # male
    m <- filter(pop, sex == "Male", age != "All") %>% group_by(year) %>%
        summarise(pop = sum(value))
    m1 <- filter(pop, sex == "Male", age == "All") %>% group_by(year) %>%
        summarise(value = sum(value))
    # female
    f <- filter(pop, sex == "Female", age != "All") %>% group_by(year) %>%
        summarise(pop = sum(value))
    f1 <- filter(pop, sex == "Female", age == "All") %>% group_by(year) %>%
        summarise(value = sum(value))
    # all
    a <- filter(pop, sex == "All") %>% group_by(year) %>% summarise(pop = sum(value))
    a1 <- bind_rows(m, f) %>% group_by(year) %>% summarise(value = sum(pop))
    
    # compare
    rbind(merge(m, m1) %>% mutate(level = "Male", diff = pop - value), 
          merge(f, f1) %>% mutate(level = "Female", diff = pop - value),
          merge(a, a1) %>% mutate(level = "All", diff = pop - value))
}

# check % change by year in breakouts
pct_change_sex <- function(x, rnd = 2) {
    filter(x, age == "All") %>% 
        group_by(sex, year) %>%
        summarise(value = sum(value)) %>%
        mutate(pct_change = round((value - lag(value)) / lag(value) * 100, rnd) %>%
                   paste0("%"))
}
pct_change_age <- function(x, rnd = 2) {
    ungroup(x) %>%
        filter(sex != "All") %>%
        group_by(age, year) %>%
        summarise(value = sum(value)) %>%
        mutate(pct_change = round((value - lag(value)) / lag(value) * 100, rnd) %>%
                   paste0("%"))
}

