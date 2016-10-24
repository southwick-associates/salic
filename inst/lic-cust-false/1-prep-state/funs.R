# functions

library(dplyr)
library(stringr)
library(lubridate)

recode_state <- function(x, var = "state", country_slct = c("US", "CN")) {
    state_dat <- read.csv("data/state-abbreviations.csv",
                          stringsAsFactors = FALSE) %>%
        filter(country %in% country_slct)
    x[[var]] <- toupper(x[[var]]) %>% str_replace("0", "O")
    x[[var]] <- ifelse(x[[var]] %in% state_dat$state, x[[var]], NA)
    x
}

distinct_rows <- function(x, rank_var, group_var) {
    group_by_(x, .dots = group_var) %>%
        arrange_(rank_var) %>%
        summarise_each(funs(last)) %>%
        ungroup()
}

# date recoding for moving between sqlite and R
# (probably not necessary)
char_to_date <- function() {

}
