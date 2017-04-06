# for recoding license data

#' Recode a state variable to deal with unusual values
#' 
#' The idea is to set any odd values to missing. It's a simple function that does
#' some minimal recoding
#' @param dat data frame: input data with a state variable
#' @param state_table data frame: input data holding valid abbreviations
#' @param oldvar character: name of state variable
#' @param newvar character: name of new state variable
#' @import dplyr
#' @family functions for recoding license data
#' @export
#' @examples
#' library(salic)
#' data(state_abbreviations)
#' data(cust)
#' x <- recode_state(cust, state_abbreviations)
#' dplyr::count(x, state_new, state)
recode_state <- function(dat, state_table, oldvar = "state", newvar = "state_new") {
    dat[[newvar]] <- gsub("0", "o", dat[[oldvar]]) %>% toupper()
    dat[[newvar]] <- ifelse(dat[[newvar]] %in% state_table$state, dat[[newvar]], NA)
    dat
}
