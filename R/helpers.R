# helper functions for printing

# TODO - might be able to drop these in final version
# otherwise probably use as internal only (don't export)

#' Convenience function to print contents of a data frame
#'
#' @param x data frame: data frame contents to print
#' @family functions for for checking license history summarization
#' @import dplyr
#' @export
#' @examples
#' printf()
printf <- function(x) {
    data.frame(x) %>% print(row.names = FALSE)
}

#' Convenience function to print a title
#'
#' @param x character: Contents of title to print
#' @family functions for for checking license history summarization
#' @export
#' @examples
#' catf()
catf <- function(x) {
    cat(paste0("\n", x, "\n"))
}
catf2 <- function(x) cat(paste0(x, "\n"))

#' Round numeric values and print as a character percentages
#'
#' This is a helper function for conveniently displaying percentages. It is used
#' in other summary functions in salic.
#' @param x numeric: Vector of values to display as rounded percentages
#' @param rnd numeric: number of decimals to round pct change results
#' @param scale numeric: scaling paramter - defaults to 100 for showing percentages
#' @family functions for validating license data
#' @export
#' @examples
#' x <- data.frame(id = c(1,2,3,4), pop = c(135, 416, 389, 320))
#' x$pct <- pct_round(x$pop / sum(x$pop))
#' print_dat(x, "Population Percentages")
pct_round <- function(x, rnd = 1, scale = 100) {
    # sprintf is used to insure trailing zeroes are included
    sprintf_param <- paste0("%.", rnd, "f")
    paste0(sprintf(sprintf_param, round(x * scale, rnd)), "%")
}
