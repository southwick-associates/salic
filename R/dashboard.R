# functions for building dashboard summary data


# Estimate ------------------------------------------------------------

#' Helper function: print a warning to flag records
#' 
#' The warning is only printed if the input data frame has more than one row 
#' (i.e., only flagged rows should be passed to warn())
#'
#' @param flagged data frame: table containing flagged rows
#' @param msg character: message to be printed in warming
#' @family dashboard functions
#' @keywords internal
#' @export
#' @importFrom utils capture.output
#' @examples
#' x <- data.frame(x = 1:2, val = c("careful", "a bit high"))
#' warn(x, "I'm a warning")
warn <- function(flagged, msg) {
    flagged <- data.frame(flagged)
    if (nrow(flagged) > 0) {
        warning(msg, "\n", paste(capture.output(print(flagged)), collapse = "\n"), 
                call. = FALSE)
    } 
}

