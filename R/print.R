# internal helper functions for printing tables with caption/note
# only currently used in summary_sale() and summary_churn()


#' Format numbers for printing (internal salic only - not exported)
#'
#' Helper function for use in print_dat
#' @param x vector to format
#' @param digits number of digits for rounding
#' @param big.mark character: separator between 1000s
#' @family internal helper functions
#' @keywords internal
#' @examples
#' format_num(c(2005, 2006, NA))
#' format_num(c(100000, 131000, 150000, NA))
#' format_num(c(100000, 131000, 150000, NA), big.mark = "")
#' format_num(c(27.456, 25.234, 30.679, NA))
#' format_num(c(27, 25, 30))
#' format_num(c("6.2%", "13.9%", "8.2%", NA))
format_num <- function(x, digits = 1, big.mark = ",") {
    if (is.numeric(x)) {
        # determine whether digits are used
        # only include digits in the absence of whole numbers
        if (all(x %% 1 == 0, na.rm = T)) digits = 0
        
        if (min(x, na.rm = T) > 10000) {
            out <- formatC(x, big.mark = big.mark, digits = 0, format = "f")
        } else if (min(x, na.rm = T) > 100) {
            out <- formatC(x, big.mark = "", digits = 0, format = "f")
        } else {
            out <- formatC(x, big.mark = "", digits = digits, format = "f")
        }
    } else {
        out <- formatC(as.character(x))
    }
    out
}


#' Print a data frame with caption/note (internal salic only - not exported)
#'
#' Intended for showing tables with titles & notes in logged output in doc/
#' @param x data frame: data frame contents to print
#' @param caption character: Optional caption to print
#' @param note character: Optional note(s) to print.
#' for multiple lines of notes
#' @inheritParams format_num
#' @family internal helper functions
#' @keywords internal
#' @examples
#' x <- data.frame(yr = c(2005, 2006), cust = c(100000, 131000),
#'     sales = c(567891, 673568), churn = c(NA, 25.23), char = c("test", NA))
#' print_dat(x)
#' print_dat(x, "Customer Sales by Year")
#' print_dat(x, "Customer Sales by Year", "A note!")
#' print_dat(x, "Customer Sales by Year", big.mark = "")
#' print_dat(x, "Customer Sales by Year", digits = 0)
print_dat <- function(x, caption = NULL, note = NULL,
                      digits = 1, big.mark = ",") {
    # print caption and note
    if (!is.null(caption)) cat(paste0(caption, ":\n"))
    if (!is.null(note)) cat(paste0("(", note, ")", "\n"))
    
    # add dashes - if applicable
    dash_len <- function(caption, note) {
        if (!is.null(note)) {
            notes <- unlist(strsplit(note, "\n"))
            note_len <- max(nchar(notes), na.rm = T) + 2
        } else {
            note_len <- 0
        }
        caption_len <- nchar(caption) + 1
        max(caption_len, note_len, na.rm = T)
    }
    if (!is.null(caption) | !is.null(note)) {
        dash_num <- dash_len(caption, note)
        cat(paste0(paste(rep("-", dash_num), collapse = ""), "\n"))
    }
    
    # print data frame (with number formatting)
    format_num2 <- function(x) format_num(x, digits = digits, big.mark = big.mark)
    x <- dplyr::mutate_all(x, "format_num2")
    print(data.frame(x), row.names = FALSE)
}

