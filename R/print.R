# functions for printing headers, notes, and tables
# these will probably be deprecated

#' Format numbers for printing
#'
#' Helper function for use in print_dat
#' @param x vector to format
#' @param digits number of digits for rounding
#' @param big.mark character: separator between 1000s
#' @family helper functions for printing headers, notes, and tables
#' @export
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

# Get border length for printing
# Helper function for use in print_dat - not currently implemented
border_len <- function(dat) {
    vec_width <- function(x) max(nchar(x), na.rm = T) + 1
    col_width <- function(dat) apply(dat, 2, vec_width)
    nm_width <- function(dat) nchar(names(dat)) + 1
    sum(apply(rbind(col_width(dat), nm_width(dat)), 2, max))
}


#' Convenience function to print contents of a data frame
#'
#' Intended for showing tables with titles & notes in logged output in doc/
#' @param x data frame: data frame contents to print
#' @param caption character: Optional caption to print
#' @param note character: Optional note(s) to print.
#' for multiple lines of notes
#' @param title character: Title to print
#' @inheritParams format_num
#' @family functions for printing headers, notes, and tables
#' @export
#' @examples
#' x <- data.frame(yr = c(2005, 2006), cust = c(100000, 131000),
#'     sales = c(567891, 673568), churn = c(NA, 25.23), char = c("test", NA))
#' print_dat(x)
#' print_dat(x, "Customer Sales by Year")
#' print_dat(x, "Customer Sales by Year", "A note!")
#' print_dat(x, "Title", "note)\n(long note to show dash printing")
#'
#' print_dat(x, "Customer Sales by Year", big.mark = "")
#' print_dat(x, "Customer Sales by Year", digits = 0)
print_dat <- function(x, caption = NULL, note = NULL, title = NULL,
                      digits = 1, big.mark = ",") {
    if (!is.null(title)) {
        cat_title(title)
        cat("\n")
    }
    if (!is.null(caption)) cat_caption(caption)
    if (!is.null(note)) cat_note(note)
    
    # number formatting
    format_num2 <- function(x) format_num(x, digits = digits, big.mark = big.mark)
    x <- dplyr::mutate_all(x, "format_num2")
    
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
        # dash_num <- nchar(caption) + 1
        # dash_num <- border_len(x)
        cat(paste0(paste(rep("-", dash_num), collapse = ""), "\n"))
    }
    print(data.frame(x), row.names = FALSE)
    # stargazer::stargazer(x, summary = FALSE, type = "text",
    #                      rownames = FALSE, notes = note, title = caption)
}

#' Convenience function to print a title
#' @param title character: Title to print
#' @inheritParams cat_note
#' @family functions for printing headers, notes, and tables
#' @export
#' @examples
#' cat_title("here is a title")
#' cat_title("here is a title", "with a note")
cat_title <- function(title, note = NULL) {
    title <- toupper(title)
    title <- paste0("---", title, "---")
    cat(paste0(title, "\n"))
    if (!is.null(note)) cat_note(note)
}

#' Convenience function to print a note
#' @param note character: Note to print
#' @family functions for printing headers, notes, and tables
#' @export
#' @examples
#' cat_note("here is a note")
cat_note <- function(note) {
    note <- paste0("(", note, ")")
    cat(paste0(note, "\n"))
}

#' Convenience function to print a caption (intended for tables)
#' @param caption character: Caption to print
#' @family functions for printing headers, notes, and tables
#' @export
#' @examples
#' cat_caption("Here is a caption")
cat_caption <- function(caption) {
    cat(paste0(caption, ":\n"))
}
