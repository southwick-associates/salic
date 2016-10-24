# functions for making files, folders, etc.
# some (maybe all?) will rely on package saproj

#' Create a new analysis data sub-directory with template scripts
#'
#' This is a variation on saproj \code{\link[saproj]{new_section}}. Section titles
#' and template scripts are populated given the selected number
#' @param num integer: Number for script order (1 >> prep-state, etc.)
#' @param cust_file logical: If TRUE, template scripts assume separate files for
#' customer and transaction data (otherwise they are assumed to be stored together)
#' @param projdir character: Subdirectory to place folders/files. If NULL,
#' the working directory will be used
#' @family functions for making directories and files
#' @export
#' @examples
#' saproj::new_project(projdir = "test-dir")
#' salic::new_section(1, projdir = "test-dir")
#' salic::section(2, projdir = "test-dir")
new_section <- function(num, cust_file = TRUE, projdir = NULL) {
    if (!requireNamespace("saproj", quietly = TRUE)) {
        stop("saproj needed for this function to work. Please install it.",
             call. = FALSE)
    }
    sect_names <- c("1-prep-state", "2-attach-data", "3-license-history",
                    "4-population-data", "5-summary-results")
    saproj::new_section(sect_names[num], projdir)

    # copy template scripts to the section
    if (!(cust_file) & num == 1) {
        src_dir <- system.file("lic-cust-false", sect_names[1], package = "salic")
    } else {
        src_dir <- system.file("lic", sect_names[num], package = "salic")
    }
    saproj::file_copy(src_dir, file.path("code"), projdir,
                      recursive = TRUE)
}
