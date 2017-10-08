# functions for making files, folders, etc.
# some (maybe all?) will rely on package saproj

### Add func.R (probably on num = 1)

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
#' salic::new_section_yr1(1, projdir = "test-dir")
#' salic::section(2, projdir = "test-dir")
new_section_yr1 <- function(num, cust_file = TRUE, projdir = NULL) {
    if (!requireNamespace("saproj", quietly = TRUE)) {
        stop("saproj needed for this function to work. Please install it.",
             call. = FALSE)
    }
    sect_names <- c("1-prep-license-data", "2-license-history", "3-secondary-data",
                    "4-dashboard-results")
    # use (alt) set of scripts if there isn't a separate customer table
    if (!cust_file) sect_names[1] <- paste(sect_names[1], "alt", sep = "-")
    
    # populate new folders for selected section
    saproj::new_section(sect_names[num], projdir)

    # copy template scripts to the section
    src_dir <- system.file("first-year", sect_names[num], package = "salic")
    saproj::file_copy(src_dir, file.path("code"), projdir, recursive = TRUE, 
                      overwrite = FALSE)
    
    # copy any R files stored in parent directory for section 1 (num = 1)
    if (num == 1) {
        salic_path <- system.file("first-year", package = "salic")
        R_files <- list.files(salic_path, pattern = "\\.R", full.names = TRUE)
        for (f in R_files) saproj::file_copy(f, file.path("code"), projdir,
                                             overwrite = FALSE)
    }
}

# templatesfor subsequent years
# new_section_update <- function()
