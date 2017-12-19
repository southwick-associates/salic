# functions for making files, folders, etc.
# some (maybe all?) will rely on package saproj

# TODO: new_state(), new_dashboard(), update_dashboard()

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

#' Setup a new state dashboard with default directories and template scripts
#' 
#' This is intended to be run before data processing for a new state begins
#' @param state character: Two letter state designation
#' @param time_period character: Time period for the first dashboard (e.g., "2015", "2016-q1", etc.)
#' @param proj_path character: File path where the Data Dashboards project is stored
#' @param R_version character: Version of R to use for this project
#' @param proj_library character: Name of project-specific R package library
#' @family functions for making directories and files
#' @export
#' @examples
#' salic::new_dashboard("XX", "2017")
new_dashboard <- function(state, time_period, proj_path = "E:/SA/Data-Dashboards",
                          R_version = "3.4.3", proj_library = "data-dashboards") {
    # don't run if a directory with that time period already exists
    state <- toupper(state)
    analysis_path <- file.path(proj_path, "Analysis", state, time_period)
    if (dir.exists(analysis_path)) {
        stop(paste("That time_period already exists!:", analysis_path))
    }
    # make directories
    dir.create(analysis_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(proj_path, "Data", state, "raw"), 
               recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(proj_path, "Data2", state), showWarnings = FALSE)
    
    # copy project template to analysis_path
    template_paths <- list.files(system.file("new-state", package = "salic"),
                                 full.names = TRUE)
    for (i in template_paths) {
        file.copy(i, analysis_path, recursive = TRUE, overwrite = FALSE)
    }
    
    # make .Rprofile
    # using specified R version and project library
    Rprofile_template <- readLines(system.file('new-state-Rfiles', ".Rprofile",
                                               package = "salic"))
    Rprofile_template[6] <- paste0("r_version <- '", R_version, "'")
    Rprofile_template[7] <- paste0("proj_libname <- '", proj_library, "'")
    writeLines(Rprofile_template, file.path(analysis_path, ".Rprofile"))
    
    # make .Rproj (for RStudio)
    Rproj_template <- readLines(system.file("new-state-Rfiles", "XX.Rproj", package = "salic"))
    writeLines(Rproj_template, file.path(analysis_path,
                                         paste0(state, "-", time_period, ".Rproj")))
}
