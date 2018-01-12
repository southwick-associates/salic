# functions for making files, folders, etc.

# TODO: look this over. Anything more needed?
# update section 1 templates based on Iowa processing

#' Setup a new state dashboard with default directories and template scripts
#' 
#' This is intended to be run before data processing for a new state begins
#' @param state character: Two letter state designation
#' @param time_period character: Time period for the first dashboard (e.g., "2015", "2016-q1", etc.)
#' @param project_path character: File path where the Data Dashboards project is stored
#' @param R_version character: Version of R to use for this project
#' @param project_library character: Name of project-specific R package library
#' @family functions for making directories and files
#' @export
#' @examples
#' salic::new_dashboard("XX", "2017")
new_dashboard <- function(state, time_period, project_path = "E:/SA/Data-Dashboards",
                          R_version = "3.4.3", project_library = "data-dashboards") {
    
    # initial variable prep
    state <- toupper(state)
    time_period <- as.character(time_period)
    analysis_path <- file.path(project_path, "Analysis", state, time_period)
    
    # error - don't run if a directory with that time period already exists
    if (dir.exists(analysis_path)) {
        stop(paste("That time_period already exists!:", analysis_path))
    }
    
    # error - don't run if saproj isn't installed
    if (!requireNamespace("saproj", quietly = TRUE)) {
        stop("saproj needed for this function to work. Please install it.",
             call. = FALSE)
    }
    
    # make directories
    dir.create(analysis_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(project_path, "Data", state, paste0("raw-", time_period)), 
               recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(project_path, "Data2", state), showWarnings = FALSE)
    
    # copy project template to analysis_path
    template_paths <- list.files(system.file("new-state", package = "salic"),
                                 full.names = TRUE)
    for (i in template_paths) {
        file.copy(i, analysis_path, recursive = TRUE, overwrite = FALSE)
    }
    
    # make .Rprofile (using specified R version and project library)
    x <- readLines(system.file("misc", ".Rprofile", package = "saproj"))
    x[9] <- paste0("r_version <- '", R_version, "'")
    x[10] <- paste0("proj_libname <- '", project_library, "'")
    writeLines(x, file.path(analysis_path, ".Rprofile"))
    
    # make README & .Rproj (for RStudio)
    file.copy(system.file("new-state-Rfiles", "README.txt", package = "salic"), 
              file.path(analysis_path, "README.txt"))
    file.copy(system.file("new-state-Rfiles", "XX.Rproj", package = "salic"), 
              file.path(analysis_path, paste0(state, "-", time_period, ".Rproj")))
    
    # print message
    message("A new dashboard project has been initialized:\n  ", analysis_path)
    
}

update_project <- function() {}

