# functions for making files, folders, etc.

#' Setup a new state dashboard with default directories and template scripts
#' 
#' This is intended to be run before data processing for a new state begins
#' @param state character: Two letter state designation
#' @param time_period character: Time period for the first dashboard 
#' (e.g., "2015", "2016-q1", etc.)
#' @param sa_path character: File path to the Southwick main folder 
#' (for analysis and data, etc.)
#' @param R_version character: Version of R to use for this project
#' @param project_library character: Name of project-specific R package library
#' @family functions for making directories and files
#' @export
#' @examples
#' salic::new_dashboard("XX", "2017")
new_dashboard <- function(
    state, time_period, sa_path = "E:/SA", 
    R_version = "3.5.1", project_library = "data-dashboards"
) {
    
    # initial variable prep
    state <- toupper(state)
    time_period <- as.character(time_period)
    analysis_path <- file.path(sa_path, "Projects", "Data-Dashboards",
                               state, time_period)
    
    # error - don't run if a directory with that time period already exists
    if (dir.exists(analysis_path)) {
        stop(paste("That time_period already exists!:", analysis_path))
    }
    
    # error - don't run if saproj isn't installed
    if (!requireNamespace("saproj", quietly = TRUE)) {
        stop("saproj needed for this function to work. Please install it.",
             call. = FALSE)
    }
    
    # make analysis folders
    dir.create(analysis_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(analysis_path, "data"), showWarnings = FALSE)
    dir.create(file.path(analysis_path, "out"), showWarnings = FALSE)
    
    # make data folders
    dir.create(file.path(
        sa_path, "Data-sensitive", "Data-Dashboards", state, paste0("raw-", time_period)), 
        recursive = TRUE, showWarnings = FALSE
    )
    dir.create(file.path(sa_path, "Data-production", "Data-Dashboards", state), 
               showWarnings = FALSE)
    
    # copy project template files to analysis_path
    template_paths <- list.files(system.file("state-template", package = "salic"),
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
    
    # replace placeholder strings in Rmd templates
    # doing this saves the analyst some time and helps enforce naming conventions
    replace_strings(analysis_path, "__state__", state, showmessage = FALSE)
    replace_strings(analysis_path, "__period__", time_period, showmessage = FALSE)
    
    # print message
    message("A new dashboard project has been initialized:\n  ", analysis_path)
    
}

# TODO - for future dashboards
# copy existing code with replacements (__period__, first = FALSE)
update_dashboard <- function() {}

#' Search and replace string across files with R
#' 
#' This is a helper function for preparing templates in salic. It uses 
#' \code{\link[base]{gsub}} (https://gist.github.com/mages/1544009)
#' @inheritParams base::list.files
#' @param find_string character: String (to find) that will be replaced
#' @param replacement_string character: New string to use
#' @param showmessage logical: If TRUE, prints a message about replacement
#' @keywords internal
#' @export
#' @examples
#' salic::replace_strings()
replace_strings <- function(
    path = ".", find_string, replacement_string, pattern = ".Rmd", showmessage = TRUE
) {
    
    # get file names in which to apply replacement
    filenames <- list.files(path, pattern = pattern,  
                            full.names = TRUE, recursive = TRUE)
    
    # stop with error if no filenames in path match pattern (i.e., no files to replace)
    if (length(filenames) == 0) {
        stop(
            "There are no files matching pattern '", pattern, "' in:\n  ",
            normalizePath(path), call. = FALSE
        )    
    }
    
    # Replace find_string with replacement_string
    for( f in filenames ) {
        x <- readLines(f)
        y <- gsub( find_string, replacement_string, x)
        cat(y, file=f, sep="\n")
    }
    
    # Print output message about replacements
    if (showmessage) {
        message(
            "Occurences of '", find_string, "' have been replaced with '",
            replacement_string, "' in '", pattern, "' files at:\n  ", 
            normalizePath(path)
        )
    }
}



