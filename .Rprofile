# Defines project-specific package library & R Version

# EDIT AT YOUR OWN RISK!!!
# project parameters can be safely changed with saproj::update_project()

local({
    
# Project-specific Parameters
r_version <- '3.5.1'
proj_libname <- 'southwick-packages'


# Project-agnostic Setup Code ---------------------------------------------

# 1. Prepare Project Library
proj_libpath <- file.path(Sys.getenv("R_HOME"), "project-library", proj_libname)
if (!dir.exists(proj_libpath)) dir.create(proj_libpath, recursive = T)
.libPaths(proj_libpath)

# 2. Print Startup Message
# which shows status of snapshot-library
comparison_outcome <- saproj::compare_library_snapshot(proj_libpath)
cat(paste0(
    "Project Package Library: '", proj_libname, "'\n",
    comparison_outcome[[1]], "\n\n"
))

# show the detailed comparison if there is disagreement
if (names(comparison_outcome[1]) %in% c("conflicts", "snapshot_behind", "library_behind")) {
    df <- comparison_outcome[[2]]
    print(df[df$in_library != df$in_snapshot,])
}

# 3. Print Warning if project R version doesn't match currently loaded R version
r_current_version <- paste(R.version$major, R.version$minor, sep = ".")
if (!(r_version == r_current_version)) {
    msg <- paste(
        "--- Southwick Warning ---\nPlease use R version", r_version,  
        "for this project", "\n  In Rstudio: Tools > Global Options > R Version", 
        "\n\nIf you don't have this version installed, you can download it from", 
        "the 'R Software' Office 365 group:\n", paste0(" Installations/", r_version, ".zip")
    )
    message(paste0("\n", msg))
    
    # Also use a popup warning on windows systems
    if (Sys.info()[["sysname"]] == "Windows") utils::winDialog(type = "ok", message = msg)
}


})     

