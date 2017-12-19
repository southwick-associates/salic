# Define project-specific package library & R Version

local({
    
# Parameters
r_version <- "3.4.2"
proj_libname <- "southwick-packages"

# Setup Project Library (/Program Files/R/project-library/)
proj_libdir <- dirname(Sys.getenv("R_HOME"))
proj_libpath <- file.path(proj_libdir, "project-library", proj_libname)
if (!dir.exists(proj_libpath)) dir.create(proj_libpath, recursive = T)
.libPaths(proj_libpath)

# Startup Message
message("---Southwick Startup Message---")
message(paste("Project built using R Version", r_version))
message(paste("Project (user) Library:", .libPaths()[1]))
message(paste0("[", paste(list.files(proj_libpath), collapse = ", "), "]"))

})     
