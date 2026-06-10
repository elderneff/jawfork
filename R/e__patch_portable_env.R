#' e__patch_portable_env
#'
#' Silently patches the user's R-Portable environment and update scripts 
#' to prevent them from routing to the C: drive.
#'
#' @return TODO

e__patch_portable_env <- function() {
  # Only run if we are actually inside an R-Portable environment
  if (grepl("R-Portable", R.home(), ignore.case = TRUE)) {
    
    # --- 1. Patch Renviron.site ---
    etc_dir <- file.path(R.home(), "etc")
    renviron_path <- file.path(etc_dir, "Renviron.site")
    
    portable_lib <- normalizePath(file.path(R.home(), "library"), winslash = "/", mustWork = FALSE)
    portable_home <- normalizePath(R.home(), winslash = "/", mustWork = FALSE)
    
    env_lines <- c(
      paste0("R_LIBS_USER=", portable_lib),
      paste0("R_USER=", portable_home)
    )
    
    # Check if patching is needed
    needs_patch <- TRUE
    if (file.exists(renviron_path)) {
      current_env <- readLines(renviron_path, warn = FALSE)
      if (any(grepl("R_LIBS_USER", current_env))) needs_patch <- FALSE
    }
    
    if (needs_patch) {
      if (!dir.exists(etc_dir)) dir.create(etc_dir, recursive = TRUE, showWarnings = FALSE)
      # Append the environment variables
      cat(paste0(env_lines, collapse = "\n"), "\n", file = renviron_path, append = TRUE)
    }
    
    # --- 2. Patch the persistent update_jaw.bat ---
    # Assuming the bat file is 3 levels up from R.home(): Root/R-Portable/App/R-Portable
    root_dir <- sub("/R-Portable/App/R-Portable.*", "", normalizePath(R.home(), winslash = "/", mustWork = FALSE), ignore.case = TRUE)
    bat_path <- file.path(root_dir, "update_jaw.bat")
    
    if (file.exists(bat_path)) {
      current_bat <- readLines(bat_path, warn = FALSE)
      
      # If the batch file doesn't have the R_LIBS_USER override, overwrite it with the good version
      if (!any(grepl("R_LIBS_USER", current_bat))) {
        new_bat_lines <- c(
          "@echo off",
          "echo \"update jaw\"",
          "REM ****** Force the batch file to use the portable library instead of the C: drive ******",
          "set \"R_LIBS_USER=%cd%\\R-Portable\\App\\R-Portable\\library\"",
          "set \"R_USER=%cd%\\R-Portable\\App\\R-Portable\"",
          "REM ******Use %cd% variable to reference directory of update_jaw.bat******",
          "\"%cd%\\R-Portable\\App\\R-Portable\\bin\\Rscript.exe\" -e \"for(p in c('sqldf', 'haven', 'lubridate')) if(!requireNamespace(p, quietly=TRUE)) install.packages(p, repos='https://cloud.r-project.org')\" -e \"devtools::install_github('elderneff/jawfork', dependencies=FALSE, force=FALSE)\"",
          "@pause"
        )
        writeLines(new_bat_lines, bat_path)
      }
    }
  }
}
