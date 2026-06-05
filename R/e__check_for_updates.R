#' e__check_for_updates
#'
#' @param outer_env TODO
#'
#' @return Boolean indicating if an update was initiated
e__check_for_updates <- function(outer_env = totem) {
  # 1. Check if we've already looked for updates today
  last_check <- as.Date(outer_env$settings_list$last_update_check)
  today <- Sys.Date()
  if (last_check >= today) {
    return(FALSE)
  }

  # 2. Safely fetch remote version from GitHub
  remote_desc <- tryCatch({
    readLines("https://raw.githubusercontent.com/elderneff/jawfork/main/DESCRIPTION", warn = FALSE)
  }, error = function(e) return(NULL))

  if (is.null(remote_desc)) return(FALSE)

  version_line <- grep("^Version:", remote_desc, value = TRUE)
  if (length(version_line) == 0) return(FALSE)

  remote_version <- trimws(sub("^Version:", "", version_line[1]))
  local_version <- as.character(packageVersion("jaw"))

  # 3. Mark that we checked today and save to disk
  outer_env$settings_list$last_update_check <- as.character(today)
  save_settings(outer_env)

  # 4. Compare versions
  if (remote_version != local_version) {
    
    # 5. Prompt the user
    dialog <- RGtk2::gtkMessageDialog(
      parent = NULL,
      flags = "destroy-with-parent",
      type = "question",
      buttons = "yes-no",
      paste0("A new version of jaw (", remote_version, ") is available!\n\nYou are currently using version ", local_version, ".\n\nWould you like to update now? (jaw will close to apply the update)")
    )
    response <- dialog$run()
    RGtk2::gtkWidgetDestroy(dialog)

    # If the user clicks Yes
    if (response == RGtk2::GtkResponseType["yes"] || response == -8) {
      
      # Use R.home("bin") and normalize it for Windows CMD
      rscript_path <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), mustWork = FALSE)
      
      # Grab the exact directory jaw is currently loaded from and normalize slashes for the R command
      current_lib <- normalizePath(dirname(find.package("jaw")), winslash = "/", mustWork = FALSE)
      
      bat_file <- file.path(Sys.getenv("TEMP"), paste0("jaw_update_", as.integer(Sys.time()), ".bat"))
      
      bat_lines <- c(
        "@echo off",
        "echo Updating jaw, do not close this window.",
        "echo ---------------------------------------",
        "echo.",
        paste0('"', rscript_path, '" -e "devtools::install_github(\'elderneff/jawfork\', dependencies=F, force=TRUE, lib=\'', current_lib, '\')"'),
        "echo.",
        "echo Update complete! Press any key to close this window.",
        "pause >nul",
        "del \"%~f0\" & exit"
      )
      writeLines(bat_lines, bat_file)
      
      # Launch the batch file using shell()
      shell(paste0('start "jaw Updater" "', bat_file, '"'), wait = FALSE)

      # Give Windows a split-second to successfully spawn the independent window
      Sys.sleep(1)

      # IMMEDIATELY kill the current R session so the package files are unlocked!
      quit(save = "no")
    }
  }
  
  return(FALSE)
}
