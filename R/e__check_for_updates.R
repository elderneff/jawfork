#' e__check_for_updates
#'
#' @param outer_env TODO
#'
#' @return Boolean indicating if an update was initiated
e__check_for_updates <- function(outer_env = totem) {
  print("Checking for updates...")
  
  # 1. Check if we've already looked for updates today
  last_check <- as.Date(outer_env$settings_list$last_update_check)
  print(last_check)
  today <- Sys.Date()
  if (last_check >= today) {
    return(FALSE)
  }

  # 2. Safely fetch remote version from GitHub
  remote_desc <- tryCatch({
    readLines("https://raw.githubusercontent.com/elderneff/jawfork/main/DESCRIPTION", warn = FALSE)
  }, error = function(e) return(NULL))

  if (is.null(remote_desc)) return(FALSE) # Silently fail if offline

  version_line <- grep("^Version:", remote_desc, value = TRUE)
  if (length(version_line) == 0) return(FALSE)

  remote_version <- trimws(sub("^Version:", "", version_line[1]))
  print(remote_version)
  local_version <- as.character(packageVersion("jaw"))
  print(local_version)

  # 3. Mark that we checked today and save to disk
  # outer_env$settings_list$last_update_check <- as.character(today)
  # save_settings(outer_env)

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
    
    # Use R.home("bin") and normalize it for Windows CMD (converts / to \)
    rscript_path <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), mustWork = FALSE)
    
    # CRITICAL FIX: Do NOT use R's tempfile() because R deletes its temp folder on exit!
    # Write directly to the system's global Temp folder so it survives R shutting down.
    bat_file <- file.path(Sys.getenv("TEMP"), paste0("jaw_update_", as.integer(Sys.time()), ".bat"))
    
    bat_lines <- c(
      "@echo off",
      "echo Updating jaw, do not close this window.",
      paste0('"', rscript_path, '" -e "devtools::install_github(\'elderneff/jawfork\', dependencies=F, force=TRUE)"'),
      "echo.",
      "echo Update complete! You can now relaunch JAW.",
      "pause",
      paste0('del "', bat_file, '"') # Have the script delete itself at the very end
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
