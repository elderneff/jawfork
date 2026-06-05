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

  if (is.null(remote_desc)) return(FALSE) # Silently fail if offline

  version_line <- grep("^Version:", remote_desc, value = TRUE)
  if (length(version_line) == 0) return(FALSE)

  remote_version <- trimws(sub("^Version:", "", version_line[1]))
  local_version <- as.character(packageVersion("jaw"))

  # 3. Mark that we checked today and save to disk
  # outer_env$settings_list$last_update_check <- as.character(today)
  # save_settings(outer_env)

  # 4. Compare versions
  if (utils::compareVersion(remote_version, local_version) > 0) {
    
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
      
      # Use R.home("bin") to dynamically locate the portable Rscript.exe
      rscript_path <- file.path(R.home("bin"), "Rscript.exe")
      
      # Build the update command. Use force=TRUE to ensure it overwrites
      update_cmd <- paste0('"', rscript_path, '" -e "devtools::install_github(\'elderneff/jawfork\', dependencies=F, force=TRUE)"')
      
      # Spawn a detached CMD window so the update can run independently
      sys_cmd <- paste0("start cmd.exe /c \"echo Updating jaw, do not close this window. & ", update_cmd, " & echo. & echo Update complete! & pause\"")
      system(sys_cmd, wait = FALSE)

      return(TRUE) # Return TRUE so we can abort the app startup
    }
  }
  
  return(FALSE)
}
