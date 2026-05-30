#' e__close_all_windows
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return TODO

e__close_all_windows <- function(session_name,outer_env=totem) {
  totem$all_sessions <- setdiff(totem$all_sessions, session_name)
  if (length(totem$all_sessions) == 0) {
    # try({
    #   gSourceRemove(totem$loop_function_obj)
    #   totem$loop_function_obj <- F
    #   message("Jaw shut down")
    # })
    # assign("totem_running", F, envir = .GlobalEnv)
    outer_env$while_loop_running <- F
  }
  # outer_env[[session_name]]$dialog$main_content_window$destroy()


  # Harvest the current window states
  main_position <- RGtk2::gtkPanedGetPosition(outer_env[[session_name]]$data_view_list$main_paned)
  top_position <- RGtk2::gtkPanedGetPosition(outer_env[[session_name]]$data_view_list$top_paned)
  slot_position <- RGtk2::gtkPanedGetPosition(outer_env[[session_name]]$data_view_list$paned)
  simplicity_view <- outer_env[[session_name]]$status_bar$simplicity_view
  allocation <- RGtk2::gtkWidgetGetAllocation(outer_env[[session_name]]$windows$main_window)$allocation
  window_size <- c(allocation$width, allocation$height)

  initial <- outer_env[[session_name]]$initial_sizes

  # --- CRITICAL FIX: Cross-Process Memory Sync ---
  # Pull the absolute latest sizes from the hard drive immediately before evaluating changes.
  # This prevents an older session from overwriting a newer session's settings!
  try({
    disk_settings <- readRDS(outer_env$local_settings_rds)
    if (is.list(disk_settings) && !is.null(disk_settings$default_sizes)) {
      outer_env$settings_list$default_sizes <- disk_settings$default_sizes
      outer_env$settings_list$simplicity <- disk_settings$simplicity
    }
  }, silent = TRUE)

  if (!is.null(initial)) {
    # GTK often shifts panes/windows by 1-2 pixels during rendering. 
    # We require a difference of > 5 pixels to count as a deliberate user resize.
    if (abs(main_position - initial$main_pane) > 5) outer_env$settings_list$default_sizes$main_pane <- main_position
    if (abs(top_position - initial$top_pane) > 5) outer_env$settings_list$default_sizes$top_pane <- top_position
    if (abs(slot_position - initial$slot_pane) > 5) outer_env$settings_list$default_sizes$slot_pane <- slot_position
    if (simplicity_view != initial$simplicity) outer_env$settings_list$simplicity <- simplicity_view
    
    if (abs(window_size[1] - initial$window[1]) > 5 || abs(window_size[2] - initial$window[2]) > 5) {
      outer_env$settings_list$default_sizes$window <- window_size
    }
  }

  # Always save settings to disk on close to ensure history/keybinds are preserved
  save_settings(outer_env)

  # Destroy the windows
  RGtk2::gtkWidgetDestroy(outer_env[[session_name]]$windows$main_window)
  RGtk2::gtkWidgetDestroy(outer_env[[session_name]]$past_code_window)
  outer_env[[session_name]] <- NULL
}
