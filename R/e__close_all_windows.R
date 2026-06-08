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
  try({
    disk_settings <- readRDS(outer_env$local_settings_rds)
    if (is.list(disk_settings)) {
      
      # 1. Sync global checkboxes and keybinds
      for (setting_name in c("maximize", "ctrlshift", "columnlabel", "columnunique", "professionalloading", "table_events")) {
        # If this session DID NOT change the setting from its initial state...
        if (identical(outer_env$settings_list[[setting_name]], outer_env[[session_name]]$initial_settings_snapshot[[setting_name]])) {
          # ...safely inherit the latest version from the disk
          if (!is.null(disk_settings[[setting_name]])) {
            outer_env$settings_list[[setting_name]] <- disk_settings[[setting_name]]
          }
        }
      }

      # 2. Merge running histories so we don't drop entries from other concurrent sessions
      if (!is.null(disk_settings$file_history)) {
        
        # Ensure the disk settings are migrated before merging
        disk_cols <- colnames(disk_settings$file_history)
        disk_cols[disk_cols == "mtime"] <- "modified"
        disk_cols[disk_cols == "load_time"] <- "loaded"
        disk_cols[disk_cols == "full_path"] <- "path"
        colnames(disk_settings$file_history) <- disk_cols

        merged_history <- rbind(outer_env$settings_list$file_history, disk_settings$file_history)
        
        # Sort descending before stripping duplicates!
        merged_history <- merged_history[order(merged_history$loaded, decreasing = TRUE), ]

        # Cap at 200 records
        outer_env$settings_list$file_history <- head(merged_history, 200)
        
        # FORCE THE DESIRED COLUMN ORDER
        merged_history <- merged_history[, c("dataset", "latest", "loaded", "modified", "path")]
        
        outer_env$settings_list$file_history <- merged_history[!duplicated(merged_history[, c("dataset", "path")]), ]
      }
      
      if (!is.null(disk_settings$previous_code)) {
        merged_code <- rbind(outer_env$settings_list$previous_code, disk_settings$previous_code)
        
        # Sort by time descending to keep newest code at the top
        merged_code <- merged_code[order(merged_code$time, decreasing = TRUE), ]
        merged_code <- merged_code[duplicated(merged_code[, -1]) == F, ]
    
        # CAP AT 500 ENTRIES
        outer_env$settings_list$previous_code <- head(merged_code, 500)
      }

      # 3. Handle window sizes
      if (!is.null(disk_settings$default_sizes)) {
        outer_env$settings_list$default_sizes <- disk_settings$default_sizes
        outer_env$settings_list$simplicity <- disk_settings$simplicity
      }
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
