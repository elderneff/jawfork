#' e__file_history
#'
#' @param outer_env TODO
#'
#' @return TODO

e__file_history <- function(outer_env=totem) {
  outer_env$file_history <- list()
  outer_env$file_history$file_history_window <- RGtk2::gtkWindow(show = F)


  outer_env$file_history$file_history_window_main_box <- RGtk2::gtkVBox()
  RGtk2::gtkContainerAdd(outer_env$file_history$file_history_window, outer_env$file_history$file_history_window_main_box)



  outer_env$file_history$file_history_window_main_new_path_box <- RGtk2::gtkHBox()

  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_box, outer_env$file_history$file_history_window_main_new_path_box, F, F)

  outer_env$file_history$file_history_window_main_new_path_chk_btn <- RGtk2::gtkButton()
  RGtk2::gtkContainerAdd(outer_env$file_history$file_history_window_main_new_path_chk_btn, RGtk2::gtkImageNewFromStock("gtk-refresh", size = 2L))
  RGtk2::gtkWidgetSetTooltipText(outer_env$file_history$file_history_window_main_new_path_chk_btn, "Check if files have been modified on disk")
  RGtk2::gtkButtonSetFocusOnClick(outer_env$file_history$file_history_window_main_new_path_chk_btn, F)
  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, outer_env$file_history$file_history_window_main_new_path_chk_btn, F, F, padding = 5)


  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, RGtk2::gtkLabel("Path to file: "), F, F, padding = 5)

  outer_env$file_history$file_history_window_main_new_path_entry <- RGtk2::gtkEntry()
  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, outer_env$file_history$file_history_window_main_new_path_entry, T, T, padding = 1)

  outer_env$file_history$file_history_window_main_new_path_btn <- RGtk2::gtkButton("load")
  RGtk2::gtkButtonSetFocusOnClick(outer_env$file_history$file_history_window_main_new_path_btn, F)
  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, outer_env$file_history$file_history_window_main_new_path_btn, F, F, padding = 5)




  RGtk2::gSignalConnect(outer_env$file_history$file_history_window_main_new_path_chk_btn, "button-press-event",
    function(widget, event, data) {
      outer_env$show_load_window()
      outer_env <- data
      
      # 1. Pull latest disk settings to catch any files opened by parallel jaw sessions
      try({
        disk_settings <- readRDS(outer_env$local_settings_rds)
        if (is.list(disk_settings) && !is.null(disk_settings$file_history)) {
          
          # Safely apply column name migration
          disk_cols <- colnames(disk_settings$file_history)
          disk_cols[disk_cols == "mtime"] <- "modified"
          disk_cols[disk_cols == "load_time"] <- "loaded"
          disk_cols[disk_cols == "full_path"] <- "path"
          colnames(disk_settings$file_history) <- disk_cols

          # Merge with current environment
          merged_history <- rbind(outer_env$settings_list$file_history, disk_settings$file_history)
          
          # HEAL DATES: Find Unix timestamps and convert them back to readable strings
          bad_dates <- grepl("^[0-9]+\\.[0-9]+$|^[0-9]+$", merged_history$modified)
          if (any(bad_dates)) {
            merged_history$modified[bad_dates] <- format(as.POSIXct(as.numeric(merged_history$modified[bad_dates]), origin="1970-01-01"), "%Y-%m-%d %H:%M:%S")
          }
          
          # SORT: Order by 'loaded' descending so newest files are at the top
          merged_history <- merged_history[order(merged_history$loaded, decreasing = TRUE), ]
          
          # FORCE THE DESIRED COLUMN ORDER
          merged_history <- merged_history[, c("dataset", "latest", "loaded", "modified", "path")]
          
          # STRIP DUPLICATES: Keep the most recent record
          outer_env$settings_list$file_history <- merged_history[!duplicated(merged_history[, c("dataset", "path")]), ]
        }
      }, silent = TRUE)

      # 2. Check all files for their 'latest' state
      file_history <- outer_env$settings_list$file_history
      for (i in seq_len(nrow(file_history))) {
        file_history[i, "latest"] <- NA
        try({
          # Ensure we format the file.info check the exact same way to match correctly
          current_mtime <- format(file.info(file_history[i, "path"], extra_cols = TRUE)$mtime, "%Y-%m-%d %H:%M:%S")
          file_history[i, "latest"] <- (file_history[i, "modified"] == current_mtime)
        }, silent = TRUE)
      }

      outer_env$settings_list$file_history <- file_history
      outer_env$file_history$file_history_window_table$update(file_history)
      outer_env$hide_load_window()

      return(FALSE)
    },
    data = outer_env
  )


  RGtk2::gSignalConnect(outer_env$file_history$file_history_window_main_new_path_btn, "button-press-event",
    function(widget, event, data) {
      outer_env <- data
      sas_path <- RGtk2::gtkEntryGetText(outer_env$file_history$file_history_window_main_new_path_entry)


      sas_path <- gsub("\"", "", gsub("\\\\", "/", sas_path))
      outer_env$start(sas_path)
      # outer_env$hide_file_history_window()

      return(FALSE)
    },
    data = outer_env
  )






  RGtk2::gtkWindowSetTitle(outer_env$file_history$file_history_window, "File History")
  RGtk2::gtkWidgetSetSizeRequest(outer_env$file_history$file_history_window, 600, 600)


  RGtk2::gSignalConnect(outer_env$file_history$file_history_window, "delete-event", f = function(window, event, data) {
    outer_env <- data
    outer_env$hide_file_history_window()
    
    # If no sessions are currently open, kill the R process completely
    if (length(outer_env$all_sessions) == 0) {
      quit(save = "no")
    }
    
    return(T)
  }, data = outer_env)



  outer_env$file_history$file_history_window_table <- outer_env$u__df_tree(
    session_name = "file_history",
    passed_box = outer_env$file_history$file_history_window_main_box,
    rows_length = 1000,
    event_mapping = NULL,
    style_list = list(value = RGtk2::pangoFontDescriptionFromString("bold 10")),
    is_value_table = F, is_meta_table = F, is_data_code_table = F, is_file_history_table = T
  )
}
