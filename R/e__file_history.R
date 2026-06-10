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

    #Pull latest disk settings to catch any files opened by parallel jaw sessions
    try({
      disk_settings <- readRDS(outer_env$local_settings_rds)
      if (is.list(disk_settings) && !is.null(disk_settings$file_history)) {

        #Safely apply column name migration
        disk_cols <- colnames(disk_settings$file_history)
        disk_cols[disk_cols == "mtime"] <- "modified"
        disk_cols[disk_cols == "load_time"] <- "loaded"
        disk_cols[disk_cols == "full_path"] <- "path"
        colnames(disk_settings$file_history) <- disk_cols

        #Merge with current environment
        merged_history <- rbind(outer_env$settings_list$file_history, disk_settings$file_history)

        #Heal dates
        bad_dates <- grepl("^[0-9]+\\.[0-9]+$|^[0-9]+$", merged_history$modified)
        if (any(bad_dates)) {
          merged_history$modified[bad_dates] <- format(as.POSIXct(as.numeric(merged_history$modified[bad_dates]), origin="1970-01-01"), "%Y-%m-%d %H:%M:%S")
        }

        #Sort by loaded descending
        merged_history <- merged_history[order(merged_history$loaded, decreasing = TRUE), ]

        #Force the desired column order
        merged_history <- merged_history[, c("dataset", "latest", "loaded", "modified", "path")]

        #Strip duplicates and overwrite the temporary variable
        merged_history <- merged_history[!duplicated(merged_history[, c("dataset", "path")]), ]

        #Cap at 200 records and assign to the global settings list
        outer_env$settings_list$file_history <- head(merged_history, 200)
        
        #Update the table UI to reflect the deduplicated list
        outer_env$file_history$file_history_window_table$update(outer_env$settings_list$file_history)
      }
    }, silent = TRUE)
    
    #Hide the load window after finishing
    outer_env$hide_load_window()
    return(FALSE)
  }, data = outer_env)
