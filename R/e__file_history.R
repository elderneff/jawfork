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

  outer_env$file_history$file_history_window_main_new_path_chk_btn <- RGtk2::gtkButton("Check")
  RGtk2::gtkButtonSetFocusOnClick(outer_env$file_history$file_history_window_main_new_path_chk_btn, F)
  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, outer_env$file_history$file_history_window_main_new_path_chk_btn, F, F, padding = 5)


  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, RGtk2::gtkLabel("New Path"), F, F, padding = 5)

  outer_env$file_history$file_history_window_main_new_path_entry <- RGtk2::gtkEntry()
  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, outer_env$file_history$file_history_window_main_new_path_entry, T, T, padding = 1)

  outer_env$file_history$file_history_window_main_new_path_btn <- RGtk2::gtkButton("load")
  RGtk2::gtkButtonSetFocusOnClick(outer_env$file_history$file_history_window_main_new_path_btn, F)
  RGtk2::gtkBoxPackStart(outer_env$file_history$file_history_window_main_new_path_box, outer_env$file_history$file_history_window_main_new_path_btn, F, F, padding = 5)




  RGtk2::gSignalConnect(outer_env$file_history$file_history_window_main_new_path_chk_btn, "button-press-event",
    function(widget, event, data) {
      outer_env$show_load_window()
      outer_env <- data
      file_history <- outer_env$settings_list$file_history
      for (i in seq_len(nrow(file_history))) {
        file_history[i, "latest"] <- NA
        try({
          file_history[i, "latest"] <- (file_history[i, "mtime"] == file.info(file_history[i, "full_path"], extra_cols = TRUE)$mtime)
        })
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
      outer_env$hide_file_history_window()

      return(FALSE)
    },
    data = outer_env
  )






  RGtk2::gtkWindowSetTitle(outer_env$file_history$file_history_window, "File History")
  RGtk2::gtkWidgetSetSizeRequest(outer_env$file_history$file_history_window, 600, 600)


  RGtk2::gSignalConnect(outer_env$file_history$file_history_window, "delete-event", f = function(window, event, data) {
    outer_env <- data
    outer_env$hide_file_history_window()
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
