#' e__apply_theme
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return TODO

e__apply_theme <- function(session_name = NULL, outer_env = totem) {
  if (is.null(session_name)) {
    is_dark <- outer_env$settings_list$dark_mode
  } else {
    is_dark <- outer_env[[session_name]]$status_bar$dark_mode
  }
  
  bg_color <- ifelse(is_dark, "#202020", "#F0F0F0")
  text_color <- ifelse(is_dark, "#E0E0E0", "#000000")
  frame_bg <- ifelse(is_dark, "#2D2D2D", "#F9F9F9")
  entry_bg <- ifelse(is_dark, "#3D3D3D", "#FFFFFF")

  if (is_dark) {
    rc_style <- "
      style 'jaw_dark' {
        engine \"\" {} 
        font_name = \"Segoe UI 9\"
        base[NORMAL]      = '#202020' 
        base[INSENSITIVE] = '#2D2D2D'
        bg[NORMAL]        = '#2D2D2D' 
        bg[PRELIGHT]      = '#404040' 
        bg[ACTIVE]        = '#1A1A1A' 
        text[NORMAL]      = '#E0E0E0'
        text[ACTIVE]      = '#FFFFFF'
        fg[NORMAL]        = '#E0E0E0' 
        fg[PRELIGHT]      = '#FFFFFF'
        fg[ACTIVE]        = '#FFFFFF'
      }
      style 'jaw_menu_light' {
        engine \"\" {}
        font_name = \"Segoe UI 9\"
        bg[NORMAL]        = '#F0F0F0'
        fg[NORMAL]        = '#000000'
        text[NORMAL]      = '#000000'
      }
      
      widget_class '*' style 'jaw_dark'
      widget_class '*Menu*' style 'jaw_menu_light'
      widget_class '*MenuItem*' style 'jaw_menu_light'
    "
  } else {
    rc_style <- "
      style 'jaw_light' {
        engine \"wimp\" {}
        font_name = \"Segoe UI 9\"
        base[NORMAL]      = '#FFFFFF' 
        base[INSENSITIVE] = '#F1F1F1'
        bg[NORMAL]        = '#F0F0F0' 
        bg[PRELIGHT]      = '#E5E5E5' 
        bg[ACTIVE]        = '#D4D4D4' 
        text[NORMAL]      = '#000000'
        text[ACTIVE]      = '#000000'
        fg[NORMAL]        = '#000000' 
        fg[PRELIGHT]      = '#000000'
        fg[ACTIVE]        = '#000000'
      }
      
      widget_class '*' style 'jaw_light'
      widget_class '*Menu*' style 'jaw_light'
      widget_class '*MenuItem*' style 'jaw_light'
    "
  }
  
  RGtk2::gtkRcResetStyles(RGtk2::gtkSettingsGetDefault())
  RGtk2::gtkRcParseString(rc_style)

  #Reset styles recursively
  reset_rc_recursive <- function(widget) {
    RGtk2::gtkWidgetResetRcStyles(widget)
    if (inherits(widget, "GtkContainer")) {
      for (child in RGtk2::gtkContainerGetChildren(widget)) {
        reset_rc_recursive(child)
      }
    }
  }
  
  # Now safely apply recursive resets
  if (!is.null(session_name)) {
    reset_rc_recursive(outer_env[[session_name]]$windows$main_window)
  }
  if (!is.null(outer_env$settings_window$settings_window)) {
    reset_rc_recursive(outer_env$settings_window$settings_window)
  }
  if (!is.null(outer_env$file_history$file_history_window)) {
    reset_rc_recursive(outer_env$file_history$file_history_window)
    # Explicitly color the File History window backgrounds
    RGtk2::gtkWidgetModifyBg(outer_env$file_history$file_history_window, "normal", bg_color)
    RGtk2::gtkWidgetModifyBg(outer_env$file_history$file_history_window_main_box, "normal", bg_color)
    if (!is.null(outer_env$file_history$file_history_window_main_new_path_box)) {
      RGtk2::gtkWidgetModifyBg(outer_env$file_history$file_history_window_main_new_path_box, "normal", bg_color)
    }
  }
  
  # Wrap session-specific UI modifiers
  if (!is.null(session_name)) {
    RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$windows$main_window, "normal", bg_color)
    RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$main$main_box, "normal", bg_color)
    RGtk2::gtkWidgetModifyBg(outer_env$load_window, "normal", "#F0F0F0")
    RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$status_bar$frame, "normal", frame_bg)
    RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$status_bar$box, "normal", frame_bg)
    
    RGtk2::gtkWidgetModifyBase(outer_env[[session_name]]$text_area_1$View, "normal", entry_bg)
    RGtk2::gtkWidgetModifyText(outer_env[[session_name]]$text_area_1$View, "normal", text_color)
    RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$text_area_1$Frame, "normal", frame_bg)
    
    entries_list <- list(
      outer_env[[session_name]]$data_view_list$select_entry,
      outer_env[[session_name]]$data_view_list$group_by_entry,
      outer_env[[session_name]]$data_view_list$unique_by_entry,
      outer_env[[session_name]]$status_bar$box_bucket_entry,
      outer_env[[session_name]]$export_name_entry,
      outer_env[[session_name]]$format_by_entry,
      outer_env[[session_name]]$format_by_entry2
    )
    for (ent in entries_list) {
      if (!is.null(ent)) {
        RGtk2::gtkWidgetModifyBase(ent, "normal", entry_bg)
        RGtk2::gtkWidgetModifyText(ent, "normal", text_color)
      }
    }
    
    labels_list <- list(
      outer_env[[session_name]]$status_bar$info_label,
      outer_env[[session_name]]$status_bar$info_label_cell,
      outer_env[[session_name]]$data_view_list$select_label,
      outer_env[[session_name]]$data_view_list$group_by_label,
      outer_env[[session_name]]$data_view_list$unique_by_label,
      outer_env[[session_name]]$export_label,
      outer_env[[session_name]]$format_by_label,
      outer_env[[session_name]]$format_by_label2
    )
    for (lbl in labels_list) {
      if (!is.null(lbl)) RGtk2::gtkWidgetModifyFg(lbl, "normal", text_color)
    }

    #Push colors to different objects
    if (!is.null(outer_env[[session_name]]$data1) && !is.null(outer_env[[session_name]]$data2)) {
      is_select_active <- RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$select_cb)
      select_txt <- trimws(RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry))
      has_select_subset <- is_select_active && (select_txt != "")

      is_subset <- (nrow(outer_env[[session_name]]$data1) != nrow(outer_env[[session_name]]$data2)) ||
                   (ncol(outer_env[[session_name]]$data1) != ncol(outer_env[[session_name]]$data2)) ||
                   has_select_subset

      if (is_subset) {
        dim_bg <- ifelse(is_dark, "#5C2E2E", "#F4D9D9")
      } else {
        dim_bg <- ifelse(is_dark, "#202020", "#F0F0F0")
      }
      
      if (!is.null(outer_env[[session_name]]$data_view_list$code_tool_bar_dim_eb)) {
        RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$data_view_list$code_tool_bar_dim_eb, "normal", dim_bg)
      }
    }
    if (!is.null(outer_env[[session_name]]$data2)) {
      outer_env[[session_name]]$data_view_list$slot1_list$full_table$update(outer_env[[session_name]]$data2)
    }
    if (!is.null(outer_env[[session_name]]$data3)) {
      outer_env[[session_name]]$data_view_list$slot1_list$meta_table$update(outer_env[[session_name]]$data3)
    }
    if (!is.null(outer_env[[session_name]]$data_view_list$slot2_box)) {
      RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$slot2_box)
    }
  }
}
