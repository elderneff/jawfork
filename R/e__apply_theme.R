#' e__apply_theme
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return TODO

# e__apply_theme <- function(session_name, outer_env = totem) {
#   is_dark <- outer_env[[session_name]]$status_bar$dark_mode
  
#   bg_color <- ifelse(is_dark, "#202020", "#FFFFFF")
#   text_color <- ifelse(is_dark, "#E0E0E0", "#000000")
#   frame_bg <- ifelse(is_dark, "#2D2D2D", "#F9F9F9")
#   entry_bg <- ifelse(is_dark, "#3D3D3D", "#FFFFFF")

#   if (is_dark) {
#     rc_style <- "
#       style 'jaw_dark' {
#         base[NORMAL]      = '#202020' 
#         base[INSENSITIVE] = '#2D2D2D'
#         bg[NORMAL]        = '#2D2D2D' 
#         bg[PRELIGHT]      = '#404040' 
#         bg[ACTIVE]        = '#1A1A1A' 
#         text[NORMAL]      = '#E0E0E0'
#         fg[NORMAL]        = '#E0E0E0' 
#         fg[PRELIGHT]      = '#FFFFFF'
#       }
#       style 'jaw_menu_light' {
#         bg[NORMAL]        = '#F0F0F0'
#         fg[NORMAL]        = '#000000'
#         text[NORMAL]      = '#000000'
#       }
#       class 'GtkEntry' style 'jaw_dark'
#       class 'GtkScrollbar' style 'jaw_dark'
#       class 'GtkTreeView' style 'jaw_dark'
#       class 'GtkFrame' style 'jaw_dark'
#       class 'GtkPaned' style 'jaw_dark'
#       class 'GtkScrolledWindow' style 'jaw_dark'
#       class 'GtkTextView' style 'jaw_dark'
#       class 'GtkButton' style 'jaw_dark'
#       class 'GtkLabel' style 'jaw_dark'
#       widget_class '*TreeView*Button*' style 'jaw_dark'
      
#       # Keep context menus legible by overriding their specific hierarchy
#       widget_class '*Menu*' style 'jaw_menu_light'
#       widget_class '*MenuItem*' style 'jaw_menu_light'
#     "
#   } else {
#     rc_style <- "
#       style 'jaw_light' {
#         base[NORMAL]      = '#FFFFFF' 
#         base[INSENSITIVE] = '#F1F1F1'
#         bg[NORMAL]        = '#F0F0F0' 
#         bg[PRELIGHT]      = '#E5E5E5' 
#         bg[ACTIVE]        = '#D4D4D4' 
#         text[NORMAL]      = '#000000'
#         fg[NORMAL]        = '#000000' 
#         fg[PRELIGHT]      = '#000000'
#       }
#       class 'GtkEntry' style 'jaw_light'
#       class 'GtkScrollbar' style 'jaw_light'
#       class 'GtkTreeView' style 'jaw_light'
#       class 'GtkFrame' style 'jaw_light'
#       class 'GtkPaned' style 'jaw_light'
#       class 'GtkScrolledWindow' style 'jaw_light'
#       class 'GtkTextView' style 'jaw_light'
#       class 'GtkButton' style 'jaw_light'
#       class 'GtkLabel' style 'jaw_light'
#       widget_class '*TreeView*Button*' style 'jaw_light'
#       widget_class '*Menu*' style 'jaw_light'
#       widget_class '*MenuItem*' style 'jaw_light'
#     "
#   }
  
#   RGtk2::gtkRcResetStyles(RGtk2::gtkSettingsGetDefault())
#   RGtk2::gtkRcParseString(rc_style)
  
#   RGtk2::gtkWidgetResetRcStyles(outer_env[[session_name]]$windows$main_window)
  
#   RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$windows$main_window, "normal", bg_color)
#   RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$main$main_box, "normal", bg_color)
#   RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$status_bar$frame, "normal", frame_bg)
#   RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$status_bar$box, "normal", frame_bg)
  
#   RGtk2::gtkWidgetModifyBase(outer_env[[session_name]]$text_area_1$View, "normal", entry_bg)
#   RGtk2::gtkWidgetModifyText(outer_env[[session_name]]$text_area_1$View, "normal", text_color)
#   RGtk2::gtkWidgetModifyBg(outer_env[[session_name]]$text_area_1$Frame, "normal", frame_bg)
  
#   entries_list <- list(
#     outer_env[[session_name]]$data_view_list$select_entry,
#     outer_env[[session_name]]$data_view_list$group_by_entry,
#     outer_env[[session_name]]$data_view_list$unique_by_entry,
#     outer_env[[session_name]]$status_bar$box_bucket_entry,
#     outer_env[[session_name]]$export_name_entry,
#     outer_env[[session_name]]$format_by_entry,
#     outer_env[[session_name]]$format_by_entry2
#   )
#   for (ent in entries_list) {
#     if (!is.null(ent)) {
#       RGtk2::gtkWidgetModifyBase(ent, "normal", entry_bg)
#       RGtk2::gtkWidgetModifyText(ent, "normal", text_color)
#     }
#   }
  
#   labels_list <- list(
#     outer_env[[session_name]]$status_bar$info_label,
#     outer_env[[session_name]]$status_bar$info_label_cell,
#     outer_env[[session_name]]$data_view_list$select_label,
#     outer_env[[session_name]]$data_view_list$group_by_label,
#     outer_env[[session_name]]$data_view_list$unique_by_label,
#     outer_env[[session_name]]$export_label,
#     outer_env[[session_name]]$format_by_label,
#     outer_env[[session_name]]$format_by_label2
#   )
#   for (lbl in labels_list) {
#     if (!is.null(lbl)) RGtk2::gtkWidgetModifyFg(lbl, "normal", text_color)
#   }
  
#   if (!is.null(outer_env[[session_name]]$data2)) {
#     outer_env[[session_name]]$data_view_list$slot1_list$full_table$update(outer_env[[session_name]]$data2)
#   }
#   if (!is.null(outer_env[[session_name]]$data3)) {
#     outer_env[[session_name]]$data_view_list$slot1_list$meta_table$update(outer_env[[session_name]]$data3)
#   }
#   if (!is.null(outer_env[[session_name]]$data_view_list$slot2_list$value_table)) {
#     current_df <- outer_env[[session_name]]$data_view_list$slot2_list$value_table$current_data()
#     if (!is.null(current_df)) {
#       outer_env[[session_name]]$data_view_list$slot2_list$value_table$update_table(current_df)
#     }
#   }
# }
