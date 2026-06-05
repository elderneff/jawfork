#' e__show_toast
#'
#' @param session_name TODO
#' @param message The text to display
#' @param duration_ms Time in milliseconds before the toast disappears
#' @param outer_env TODO
#'
#' @return TODO

e__show_toast <- function(session_name, message = "Code copied to clipboard!", duration_ms = 1000, outer_env = totem) {
  toast_win <- RGtk2::gtkWindowNew("toplevel")
  RGtk2::gtkWindowSetDecorated(toast_win, FALSE)
  RGtk2::gtkWindowSetResizable(toast_win, FALSE)
  RGtk2::gtkWindowSetKeepAbove(toast_win, TRUE)
  RGtk2::gtkWindowSetPosition(toast_win, 0L)
  
  event_box <- RGtk2::gtkEventBoxNew()
  bg_color <- RGtk2::gdkColorParse("#ffaec8")$color
  RGtk2::gtkWidgetModifyBg(event_box, RGtk2::GtkStateType["normal"], bg_color)
  RGtk2::gtkContainerAdd(toast_win, event_box)
  
  frame <- RGtk2::gtkFrameNew()
  RGtk2::gtkFrameSetShadowType(frame, "out")
  RGtk2::gtkContainerAdd(event_box, frame)
  
  vbox <- RGtk2::gtkVBoxNew(FALSE, 0)
  RGtk2::gtkContainerSetBorderWidth(vbox, 15)
  RGtk2::gtkContainerAdd(frame, vbox)
  
  label <- RGtk2::gtkLabelNew()
  markup <- paste0("<span size='large' weight='bold' foreground='#000000'>", message, "</span>")
  RGtk2::gtkLabelSetMarkup(label, markup)
  RGtk2::gtkBoxPackStart(vbox, label, TRUE, TRUE, 0)
  
  parent_window <- outer_env[[session_name]]$windows$main_window
  
  if (!is.null(parent_window)) {
    true_parent <- RGtk2::gtkWidgetGetToplevel(parent_window)
    RGtk2::gtkWindowSetTransientFor(toast_win, true_parent)
    
    p_pos <- RGtk2::gtkWindowGetPosition(true_parent)
    p_size <- RGtk2::gtkWindowGetSize(true_parent)
    
    # --- DIAGNOSTIC PRINTS ---
    cat("\n--- WINDOW POSITION STRUCTURE ---\n")
    str(p_pos)
    
    cat("\n--- WINDOW SIZE STRUCTURE ---\n")
    str(p_size)
    cat("---------------------------------\n\n")
    
  } else {
    RGtk2::gtkWindowSetPosition(toast_win, 1L) 
  }
  
  RGtk2::gtkWidgetShowAll(toast_win)
  RGtk2::gtkWindowPresent(toast_win)
  
  RGtk2::gTimeoutAdd(duration_ms, function(...) {
    RGtk2::gtkWidgetDestroy(toast_win)
    return(FALSE) 
  })
}
