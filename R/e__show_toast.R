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
  
  # 1. Force the window to float on top of everything else
  RGtk2::gtkWindowSetKeepAbove(toast_win, TRUE)
  
  parent_window <- outer_env[[session_name]]$windows$main_window
  
  if (!is.null(parent_window)) {
    true_parent <- RGtk2::gtkWidgetGetToplevel(parent_window)
    RGtk2::gtkWindowSetTransientFor(toast_win, true_parent)
    
    # FIX: Append 'L' to force R to pass a strict integer to the C backend
    RGtk2::gtkWindowSetPosition(toast_win, 4L) 
  } else {
    # FIX: Append 'L' here as well
    RGtk2::gtkWindowSetPosition(toast_win, 1L) 
  }
  
  frame <- RGtk2::gtkFrameNew()
  RGtk2::gtkFrameSetShadowType(frame, "out")
  RGtk2::gtkContainerAdd(toast_win, frame)
  
  vbox <- RGtk2::gtkVBoxNew(FALSE, 0)
  RGtk2::gtkContainerSetBorderWidth(vbox, 15)
  RGtk2::gtkContainerAdd(frame, vbox)
  
  label <- RGtk2::gtkLabelNew()
  markup <- paste0("<span size='large' weight='bold' foreground='#2e8b57'>", message, "</span>")
  RGtk2::gtkLabelSetMarkup(label, markup)
  RGtk2::gtkBoxPackStart(vbox, label, TRUE, TRUE, 0)
  
  # Show the window and all its children
  RGtk2::gtkWidgetShowAll(toast_win)
  
  # 2. Forcefully command the window manager to draw it right now
  RGtk2::gtkWindowPresent(toast_win)
  
  RGtk2::gTimeoutAdd(duration_ms, function(...) {
    RGtk2::gtkWidgetDestroy(toast_win)
    return(FALSE) 
  })
}
