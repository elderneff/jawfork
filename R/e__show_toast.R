#' e__show_toast
#'
#' @param session_name TODO
#' @param message The text to display
#' @param duration_ms Time in milliseconds before the toast disappears
#' @param outer_env TODO
#'
#' @return TODO

e__show_toast <- function(session_name, message = "Code copied to clipboard!", duration_ms = 5000, outer_env = totem) {
  toast_win <- RGtk2::gtkWindowNew("toplevel")
  RGtk2::gtkWindowSetDecorated(toast_win, FALSE)
  
  # 1. Shrink-wrap the window so it perfectly hugs the text
  RGtk2::gtkWindowSetResizable(toast_win, FALSE)
  
  parent_window <- outer_env[[session_name]]$windows$main_window
  
  if (!is.null(parent_window)) {
    # 2. Guarantee we have the absolute root window of the application
    true_parent <- RGtk2::gtkWidgetGetToplevel(parent_window)
    RGtk2::gtkWindowSetTransientFor(toast_win, true_parent)
    
    # 3. Use the raw C-integer to guarantee placement (4 = GTK_WIN_POS_CENTER_ON_PARENT)
    RGtk2::gtkWindowSetPosition(toast_win, 4)
  } else {
    # (1 = GTK_WIN_POS_CENTER)
    RGtk2::gtkWindowSetPosition(toast_win, 1)
  }
  
  frame <- RGtk2::gtkFrameNew()
  RGtk2::gtkFrameSetShadowType(frame, "out")
  RGtk2::gtkContainerAdd(toast_win, frame)
  
  vbox <- RGtk2::gtkVBoxNew(FALSE, 0)
  RGtk2::gtkContainerSetBorderWidth(vbox, 15)
  RGtk2::gtkContainerAdd(frame, vbox)
  
  label <- RGtk2::gtkLabelNew()
  # Optional: I added 'center' alignment to the markup just in case
  markup <- paste0("<span size='large' weight='bold' foreground='#2e8b57'>", message, "</span>")
  RGtk2::gtkLabelSetMarkup(label, markup)
  RGtk2::gtkBoxPackStart(vbox, label, TRUE, TRUE, 0)
  
  RGtk2::gtkWidgetShowAll(toast_win)
  
  RGtk2::gTimeoutAdd(duration_ms, function(...) {
    RGtk2::gtkWidgetDestroy(toast_win)
    return(FALSE) 
  })
}
