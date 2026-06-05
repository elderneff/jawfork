#' e__show_toast
#'
#' @param session_name TODO
#' @param message The text to display
#' @param duration_ms Time in milliseconds before the toast disappears
#' @param outer_env TODO
#'
#' @return TODO

e__show_toast <- function(session_name, message = "Code copied to clipboard!", duration_ms = 1000, outer_env = totem) {
  # 1. Create a TOPLEVEL window (Popups ignore position commands!)
  toast_win <- RGtk2::gtkWindowNew("toplevel")
  
  # Strip away the title bar and X button to make it a frameless toast
  RGtk2::gtkWindowSetDecorated(toast_win, FALSE)
  
  parent_window <- outer_env[[session_name]]$windows$main_window
  
  # 2. Position it relative to the parent using the simple strings
  if (!is.null(parent_window)) {
    RGtk2::gtkWindowSetTransientFor(toast_win, parent_window)
    RGtk2::gtkWindowSetPosition(toast_win, "center-on-parent")
  } else {
    RGtk2::gtkWindowSetPosition(toast_win, "center")
  }
  
  # 3. Add a decorative frame and padding so it looks like a nice notification
  frame <- RGtk2::gtkFrameNew()
  RGtk2::gtkFrameSetShadowType(frame, "out")
  RGtk2::gtkContainerAdd(toast_win, frame)
  
  vbox <- RGtk2::gtkVBoxNew(FALSE, 0)
  RGtk2::gtkContainerSetBorderWidth(vbox, 15)
  RGtk2::gtkContainerAdd(frame, vbox)
  
  # 4. Add the message label with Pango markup for styling
  label <- RGtk2::gtkLabelNew()
  markup <- paste0("<span size='large' weight='bold' foreground='#2e8b57'>", message, "</span>")
  RGtk2::gtkLabelSetMarkup(label, markup)
  RGtk2::gtkBoxPackStart(vbox, label, TRUE, TRUE, 0)
  
  # Show the window
  RGtk2::gtkWidgetShowAll(toast_win)
  
  # 5. Set the auto-destruct timer
  RGtk2::gTimeoutAdd(duration_ms, function(...) {
    RGtk2::gtkWidgetDestroy(toast_win)
    return(FALSE) # Returning FALSE tells GTK not to repeat the timer
  })
}
