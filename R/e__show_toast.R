#' e__show_toast
#'
#' @param session_name TODO
#' @param message The text to display
#' @param duration_ms Time in milliseconds before the toast disappears
#' @param outer_env TODO
#'
#' @return TODO

e__show_toast <- function(session_name, message = "Code copied to clipboard!", duration_ms = 2000, outer_env = totem) {
  toast_win <- RGtk2::gtkWindowNew("toplevel")
  RGtk2::gtkWindowSetDecorated(toast_win, FALSE)
  RGtk2::gtkWindowSetResizable(toast_win, FALSE)
  RGtk2::gtkWindowSetKeepAbove(toast_win, TRUE)
  
  # Set position policy to NONE (0L) so GTK obeys our math
  RGtk2::gtkWindowSetPosition(toast_win, 0L)
  
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
  
  parent_window <- outer_env[[session_name]]$windows$main_window
  
  if (!is.null(parent_window)) {
    true_parent <- RGtk2::gtkWidgetGetToplevel(parent_window)
    RGtk2::gtkWindowSetTransientFor(toast_win, true_parent)
    
    # Pre-calculate toast dimensions
    req <- RGtk2::gtkWidgetSizeRequest(toast_win)$requisition
    toast_w <- req$width
    toast_h <- req$height
    
    # Get true parent dimensions safely
    p_pos <- RGtk2::gtkWindowGetPosition(true_parent)
    p_size <- RGtk2::gtkWindowGetSize(true_parent)
    
    # Calculate exact bottom-right placement
    padding <- 50
    target_x <- p_pos$root_x + p_size$width - toast_w - padding
    target_y <- p_pos$root_y + p_size$height - toast_h - padding
    
    # Move it
    RGtk2::gtkWindowMove(toast_win, as.integer(target_x), as.integer(target_y))
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
