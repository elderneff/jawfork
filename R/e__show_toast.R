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
  
  # 1. Use an EventBox to force the background color, overriding Dark/Light mode
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
  
  # 2. Force text color to black (#000000) using markup
  label <- RGtk2::gtkLabelNew()
  markup <- paste0("<span size='large' weight='bold' foreground='#000000'>", message, "</span>")
  RGtk2::gtkLabelSetMarkup(label, markup)
  RGtk2::gtkBoxPackStart(vbox, label, TRUE, TRUE, 0)
  
  parent_window <- outer_env[[session_name]]$windows$main_window
  
  if (!is.null(parent_window)) {
    true_parent <- RGtk2::gtkWidgetGetToplevel(parent_window)
    RGtk2::gtkWindowSetTransientFor(toast_win, true_parent)
    
    # Pre-calculate toast dimensions (using the tail trick to strip C-level flags)
    req <- RGtk2::gtkWidgetSizeRequest(toast_win)
    req_vec <- tail(as.numeric(unlist(req)), 2)
    toast_w <- req_vec[1]
    toast_h <- req_vec[2]
    
    # 3. Bulletproof coordinate extraction
    p_pos <- RGtk2::gtkWindowGetPosition(true_parent)
    p_size <- RGtk2::gtkWindowGetSize(true_parent)
    
    p_vec <- tail(as.numeric(unlist(p_pos)), 2)
    s_vec <- tail(as.numeric(unlist(p_size)), 2)
    
    # Calculate bottom-right placement
    padding <- 50
    target_x <- p_vec[1] + s_vec[1] - toast_w - padding
    target_y <- p_vec[2] + s_vec[2] - toast_h - padding
    
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
