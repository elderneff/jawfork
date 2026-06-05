#' e__show_toast
#'
#' @param session_name TODO
#' @param message The text to display
#' @param duration_ms Time in milliseconds before the toast disappears
#' @param outer_env TODO
#'
#' @return TODO

e__show_toast <- function(session_name, message = "Code copied to clipboard!", duration_ms = 2500, outer_env = totem) {
  toast_win <- RGtk2::gtkWindowNew("toplevel")
  RGtk2::gtkWindowSetDecorated(toast_win, FALSE)
  RGtk2::gtkWindowSetResizable(toast_win, FALSE)
  RGtk2::gtkWindowSetKeepAbove(toast_win, TRUE)
  RGtk2::gtkWindowSetPosition(toast_win, 0L)
  
  # 1. Custom Pink Background (#ffaec8)
  event_box <- RGtk2::gtkEventBoxNew()
  bg_color <- RGtk2::gdkColorParse("#ffcadb")$color
  RGtk2::gtkWidgetModifyBg(event_box, RGtk2::GtkStateType["normal"], bg_color)
  RGtk2::gtkContainerAdd(toast_win, event_box)
  
  frame <- RGtk2::gtkFrameNew()
  RGtk2::gtkFrameSetShadowType(frame, "out")
  RGtk2::gtkContainerAdd(event_box, frame)
  
  vbox <- RGtk2::gtkVBoxNew(FALSE, 0)
  RGtk2::gtkContainerSetBorderWidth(vbox, 15)
  RGtk2::gtkContainerAdd(frame, vbox)
  
  # 2. Custom Black Text
  label <- RGtk2::gtkLabelNew()
  markup <- paste0("<span size='large' weight='bold' foreground='#000000'>", message, "</span>")
  RGtk2::gtkLabelSetMarkup(label, markup)
  RGtk2::gtkBoxPackStart(vbox, label, TRUE, TRUE, 0)
  
  parent_window <- outer_env[[session_name]]$windows$main_window
  
  if (!is.null(parent_window)) {
    true_parent <- RGtk2::gtkWidgetGetToplevel(parent_window)
    RGtk2::gtkWindowSetTransientFor(toast_win, true_parent)
    
    # Request the dimensions of our newly built toast
    req <- RGtk2::gtkWidgetSizeRequest(toast_win)$requisition
    toast_w <- req$width
    toast_h <- req$height
    
    # Pull the exact variables using the DOT syntax
    p_pos <- RGtk2::gtkWindowGetPosition(true_parent)
    p_size <- RGtk2::gtkWindowGetSize(true_parent)
    
    parent_x <- p_pos$root.x
    parent_y <- p_pos$root.y
    parent_w <- p_size$width
    parent_h <- p_size$height
    
    # 3. Calculate bottom-left placement
    padding <- 50
    target_x <- parent_x + padding
    target_y <- parent_y + parent_h - toast_h - padding
    
    # Move it!
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
