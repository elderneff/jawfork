


#' u__button
#'
#' @param box TODO
#' @param start TODO
#' @param padding TODO
#' @param stock_id TODO
#' @param but_txt TODO
#' @param tool_tip TODO
#' @param call_back_fct TODO
#' @param data TODO
#'
#' @return TODO

u__button <- function(box, start = T, padding = 5, stock_id = NULL, but_txt = NULL, tool_tip = NULL, call_back_fct = NULL, data = NULL) {
  btn <- RGtk2::gtkButton()
  RGtk2::gtkButtonSetFocusOnClick(btn, F)
  if (is.null(stock_id) == F) {
    RGtk2::gtkContainerAdd(btn, RGtk2::gtkImageNewFromStock(stock_id, size = 2L))
  } else if (is.null(but_txt) == F) {
    RGtk2::gtkButtonSetLabel(btn, but_txt)
  }

  RGtk2::gtkButtonSetRelief(btn, "GTK_RELIEF_NONE") 
  # Only attach the tooltip if the text isn't null AND the global setting allows it
  if (is.null(tool_tip) == F && isTRUE(totem$settings_list$show_tooltips)) {
    RGtk2::gtkWidgetSetTooltipText(btn, tool_tip)
  }
  if (start) {
    RGtk2::gtkBoxPackStart(box, btn, F, F, padding = padding)
  } else {
    RGtk2::gtkBoxPackEnd(box, btn, F, F, padding = padding)
  }



  if (is.null(call_back_fct) == F) {
    RGtk2::gSignalConnect(btn, "button-press-event", call_back_fct, data = data)
  }
  return(btn)
}
