#' z__tree_view_column_get_col_idx
#'
#' @param column TODO
#'
#' @return TODO

z__tree_view_column_get_col_idx <- function(column) {
  tryCatch(
    column["title"],
    error = function(e) integer(0)
  )
}

#' e__tree_view_column_btn_press
#'
#' @param widget TODO
#' @param event TODO
#' @param data TODO
#'
#' @return TODO

e__tree_view_column_btn_press <- function(widget, event, data) {
  # model <- data$model

  info <- RGtk2::gtkTreeViewGetPathAtPos(widget, event[["x"]], event[["y"]])
  if (is.null(info$path)) {
    return(TRUE)
  }
  
  RGtk2::gtkTreeViewSetCursor(widget, info$path, NULL, FALSE)
  
  # Explicitly shift GTK focus to the table so the text area stops listening
  RGtk2::gtkWidgetGrabFocus(widget)
  
  row.idx <- RGtk2::gtkTreePathGetIndices(info$path) + 1
  col.idx <- z__tree_view_column_get_col_idx(info$column)
  
  data$table_cell_events(event, row.idx, col.idx)

  #Prevent GTK's default selection behavior from clearing the highlight on right-clicks
  if (event[["button"]] == 3) {
    return(TRUE)
  }

  return(FALSE)
}
