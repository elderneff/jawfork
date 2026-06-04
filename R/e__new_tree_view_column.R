#' e__new_tree_view_column
#'
#' @param df TODO
#' @param j TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__new_tree_view_column <- function(df, j,outer_env=totem,obj_env=inner_env) {
  renderer <- RGtk2::gtkCellRendererText()



  column <- RGtk2::gtkTreeViewColumn()
  RGtk2::gtkTreeViewColumnSetTitle(column, colnames(df)[j])
  RGtk2::gtkTreeViewColumnPackStart(column, renderer)

  RGtk2::gtkTreeViewColumnAddAttribute(column, renderer, "text", j - 1)
  if ((is_full_data_table & j == 1) | j == 1) {
    RGtk2::gtkTreeViewColumnAddAttribute(column, renderer, "background", as.integer(ncol(df) - 3))
  } else {
    RGtk2::gtkTreeViewColumnAddAttribute(column, renderer, "background", as.integer(ncol(df) - 2))
  }
  #Set text color based on f__3
  RGtk2::gtkTreeViewColumnAddAttribute(column, renderer, "foreground", as.integer(ncol(df) - 1))


  RGtk2::gtkTreeViewColumnSetSizing(column, RGtk2::GtkTreeViewColumnSizing["autosize"])
  RGtk2::gtkTreeViewColumnSetResizable(column, TRUE)
  RGtk2::gtkTreeViewColumnSetVisible(column, TRUE)
  RGtk2::gtkTreeViewColumnSetReorderable(column, F)
  RGtk2::gtkTreeViewColumnSetClickable(column, T)


  if (is_full_data_table & j > 1) {
    data3 <- outer_env[[session_name]]$data3
    my_row <- data3[j - 1, ]
    my_tool_tip <- paste0(
      my_row[, "label"], "\nLength: ", my_row[, "length"],
      "\nClass: ", my_row[, "class"], "\nDistinct: ", my_row[, "distinct"],
      "\nUnique: ", my_row[, "unique"], "\nMissing: ", my_row[, "missing"],
      "\nBlank: ", my_row[, "blank"]
    )

    evt <- obj_env$add_column_label(column, colnames(df)[j], j, my_row[, "class"], my_tool_tip, paste0("U: ", my_row[, "unique"]))
  } else {
    evt <- obj_env$add_column_label(column, colnames(df)[j], j)
  }


  RGtk2::gSignalConnect(column, "clicked", function(treeviewcolumn, data) {
    obj_env$order_by_obj$add(data)
    return(T)
  }, data = colnames(df)[j])




  return(list(column = column, renderer = renderer, col.idx = j, evt = evt))
}
