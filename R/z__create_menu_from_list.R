#' z__create_menu_from_list
#'
#' @param obj TODO
#' @param parent_name TODO
#' @param my_list TODO
#'
#' @return TODO

z__create_menu_from_list <- function(obj, parent_name, my_list) {
  #Define items that should have the darker grey background.
  dark_items <- c(
    "if then", "if then do",
    "Column full", "Column filtered", "Column Wide",
    "Add Column to Main Filter Exclude", "Add to Main Filter Exclude",
    "Add Bucket to Main Filter Exclude"
  )
  
  if (is.list(my_list)) {
    menu_dirs <- names(my_list)
    for (my_sub_str in menu_dirs) {
      my_sub_str_name <- my_sub_str
      item_d_name <- paste0(parent_name, "|", my_sub_str_name)
      obj$items[[item_d_name]] <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      obj[[item_d_name]] <- RGtk2::gtkMenu()
      RGtk2::gtkMenuItemSetSubmenu(obj$items[[item_d_name]], obj[[item_d_name]])
      obj <- z__create_menu_from_list(
        obj, item_d_name,
        my_list[[my_sub_str]]
      )
    }
  } else if (is.vector(my_list)) {
    for (my_sub_str in my_list) {
      my_sub_str_name <- my_sub_str
      item_d_name <- paste0(parent_name, "|", my_sub_str_name)
      
      # Use native GtkMenuItem generation to preserve standard geometry and padding
      menu_item <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      
      # If the item is in the dark block, use Cairo to color the full width natively
      if (my_sub_str_name %in% dark_items) {
        RGtk2::gSignalConnect(menu_item, "expose-event", function(widget, event, data) {
          # Allow the native Windows engine to take over if the user is hovering.
          # This preserves the readable native blue highlight and white text.
          if (widget[["state"]] == RGtk2::GtkStateType["prelight"]) {
            return(FALSE)
          }
          
          # Otherwise, manually paint the dark grey box across the entire allocation.
          # This securely paints over the invisible left gutter area.
          alloc <- widget[["allocation"]]
          cr <- RGtk2::gdkCairoCreate(widget[["window"]])
          
          # #D0D0D0 corresponds to RGB: 208/255 = 0.8157
          RGtk2::cairoSetSourceRgb(cr, 0.8157, 0.8157, 0.8157)
          RGtk2::cairoRectangle(cr, alloc[["x"]], alloc[["y"]], alloc[["width"]], alloc[["height"]])
          RGtk2::cairoFill(cr)
          
          # Return FALSE so GTK still knows to draw the text label on top of our rectangle
          return(FALSE)
        })
      }
      
      obj$items[[item_d_name]] <- menu_item
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      obj$end_nodes <- c(obj$end_nodes, item_d_name)
    }
  }

  return(obj)
}
