#' z__create_menu_from_list
#'
#' @param obj TODO
#' @param parent_name TODO
#' @param my_list TODO
#'
#' @return TODO

z__create_menu_from_list <- function(obj, parent_name, my_list) {
  # Define items that should have the darker grey background.
  dark_items <- c(
    "if then", "if then do",
    "Column full", "Column filtered", "Column Wide",
    "Add Column to Main Filter Exclude", "Add to Main Filter Exclude",
    "Add Bucket to Main Filter Exclude"
  )

  # Inject an RC style to drop the Windows Wimp engine specifically for our dark items.
  # This prevents the overarching Windows theme from aggressively overwriting our custom Cairo paint.
  RGtk2::gtkRcParseString("
    style 'jaw_dark_menu_style' {
      engine \"\" {}
      bg[PRELIGHT] = '#91C9F7'
      fg[PRELIGHT] = '#000000'
      text[PRELIGHT] = '#000000'
    }
    widget '*jaw_dark_item' style 'jaw_dark_menu_style'
  ")

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
      
      # Retain GTK's native menu item generation so text background stays transparent
      menu_item <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      
      if (my_sub_str_name %in% dark_items) {
         # Target this widget with the Wimp-free RC style
         RGtk2::gtkWidgetSetName(menu_item, "jaw_dark_item")
         
         # Paint the background natively, spanning the full allocation width
         RGtk2::gSignalConnect(menu_item, "expose-event", function(widget, event, data) {
            # Only paint the dark background if we aren't hovering (normal state)
            if (widget[["state"]] == RGtk2::GtkStateType["normal"]) {
               cr <- RGtk2::gdkCairoCreate(widget[["window"]])
               alloc <- widget[["allocation"]]
               
               # #C8C8C8 corresponds to RGB: 200/255 = 0.7843
               RGtk2::cairoSetSourceRgb(cr, 0.7843, 0.7843, 0.7843) 
               RGtk2::cairoRectangle(cr, alloc[["x"]], alloc[["y"]], alloc[["width"]], alloc[["height"]])
               RGtk2::cairoFill(cr)
            }
            
            # Return FALSE so GTK still knows to draw the text label on top of our rectangle
            # and handles the blue prelight hover cleanly natively.
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
