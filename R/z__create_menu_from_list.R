#' z__create_menu_from_list
#'
#' @param obj TODO
#' @param parent_name TODO
#' @param my_list TODO
#'
#' @return TODO

z__create_menu_from_list <- function(obj, parent_name, my_list) {
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
      
      menu_item <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      
      if (grepl("Copy", parent_name)) {
        light_grey <- RGtk2::gdkColorParse("#F0F0F0")$color
        dark_grey <- RGtk2::gdkColorParse("#D0D0D0")$color
        
        if (my_sub_str_name %in% c("Cell value", "Column Name", "Column=Cell")) {
          RGtk2::gtkWidgetModifyBg(menu_item, RGtk2::GtkStateType["normal"], light_grey)
        } else if (my_sub_str_name %in% c("if then", "if then do")) {
          RGtk2::gtkWidgetModifyBg(menu_item, RGtk2::GtkStateType["normal"], dark_grey)
        }
      }
      
      obj$items[[item_d_name]] <- menu_item
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      obj$end_nodes <- c(obj$end_nodes, item_d_name)
    }
  }

  return(obj)
}
