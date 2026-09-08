#' z__create_menu_from_list
#'
#' @param obj TODO
#' @param parent_name TODO
#' @param my_list TODO
#'
#' @return TODO

z__create_menu_from_list <- function(obj, parent_name, my_list) {
  for (item_name in my_list[[parent_name]]) {
    node_name <- paste0(parent_name, "|", item_name)
    
    #Create standard GTK menu item without custom color formatting.
    obj$item[[node_name]] <- RGtk2::gtkMenuItem(label = item_name)
    
    if (node_name %in% names(my_list)) {
      obj$item[[node_name]][["submenu"]] <- RGtk2::gtkMenu()
      obj <- z__create_menu_from_list(obj, node_name, my_list)
      RGtk2::gtkMenuItemSetSubmenu(obj$item[[node_name]], obj$item[[node_name]][["submenu"]])
    } else {
      obj$end_nodes <- c(obj$end_nodes, node_name)
    }
    
    #Attach the item to the appropriate parent menu level.
    if (parent_name == "base") {
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$item[[node_name]])
    } else {
      RGtk2::gtkMenuShellAppend(obj$item[[parent_name]][["submenu"]], obj$item[[node_name]])
    }
  }
  
  return(obj)
}
