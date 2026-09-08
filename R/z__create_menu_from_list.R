#Creates a nested GTK menu structure from a list.
z__create_menu_from_list <- function(obj, parent_name, my_list) {
  for (category_name in names(my_list)) {
    #Create the parent category item.
    category_item <- RGtk2::gtkMenuItem(label = category_name)
    RGtk2::gtkMenuShellAppend(obj[[parent_name]], category_item)
    
    #Create the submenu to hold the actual actions.
    sub_menu <- RGtk2::gtkMenu()
    RGtk2::gtkMenuItemSetSubmenu(category_item, sub_menu)
    
    for (item_name in my_list[[category_name]]) {
      #Create the clickable action item.
      action_item <- RGtk2::gtkMenuItem(label = item_name)
      RGtk2::gtkMenuShellAppend(sub_menu, action_item)
      
      #Store a reference so event handlers and settings can access it later.
      end_node <- paste0(parent_name, "|", category_name, "|", item_name)
      obj$items[[end_node]] <- action_item
    }
  }
  return(obj)
}
