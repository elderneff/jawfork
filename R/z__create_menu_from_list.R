z__create_menu_from_list <- function(obj, parent_name, my_list) {
  #Inject RC string to permanently collapse the invisible left gutter.
  RGtk2::gtkRcParseString("
    style 'jaw_menu_flush' {
      engine \"\" {}
      GtkMenuItem::toggle-spacing = 0
      GtkMenuItem::indicator-size = 0
    }
    widget_class '*GtkMenuItem*' style 'jaw_menu_flush'
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
      
      #You will also need to remove any downstream if-statements in your loop.
      #Remove the logic that checks if the item is in dark_items or conditional_dark_items.
      
      obj <- z__create_menu_from_list(obj, item_d_name, my_list[[my_sub_str]])
    }
  }
  return(obj)
}
