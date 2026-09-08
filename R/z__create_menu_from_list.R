#Creates nested GTK context submenus with cleaned category titles.
z__create_menu_from_list <- function(obj, parent_name, my_list) {
  for (category_name in names(my_list)) {
    #Clean category name for the submenu display title.
    display_label <- gsub("^(Full Data Table|Meta Table|Summary Table)\\s*", "", category_name)
    display_label <- gsub("\\s*Table$", "", display_label)
    if (display_label == "") display_label <- category_name

    #Create parent menu item using cleaned display label.
    category_item <- RGtk2::gtkMenuItem(label = display_label)
    RGtk2::gtkMenuShellAppend(obj[[parent_name]], category_item)

    #Create submenu for category items.
    sub_menu <- RGtk2::gtkMenu()
    RGtk2::gtkMenuItemSetSubmenu(category_item, sub_menu)

    for (item_name in my_list[[category_name]]) {
      #Create clickable menu item.
      action_item <- RGtk2::gtkMenuItem(label = item_name)
      RGtk2::gtkMenuShellAppend(sub_menu, action_item)

      #Store end node reference using original internal category name.
      end_node <- paste0(parent_name, "|", category_name, "|", item_name)
      obj$items[[end_node]] <- action_item
    }
  }
  return(obj)
}
