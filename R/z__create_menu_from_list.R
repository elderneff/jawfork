z__create_menu_from_list <- function(obj, parent_name, my_list) {
  #Define items that should have the darker grey background.
  dark_items <- c(
    "if then", "if then do",
    "Column full", "Column filtered", "Column Wide",
    "Add Column to Main Filter Exclude", "Add to Main Filter Exclude",
    "Add Bucket to Main Filter Exclude"
  )
  
  #Inject RC string to permanently collapse the invisible left gutter.
  #This allows our EventBox to stretch entirely edge-to-edge.
  RGtk2::gtkRcParseString("
    style 'jaw_menu_flush' {
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
      obj <- z__create_menu_from_list(
        obj, item_d_name,
        my_list[[my_sub_str]]
      )
    }
  } else if (is.vector(my_list)) {
    for (my_sub_str in my_list) {
      my_sub_str_name <- my_sub_str
      item_d_name <- paste0(parent_name, "|", my_sub_str_name)
      
      #Create an empty menu item.
      menu_item <- RGtk2::gtkMenuItemNew()
      
      #Create an event box to control background rendering completely.
      eb <- RGtk2::gtkEventBoxNew()
      RGtk2::gtkWidgetSetHasWindow(eb, TRUE)
      
      #Create the text label and align it left.
      lbl <- RGtk2::gtkLabelNew(my_sub_str_name)
      lbl$xalign <- 0
      
      #Add visual padding inside the EventBox so it mimics native menus.
      lbl["xpad"] <- 0
      lbl["ypad"] <- 0
      
      #Pack them together.
      RGtk2::gtkContainerAdd(eb, lbl)
      RGtk2::gtkContainerAdd(menu_item, eb)
      
      #Determine the colors for the block.
      if (my_sub_str_name %in% dark_items) {
        c_norm <- RGtk2::gdkColorParse("#C8C8C8")$color
        c_hov <- RGtk2::gdkColorParse("#91C9F7")$color
      } else {
        c_norm <- RGtk2::gdkColorParse("#F0F0F0")$color
        c_hov <- RGtk2::gdkColorParse("#91C9F7")$color
      }
      
      #Set the normal and prelight backgrounds initially.
      RGtk2::gtkWidgetModifyBg(eb, "normal", c_norm)
      RGtk2::gtkWidgetModifyBg(eb, "prelight", c_norm)
      
      #Manually capture hover states to swap colors with solid paint.
      #Update both normal and prelight states to ensure the EventBox color fully applies.
      RGtk2::gSignalConnect(menu_item, "select", function(widget, data) {
        RGtk2::gtkWidgetModifyBg(data$eb, "normal", data$c_hov)
        RGtk2::gtkWidgetModifyBg(data$eb, "prelight", data$c_hov)
        return(FALSE)
      }, data = list(eb = eb, c_hov = c_hov))
      
      RGtk2::gSignalConnect(menu_item, "deselect", function(widget, data) {
        RGtk2::gtkWidgetModifyBg(data$eb, "normal", data$c_norm)
        RGtk2::gtkWidgetModifyBg(data$eb, "prelight", data$c_norm)
        return(FALSE)
      }, data = list(eb = eb, c_norm = c_norm))
      
      obj$items[[item_d_name]] <- menu_item
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      obj$end_nodes <- c(obj$end_nodes, item_d_name)
    }
  }

  return(obj)
}
