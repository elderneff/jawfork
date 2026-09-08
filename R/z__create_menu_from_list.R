z__create_menu_from_list <- function(obj, parent_name, my_list) {
  # Define items that should have the darker grey background.
  dark_items <- c(
    "Add to filter", "Add to grepl to filter", "Clear filter", "Bob",
    "if then", "if then do", "Column full", "Column filtered", "Column Wide", "Row",
    "Add Column to Main Filter", "Add Column to Main Filter Exclude", "Get Summary", "Graph Summary", "Scatterplot Summary",
    "Add Column to select", "Move column before", "Move column after", 
    "dataset layout", "keep statement", "label statement", "length statement",
    "Open Flat View", "Open Inverted View"
  )
  
  #Define conditionally dark items using a named list.
  conditional_dark_items <- list(
    "Format by Column" = "Full Data Table Organize",
    "Add'l format by Column" = "Full Data Table Organize",
    "Pin for Comparison" = "Meta Table Organize|Summary Table Organize",
    "Compare with Pinned" = "Meta Table Organize|Summary Table Organize"
  )
  
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
      
      # Strip the internal prefixes so the user just sees "Filter", "Summarize", etc.
      my_sub_str_name <- sub("^(Full Data Table |Meta Table |Summary Table )", "", my_sub_str)
      
      item_d_name <- paste0(parent_name, "|", my_sub_str)
      obj$items[[item_d_name]] <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      obj[[item_d_name]] <- RGtk2::gtkMenu()
      RGtk2::gtkMenuItemSetSubmenu(obj$items[[item_d_name]], obj[[item_d_name]])
      
      z__create_menu_from_list(obj, item_d_name, my_list[[my_sub_str]])
    }
  } else {
    for (my_sub_str in my_list) {
      item_d_name <- paste0(parent_name, "|", my_sub_str)
      obj$items[[item_d_name]] <- RGtk2::gtkMenuItem(label = my_sub_str)
      
      # Apply custom visual styling
      evb <- RGtk2::gtkEventBox()
      lbl <- RGtk2::gtkLabel(my_sub_str)
      RGtk2::gtkMiscSetAlignment(lbl, 0, 0.5)
      RGtk2::gtkContainerAdd(evb, lbl)
      RGtk2::gtkContainerAdd(obj$items[[item_d_name]], evb)
      
      is_dark <- FALSE
      if (my_sub_str %in% dark_items) {
        is_dark <- TRUE
      } else if (my_sub_str %in% names(conditional_dark_items)) {
        if (grepl(conditional_dark_items[[my_sub_str]], parent_name)) is_dark <- TRUE
      }
      
      if (is_dark) {
        RGtk2::gtkWidgetModifyBg(evb, RGtk2::GtkStateType["normal"], RGtk2::gdkColorParse("#E8E8E8")$color)
      } else {
        RGtk2::gtkWidgetModifyBg(evb, RGtk2::GtkStateType["normal"], RGtk2::gdkColorParse("#F0F0F0")$color)
      }
      
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
    }
  }
  return(obj)
}
