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
      
      #1. Create an empty menu item.
      menu_item <- RGtk2::gtkMenuItemNew()
      
      #2. Create an event box to control background rendering completely.
      eb <- RGtk2::gtkEventBoxNew()
      
      #3. Create the text label and align it left.
      lbl <- RGtk2::gtkLabelNew(my_sub_str_name)
      lbl$xalign <- 0
      
      #Add visual padding to mimic a native menu item.
      lbl["xpad"] <- 16
      lbl["ypad"] <- 4
      
      #4. Pack them together.
      RGtk2::gtkContainerAdd(eb, lbl)
      RGtk2::gtkContainerAdd(menu_item, eb)
      
      #5. Determine the colors for the block.
      if (my_sub_str_name %in% dark_items) {
        c_norm <- RGtk2::gdkColorParse("#D0D0D0")$color
        c_hov <- RGtk2::gdkColorParse("#B0B0B0")$color
      } else {
        c_norm <- RGtk2::gdkColorParse("#F0F0F0")$color
        c_hov <- RGtk2::gdkColorParse("#E5E5E5")$color
      }
      
      #Set the normal background.
      RGtk2::gtkWidgetModifyBg(eb, "normal", c_norm)
      
      #6. Manually capture hover states to swap colors.
      RGtk2::gSignalConnect(menu_item, "select", function(widget, data) {
        RGtk2::gtkWidgetModifyBg(data$eb, "normal", data$c_hov)
        return(FALSE)
      }, data = list(eb = eb, c_hov = c_hov))
      
      RGtk2::gSignalConnect(menu_item, "deselect", function(widget, data) {
        RGtk2::gtkWidgetModifyBg(data$eb, "normal", data$c_norm)
        return(FALSE)
      }, data = list(eb = eb, c_norm = c_norm))
      
      obj$items[[item_d_name]] <- menu_item
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      obj$end_nodes <- c(obj$end_nodes, item_d_name)
    }
  }

  return(obj)
}
