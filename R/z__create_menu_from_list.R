#' z__create_menu_from_list
#'
#' @param obj TODO
#' @param parent_name TODO
#' @param my_list TODO
#'
#' @return TODO

z__create_menu_from_list <- function(obj, parent_name, my_list) {
  # Define items that should have the darker grey background
  dark_items <- c(
    "if then", "if then do",
    "Column full", "Column filtered", "Column Wide",
    "Add Column to Main Filter Exclude", "Add to Main Filter Exclude",
    "Add Bucket to Main Filter Exclude"
  )
  
  # Inject pure RC styles to completely override the Wimp engine.
  # This collapses the left gutter and paints the prelight (hover) natively.
  RGtk2::gtkRcParseString("
    style 'jaw_menu_flat' {
      engine \"\" {}
      GtkMenuItem::toggle-spacing = 0
      GtkMenuItem::indicator-size = 0
      bg[NORMAL] = '#F0F0F0'
    }
    style 'jaw_light_item' {
      engine \"\" {}
      bg[NORMAL] = '#F0F0F0'
      bg[PRELIGHT] = '#91C9F7'
      fg[NORMAL] = '#000000'
      fg[PRELIGHT] = '#000000'
      text[NORMAL] = '#000000'
      text[PRELIGHT] = '#000000'
    }
    style 'jaw_dark_item' {
      engine \"\" {}
      bg[NORMAL] = '#C8C8C8'
      bg[PRELIGHT] = '#91C9F7'
      fg[NORMAL] = '#000000'
      fg[PRELIGHT] = '#000000'
      text[NORMAL] = '#000000'
      text[PRELIGHT] = '#000000'
    }
    widget '*jaw_context_menu*' style 'jaw_menu_flat'
    widget '*jaw_light_item*' style 'jaw_light_item'
    widget '*jaw_dark_item*' style 'jaw_dark_item'
  ")

  # Name the parent menu so it drops the Wimp gutter
  if (!is.null(obj[[parent_name]])) {
    RGtk2::gtkWidgetSetName(obj[[parent_name]], "jaw_context_menu")
  }

  if (is.list(my_list)) {
    menu_dirs <- names(my_list)
    for (my_sub_str in menu_dirs) {
      my_sub_str_name <- my_sub_str
      item_d_name <- paste0(parent_name, "|", my_sub_str_name)
      
      # Build item using native label constructor to maintain proper transparent state propagation
      menu_item <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      
      # Manually pad the native label to fix indentation without breaking hover highlighting
      child_label <- RGtk2::gtkBinGetChild(menu_item)
      if (!is.null(child_label)) {
        child_label["xpad"] <- 0
      }
      
      # Assign specific native style name
      if (my_sub_str_name %in% dark_items) {
        RGtk2::gtkWidgetSetName(menu_item, "jaw_dark_item")
      } else {
        RGtk2::gtkWidgetSetName(menu_item, "jaw_light_item")
      }
      
      obj$items[[item_d_name]] <- menu_item
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      
      # Name the submenu to ensure Wimp is dropped recursively
      obj[[item_d_name]] <- RGtk2::gtkMenu()
      RGtk2::gtkWidgetSetName(obj[[item_d_name]], "jaw_context_menu")
      
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
      
      # Build item using native label constructor to maintain proper transparent state propagation
      menu_item <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      
      # Manually pad the native label to fix indentation without breaking hover highlighting
      child_label <- RGtk2::gtkBinGetChild(menu_item)
      if (!is.null(child_label)) {
        child_label["xpad"] <- 0
      }
      
      # Assign specific native style name
      if (my_sub_str_name %in% dark_items) {
        RGtk2::gtkWidgetSetName(menu_item, "jaw_dark_item")
      } else {
        RGtk2::gtkWidgetSetName(menu_item, "jaw_light_item")
      }
      
      obj$items[[item_d_name]] <- menu_item
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      obj$end_nodes <- c(obj$end_nodes, item_d_name)
    }
  }

  return(obj)
}
