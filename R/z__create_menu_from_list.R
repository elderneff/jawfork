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
  
  # Inject an RC style that strips the Windows Wimp engine from these specific widgets.
  # This returns total background control to standard GTK, eliminating the forced left gutter.
  rc_style <- "
    style 'jaw_menu_base' {
      engine \"\" {}
      bg[NORMAL] = '#F0F0F0'
    }
    style 'jaw_light_item' {
      engine \"\" {}
      bg[NORMAL] = '#F0F0F0'
      bg[PRELIGHT] = '#CCE8FF'
      fg[NORMAL] = '#000000'
      fg[PRELIGHT] = '#000000'
      text[NORMAL] = '#000000'
      text[PRELIGHT] = '#000000'
    }
    style 'jaw_dark_item' {
      engine \"\" {}
      bg[NORMAL] = '#C8C8C8'
      bg[PRELIGHT] = '#CCE8FF'
      fg[NORMAL] = '#000000'
      fg[PRELIGHT] = '#000000'
      text[NORMAL] = '#000000'
      text[PRELIGHT] = '#000000'
    }
    widget '*jaw_context_menu*' style 'jaw_menu_base'
    widget '*jaw_context_menu*.*' style 'jaw_light_item'
    widget '*jaw_context_menu*.jaw_dark_item' style 'jaw_dark_item'
    widget '*jaw_context_menu*.jaw_dark_item.*' style 'jaw_dark_item'
  "
  RGtk2::gtkRcParseString(rc_style)

  # Name the parent menu so it gets caught by the RC wildcard string above
  if (!is.null(obj[[parent_name]])) {
    RGtk2::gtkWidgetSetName(obj[[parent_name]], "jaw_context_menu")
  }

  if (is.list(my_list)) {
    menu_dirs <- names(my_list)
    for (my_sub_str in menu_dirs) {
      my_sub_str_name <- my_sub_str
      item_d_name <- paste0(parent_name, "|", my_sub_str_name)
      
      menu_item <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      
      # Assign the native style names
      if (my_sub_str_name %in% dark_items) {
        RGtk2::gtkWidgetSetName(menu_item, "jaw_dark_item")
      } else {
        RGtk2::gtkWidgetSetName(menu_item, "jaw_light_item")
      }
      
      obj$items[[item_d_name]] <- menu_item
      RGtk2::gtkMenuShellAppend(obj[[parent_name]], obj$items[[item_d_name]])
      
      # Create submenu and tag it to propagate the Wimp-free zone
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
      
      menu_item <- RGtk2::gtkMenuItem(label = my_sub_str_name)
      
      # Assign the native style names
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
