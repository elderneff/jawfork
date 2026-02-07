#' e__all_event_functions
#'
#' @param outer_env TODO
#'
#' @return TODO

e__all_event_functions <- function(outer_env = totem) {
  i__all_event_functions <- list()

  #--------------------------------------------

  # General

  #-------------------------------------------

  i__all_event_functions[["General"]][["View"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$view()
  }
  i__all_event_functions[["General"]][["Refresh"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$update_filter()
  }
  i__all_event_functions[["General"]][["Add to filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column <- current_row$column
    value <- current_row$value
    obj_env$filter_obj$add(column, value)
  }
  i__all_event_functions[["General"]][["Add to grepl to filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column <- current_row$column
    value <- current_row$value
    obj_env$filter_obj$add_grepl(column, value)
  }
  i__all_event_functions[["General"]][["Clear filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$filter_obj$clean()
  }
  i__all_event_functions[["General"]][["Add to arrange"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column <- current_row$column
    obj_env$order_by_obj$add(column)
  }
  i__all_event_functions[["General"]][["Clear arrange"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$order_by_obj$clean()
  }
  i__all_event_functions[["General"]][["Trigger code"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$show_load_window()
    outer_env$u__load_dataset_filter(session_name)
    outer_env$hide_load_window()
  }
  i__all_event_functions[["General"]][["Open Context Menu"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    if (is.null(view_objects$event) == F) {
      click_btn <- view_objects$event$button
      RGtk2::gtkMenuPopup(obj_env$menubar[["base"]],
        button = click_btn,
        activate.time = RGtk2::gdkEventGetTime(view_objects$event)
      )
    }
  }
  i__all_event_functions[["General"]][["Bob"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    w <- RGtk2::gtkWindow(show = FALSE)
    w["title"] <- "Bob"
    if (rand == party_rand) {
      RGtk2::gtkContainerAdd(w, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "party_loading.gif")))
    } else if (rand == left_rand) {
      RGtk2::gtkContainerAdd(w, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "left_loading.gif")))
    } else if (rand == evil_rand) {
      RGtk2::gtkContainerAdd(w, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "evil_loading.gif")))
    } else if (rand == blurry_rand) {
      RGtk2::gtkContainerAdd(w, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "blurry_loading.gif")))
    } else {
      RGtk2::gtkContainerAdd(w, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "loading.gif")))
    }
    RGtk2::gtkWidgetSetSizeRequest(w, 300, 350)
    
    RGtk2::gtkWidgetShow(w)

    save_settings(totem)
  }


  #--------------------------------------------

  # Copy

  #-------------------------------------------



  i__all_event_functions[["Copy"]][["Cell value"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    value <- current_row$value
    utils::writeClipboard(str = charToRaw(paste0(value, " ")), format = 1)
  }


  i__all_event_functions[["Copy"]][["Column Name"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column <- current_row$column
    utils::writeClipboard(str = charToRaw(paste0(column, " ")), format = 1)
  }

  i__all_event_functions[["Copy"]][["Column=Cell"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column_classes <- obj_env$df_obj$get_column_classes()
    if (column_classes[obj_env$table_objects_list$current_row$column] == "numeric") {
      utils::writeClipboard(str = charToRaw(paste0(obj_env$table_objects_list$current_row$column, "=", obj_env$table_objects_list$current_row$value, " ")), format = 1)
    } else {
      utils::writeClipboard(str = charToRaw(paste0(obj_env$table_objects_list$current_row$column, "=\"", obj_env$table_objects_list$current_row$value, "\" ")), format = 1)
    }
  }

  i__all_event_functions[["Copy"]][["if then"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$copy_if_then(session_name, obj_env$table_objects_list$current_row, obj_env$df_obj)
  }

  i__all_event_functions[["Copy"]][["if then do"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$copy_if_then_do(session_name, obj_env$table_objects_list$current_row, obj_env$df_obj)
  }

  i__all_event_functions[["Copy"]][["Table full"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_full()
  }

  i__all_event_functions[["Copy"]][["Table full to file"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_full_to_file()
  }

  i__all_event_functions[["Copy"]][["Table filtered"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_filter()
  }

  i__all_event_functions[["Copy"]][["Column full"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_full(obj_env$table_objects_list$current_row$column)
  }

  i__all_event_functions[["Copy"]][["Column filtered"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_filter(obj_env$table_objects_list$current_row$column)
  }

  i__all_event_functions[["Copy"]][["Vector Column full"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_full(obj_env$table_objects_list$current_row$column, vector = T)
  }

  i__all_event_functions[["Copy"]][["Vector Column filtered"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_filter(obj_env$table_objects_list$current_row$column, vector = T)
  }

  #--------------------------------------------

  # meta table

  #-------------------------------------------
  i__all_event_functions[["Meta Table"]][["Trigger Value Summary"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    row_i <- current_row$row_i
    view_objects$event_mapping[["Meta Table|Trigger Value Summary"]](session_name, current_data[row_i, "variable",
      drop = T
    ])
  }
  i__all_event_functions[["Meta Table"]][["Trigger Value Summary with Group By"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    row_i <- current_row$row_i
    view_objects$event_mapping[["Meta Table|Trigger Value Summary with Group By"]](session_name, current_data[row_i, "variable",
      drop = T
    ])
  }

  i__all_event_functions[["Meta Table"]][["dataset_layout"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_dataset_layout()
  }
  i__all_event_functions[["Meta Table"]][["keep statement"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_keep()
  }
  i__all_event_functions[["Meta Table"]][["label statement"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_label()
  }
  i__all_event_functions[["Meta Table"]][["length statement"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_length()
  }

  i__all_event_functions[["Meta Table"]][["Move column before"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    #Make a vector of the format needed to pass to move_column
    fake_current_row <- c()
    fake_current_row$column <- current_data[current_row$row_i, "variable", drop = T]
    outer_env$move_column(0, session_name, fake_current_row)
  }
  i__all_event_functions[["Meta Table"]][["Move column after"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    #Make a vector of the format needed to pass to move_column
    fake_current_row <- c()
    fake_current_row$column <- current_data[current_row$row_i, "variable", drop = T]
    outer_env$move_column(1, session_name, fake_current_row)
  }

  #--------------------------------------------

  # full data

  #-------------------------------------------
  i__all_event_functions[["Full Data Table"]][["Trigger Value Summary"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column <- current_row$column
    view_objects$event_mapping[["Full Data Table|Trigger Value Summary"]](session_name, column)
  }


  i__all_event_functions[["Full Data Table"]][["Trigger Value Summary with Group By"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column <- current_row$column
    view_objects$event_mapping[["Full Data Table|Trigger Value Summary with Group By"]](session_name, column)
  }




  i__all_event_functions[["Full Data Table"]][["Add Column to select"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
    if (st != "") {
      st <- paste0(st, ", ", obj_env$table_objects_list$current_row$column)
    } else {
      st <- obj_env$table_objects_list$current_row$column
    }
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, st)
  }
  i__all_event_functions[["Full Data Table"]][["Add to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data(session_name, obj_env$table_objects_list$current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Add to Main Filter Exclude"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data(session_name, obj_env$table_objects_list$current_row, exclude = T)
  }
  i__all_event_functions[["Full Data Table"]][["Add Bucket to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_bucket(session_name, obj_env$table_objects_list$current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Add Bucket to Main Filter Exclude"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_bucket(session_name, obj_env$table_objects_list$current_row, exclude = T)
  }
  i__all_event_functions[["Full Data Table"]][["Add Column to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_column(session_name, obj_env$table_objects_list$current_row, obj_env$df_obj)
  }
  i__all_event_functions[["Full Data Table"]][["Add Column to Main Filter Exclude"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_column(session_name, obj_env$table_objects_list$current_row, obj_env$df_obj, exclude = T)
  }
  i__all_event_functions[["Full Data Table"]][["Add Count to df"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_count_to_df_summary(session_name, obj_env$table_objects_list$current_row$column)
  }
  
  i__all_event_functions[["Full Data Table"]][["Get Summary"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__get_summary(session_name, current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Graph Summary"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__graph_summary(session_name, current_row)
  }

  i__all_event_functions[["Full Data Table"]][["Move column before"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$move_column(0, session_name, current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Move column after"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$move_column(1, session_name, current_row)
  }

  #--------------------------------------------

  # value table

  #-------------------------------------------
  i__all_event_functions[["Summary Table"]][["Open Flat View"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__flat_view(session_name, current_row)
  }


  i__all_event_functions[["Summary Table"]][["Open Inverted View"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__inverted_view(session_name, current_row)
  }

  i__all_event_functions[["Summary Table"]][["Add to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter(session_name, current_row)
  }

  i__all_event_functions[["Summary Table"]][["Add to Main Filter Exclude"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter(session_name, obj_env$table_objects_list$current_row, exclude = T)
  }

  i__all_event_functions[["Summary Table"]][["Add Bucket to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_bucket(session_name, obj_env$table_objects_list$current_row)
  }

  i__all_event_functions[["Summary Table"]][["Add Bucket to Main Filter Exclude"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_bucket(session_name, obj_env$table_objects_list$current_row, exclude = T)
  }

  i__all_event_functions[["Summary Table"]][["Add Column to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_column(session_name, obj_env$table_objects_list$current_row, obj_env$df_obj)
  }

  i__all_event_functions[["Summary Table"]][["Add Column to Main Filter Exclude"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_column(session_name, obj_env$table_objects_list$current_row, obj_env$df_obj, exclude = T)
  }

  i__all_event_functions[["Summary Table"]][["Add Table to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_table(session_name, obj_env$table_objects_list$current_row, obj_env$df_obj, exclude = T)
  }

  i__all_event_functions[["Summary Table"]][["Add Count to df"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    cross_tab_names <- setdiff(colnames(obj_env$table_objects_list$current_row$row), c("r__", "n", "freq", "lines"))
    outer_env$u__add_count_to_df_summary(session_name, cross_tab_names)
  }

  #--------------------------------------------

  # past code

  #-------------------------------------------
  i__all_event_functions[["Past Code Table"]][["Load Code"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__set_before_code(session_name, cmd = current_row$row[, "code", drop = T])
  }

  #--------------------------------------------

  # file history

  #-------------------------------------------

  i__all_event_functions[["File History Table"]][["New Session"]] <- function(session_name, current_row, view_objects, outer_env = totem) {
    outer_env$start(current_row$row[, "full_path", drop = T])
    outer_env$hide_file_history_window()
  }


  #--------------------------------------------

  # Tail

  #-------------------------------------------


  return(i__all_event_functions)
}
