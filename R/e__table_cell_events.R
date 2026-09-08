#' e__table_cell_events
#'
#' @param event TODO
#' @param row.idx TODO
#' @param col.idx TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

#Handles mouse click events and triggers corresponding shortcut actions.
e__table_cell_events <- function(event, row.idx, col.idx, outer_env = totem, obj_env = inner_env) {
  #Determine click combination state.
  current_state <- z__event_state(event)

  #Cancel shortcut trigger flag on text area.
  if (!is.null(outer_env[[session_name]])) {
    outer_env[[session_name]]$cancel_ctrl_shift <- TRUE
  }

  row_i <- row.idx + obj_env$page_obj$get_page() - 1
  column <- col.idx

  current_data <- obj_env$df_obj$current_data()
  if (nrow(current_data) < row_i) {
    return(TRUE)
  }
  value <- current_data[row_i, column, drop = TRUE]

  #Update active row tracking list.
  obj_env$table_objects_list$current_row <- list(
    row.idx = row.idx, col.idx = col.idx, value = value,
    column = column, row = current_data[row_i, , drop = FALSE], row_i = row_i
  )

  if (is_file_history_table == FALSE) {
    if (!is.null(outer_env[[session_name]]$status_bar)) {
      RGtk2::gtkLabelSetLabel(outer_env[[session_name]]$status_bar$info_label_cell, paste0("| Cell length: ", nchar(value)))

      if (outer_env[[session_name]]$status_bar$box_bucket_showing && current_state == "left+none") {
        column_classes <- obj_env$df_obj$get_column_classes()

        if (column_classes[column] == "numeric") {
          sep <- ""
        } else {
          sep <- "\""
        }

        temp_string <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$status_bar$box_bucket_entry)
        if (temp_string == "") {
          temp_string <- paste0(sep, value, sep)
        } else {
          temp_string <- paste0(temp_string, ", ", sep, value, sep)
        }
        RGtk2::gtkEntrySetText(outer_env[[session_name]]$status_bar$box_bucket_entry, temp_string)
      }
    }
  }

  config_i <- ""
  item_i <- ""
  table_events <- outer_env$settings_list$table_events

  #Match active key combination directly against saved event settings.
  for (area_j in names(table_events)) {
    for (item_j in names(table_events[[area_j]])) {
      val_i <- table_events[[area_j]][[item_j]]

      if (current_state == val_i && val_i != "-") {
        config_i <- area_j
        item_i <- item_j
        break
      }
    }
    if (config_i != "") break
  }

  #Execute function bound to matched event.
  if (config_i %in% names(outer_env$all_event_functions)) {
    if (item_i %in% names(outer_env$all_event_functions[[config_i]])) {
      view_objects <- list(event_mapping = event_mapping, event = event)
      outer_env$all_event_functions[[config_i]][[item_i]](session_name, obj_env$table_objects_list$current_row, view_objects, outer_env = outer_env, obj_env = obj_env)
    }
  }
}
