#' e__table_cell_events
#'
#' @param event TODO
#' @param row.idx TODO
#' @param col.idx TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__table_cell_events <- function(event, row.idx, col.idx, outer_env = totem, obj_env = inner_env) {
  current_state <- z__event_state(event)

  row_i <- row.idx + obj_env$page_obj$get_page() - 1
  column <- col.idx

  current_data <- obj_env$df_obj$current_data()
  if (nrow(current_data) < row_i) {
    return(T)
  }
  value <- current_data[row_i, column, drop = T]

  obj_env$table_objects_list$current_row <- list(
    row.idx = row.idx, col.idx = col.idx, value = value,
    column = column, row = current_data[row_i, , drop = F], row_i = row_i
  )

  if (is_file_history_table == F) {
    #Check if the status bar exists to prevent pseudo-sessions from throwing errors
    if (!is.null(outer_env[[session_name]]$status_bar)) {
      RGtk2::gtkLabelSetLabel(outer_env[[session_name]]$status_bar$info_label_cell, paste0("| Cell length: ", nchar(value)))

      if (outer_env[[session_name]]$status_bar$box_bucket_showing & current_state == "left+none") {
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
  for (event_i in names(outer_env$settings_window$settings_config_objs)) {
    val_i <- outer_env$settings_window$settings_config_objs[[event_i]]$val
    area_j <- outer_env$settings_window$settings_config_objs[[event_i]]$area
    item_j <- outer_env$settings_window$settings_config_objs[[event_i]]$item

    if (current_state == val_i) {
      if (is_meta_table & area_j == "Meta Table") {
        config_i <- area_j
        item_i <- item_j
        break
      } else if (is_full_data_table & area_j == "Full Data Table") {
        config_i <- area_j
        item_i <- item_j
        break
      } else if (is_value_table & area_j == "Summary Table") {
        config_i <- area_j
        item_i <- item_j
        break
      } else if (is_data_code_table & area_j == "Past Code Table") {
        config_i <- area_j
        item_i <- item_j
        break
      } else if (is_file_history_table & area_j == "File History Table") {
        config_i <- area_j
        item_i <- item_j
        break
      } else if (area_j == "General") {
        config_i <- area_j
        item_i <- item_j
        break
      } else if (area_j == "Copy") {
        config_i <- area_j
        item_i <- item_j
        break
      }
    }
  }

  if (config_i %in% names(outer_env$all_event_functions)) {
    if (item_i %in% names(outer_env$all_event_functions[[config_i]])) {
      view_objects <- list(event_mapping = event_mapping, event = event)
      outer_env$all_event_functions[[config_i]][[item_i]](session_name, obj_env$table_objects_list$current_row, view_objects, outer_env = outer_env, obj_env = obj_env)
    }
  }
}
