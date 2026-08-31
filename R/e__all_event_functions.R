#' e__all_event_functions
#'
#' @param outer_env TODO
#'
#' @return TODO

e__all_event_functions <- function(outer_env = totem) {
  i__all_event_functions <- list()

  #Helper to extract frequency data based on table type.
  get_comparison_data <- function(session_name, current_row, outer_env, obj_env, table_type) {
    # 1. Universally prevent comparison if unique by is active.
    unique_cb <- RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$unique_by_cb)
    unique_txt <- trimws(RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$unique_by_entry))
    has_unique <- unique_cb && unique_txt != ""
    
    if (has_unique) {
      err_dialog <- RGtk2::gtkMessageDialog(
        parent = outer_env[[session_name]]$windows$main_window,
        flags = "destroy-with-parent",
        type = "error",
        buttons = "close",
        "Cannot pin or compare data when unique by is active."
      )
      err_dialog$run()
      RGtk2::gtkWidgetDestroy(err_dialog)
      return(NULL)
    }

    # 2. Extract data safely.
    if (table_type == "Summary Table") {
      current_data <- obj_env$df_obj$current_data()
      
      #Extract all grouping and target columns by removing internal UI variables.
      cross_tab_names <- setdiff(colnames(current_data), c("r__", "n", "freq", "lines", "nchar"))
      
      res <- as.data.frame(current_data[, c(cross_tab_names, "n"), drop = FALSE], stringsAsFactors = FALSE)
      res$n <- as.numeric(res$n)
      
      #Convert all key columns to character to ensure safe merging.
      for (col in cross_tab_names) {
        res[[col]] <- as.character(res[[col]])
      }
      
      return(list(keys = cross_tab_names, data = res))
      
    } else {
      #Meta Table or Full Data Table logic.
      col_name <- current_row$column
      
      #Determine base data (Full Data Table local filters are respected via data2).
      temp_df <- outer_env[[session_name]]$data2
      
      #Check for Group By variables.
      group_cb <- RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$group_by_cb)
      group_txt <- trimws(RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry))
      
      group_cols <- c()
      if (group_cb && group_txt != "") {
        parsed_groups <- trimws(strsplit(group_txt, ",")[[1]])
        parsed_groups <- parsed_groups[parsed_groups != ""]
        group_cols <- intersect(parsed_groups, colnames(temp_df))
      }
      
      #Combine grouping columns with the target column.
      all_keys <- unique(c(group_cols, col_name))
      all_keys <- intersect(all_keys, colnames(temp_df))
      
      target_data <- temp_df[, all_keys, drop = FALSE]
      
      #Convert NA to "NA" for grouping consistency.
      for (col in all_keys) {
        char_vals <- as.character(target_data[[col]])
        char_vals[is.na(char_vals)] <- "NA"
        target_data[[col]] <- char_vals
      }
      
      #Generate frequency table dynamically.
      if (length(all_keys) > 1) {
        res <- as.data.frame(table(target_data), stringsAsFactors = FALSE)
        
        #Drop combinations that have 0 frequency.
        res <- res[res$Freq > 0, , drop = FALSE]
        colnames(res)[colnames(res) == "Freq"] <- "n"
      } else {
        res <- as.data.frame(table(target_data[[all_keys[1]]]), stringsAsFactors = FALSE)
        colnames(res) <- c(all_keys[1], "n")
      }
      
      #Ensure data types are safe for merging.
      res$n <- as.numeric(res$n)
      for (col in all_keys) {
        res[[col]] <- as.character(res[[col]])
      }
      
      return(list(keys = all_keys, data = res))
    }
  }

  #Action for pinning the column.
  action_pin <- function(session_name, current_row, view_objects, outer_env, obj_env, table_type) {
    comp_info <- get_comparison_data(session_name, current_row, outer_env, obj_env, table_type)
    if (is.null(comp_info)) return()
    
    pinned_data <- list(
      dataset = outer_env[[session_name]]$sas_file_basename,
      keys = comp_info$keys,
      data = comp_info$data
    )
    
    #Write to cross-session RDS file.
    pinned_path <- file.path(outer_env$settings_dir_path, "pinned_comparison.rds")
    saveRDS(pinned_data, file = pinned_path)
    
    if (outer_env$settings_list$copy_messages) outer_env$u__show_toast(session_name, "Data pinned for cross-session comparison")
  }

  #Action for compare with pinned.
  action_compare <- function(session_name, current_row, view_objects, outer_env, obj_env, table_type) {
    #Determine path to the cross-session RDS file.
    pinned_path <- file.path(outer_env$settings_dir_path, "pinned_comparison.rds")
    
    #Check if file exists before trying to read.
    if (!file.exists(pinned_path)) {
      err_dialog <- RGtk2::gtkMessageDialog(
        parent = outer_env[[session_name]]$windows$main_window,
        flags = "destroy-with-parent",
        type = "error",
        buttons = "close",
        "No data is currently pinned for comparison across sessions."
      )
      err_dialog$run()
      RGtk2::gtkWidgetDestroy(err_dialog)
      return()
    }
    
    #Safely read the pinned data to prevent crashes if file is locked.
    pinned <- try(readRDS(pinned_path), silent = TRUE)
    if (inherits(pinned, "try-error")) {
      err_dialog <- RGtk2::gtkMessageDialog(
        parent = outer_env[[session_name]]$windows$main_window,
        flags = "destroy-with-parent",
        type = "error",
        buttons = "close",
        "Failed to read the pinned comparison file. Try pinning the data again."
      )
      err_dialog$run()
      RGtk2::gtkWidgetDestroy(err_dialog)
      return()
    }
    
    comp_info <- get_comparison_data(session_name, current_row, outer_env, obj_env, table_type)
    if (is.null(comp_info)) return()
    
    current <- list(
      dataset = outer_env[[session_name]]$sas_file_basename,
      keys = comp_info$keys,
      data = comp_info$data
    )
    
    #Determine matching keys for the merge.
    merge_keys <- intersect(pinned$keys, current$keys)
    
    if (length(merge_keys) == 0) {
      err_dialog <- RGtk2::gtkMessageDialog(
        parent = outer_env[[session_name]]$windows$main_window,
        flags = "destroy-with-parent",
        type = "error",
        buttons = "close",
        "Cannot compare: The pinned data and current data have no columns in common."
      )
      err_dialog$run()
      RGtk2::gtkWidgetDestroy(err_dialog)
      return()
    }
    
    #Strip file extensions for cleaner headers.
    clean_pinned_ds <- sub("\\.[^.]+$", "", pinned$dataset)
    clean_current_ds <- sub("\\.[^.]+$", "", current$dataset)
    
    col_pinned <- paste0("Pinned Counts\n", clean_pinned_ds)
    col_current <- paste0("Comparison Counts\n", clean_current_ds)
    
    #Explicitly rename the target metrics before merging to guarantee column safety.
    colnames(pinned$data)[colnames(pinned$data) == "n"] <- col_pinned
    colnames(current$data)[colnames(current$data) == "n"] <- col_current
    
    #Merge the data dynamically based on the overlapping keys.
    merged_df <- merge(pinned$data, current$data, by = merge_keys, all = TRUE)
    
    #Add presence indicator columns before zeroing out NAs.
    merged_df$Pinned <- ifelse(!is.na(merged_df[[col_pinned]]), "Y", "")
    merged_df$Comparison <- ifelse(!is.na(merged_df[[col_current]]), "Y", "")
    
    #Replace missing counts with zero.
    merged_df[[col_pinned]][is.na(merged_df[[col_pinned]])] <- 0
    merged_df[[col_current]][is.na(merged_df[[col_current]])] <- 0
    
    #Calculate difference and match.
    merged_df$Difference <- merged_df[[col_current]] - merged_df[[col_pinned]]
    merged_df$Match <- ifelse(merged_df$Difference == 0, "Y", "")
    
    #Reorder columns for logical flow.
    metric_cols <- c(col_pinned, col_current, "Match", "Difference", "Pinned", "Comparison")
    
    #Ensure all grouping and target keys lead the table.
    all_keys_union <- unique(c(pinned$keys, current$keys))
    merged_df <- merged_df[, c(all_keys_union, metric_cols)]
    
    #Dynamically generate the window title based on the keys used.
    title_str <- paste0("Comparison: ", paste(pinned$keys, collapse = ", "), " vs ", paste(current$keys, collapse = ", "))
    
    outer_env$u__df_view(
      merged_df, 
      title_str, 
      height = 400, width = 800
    )
  }
  
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
  i__all_event_functions[["General"]][["Open Context Menu"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    if (is.null(view_objects$event) == F) {
      
      # Dynamically hide or show menu items based on settings right before pop-up
      if (!is.null(outer_env$settings_list$menu_items_show)) {
        for (config_m in names(outer_env$settings_list$menu_items_show)) {
          for (item_m in names(outer_env$settings_list$menu_items_show[[config_m]])) {
            is_shown <- outer_env$settings_list$menu_items_show[[config_m]][[item_m]]
            end_node <- paste0("base|", config_m, "|", item_m)

            if (!is.null(obj_env$menubar$items[[end_node]])) {
              if (is_shown) {
                RGtk2::gtkWidgetShow(obj_env$menubar$items[[end_node]])
              } else {
                RGtk2::gtkWidgetHide(obj_env$menubar$items[[end_node]])
              }
            }
          }
        }
      }

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
    # 1. Check preferences (and prompt if needed)
    if (!outer_env$u__check_code_prefs(session_name)) return()
    
    # 2. Extract spacing
    sp <- ifelse(outer_env$settings_list$code_spacing == "Spaced (x = y)", " = ", "=")
    
    column_classes <- obj_env$df_obj$get_column_classes()
    if (column_classes[obj_env$table_objects_list$current_row$column] == "numeric") {
      utils::writeClipboard(str = charToRaw(paste0(obj_env$table_objects_list$current_row$column, sp, obj_env$table_objects_list$current_row$value, " ")), format = 1)
    } else {
      utils::writeClipboard(str = charToRaw(paste0(obj_env$table_objects_list$current_row$column, sp, "\"", obj_env$table_objects_list$current_row$value, "\" ")), format = 1)
    }
    if (totem$settings_list$copy_messages) outer_env$u__show_toast(session_name, "Code copied to clipboard")
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

  i__all_event_functions[["Copy"]][["Column Wide"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    #Get the currently filtered data for the selected column
    col_name <- current_row$column
    col_data <- obj_env$df_obj$current_data()[, col_name, drop = TRUE]
    
    #Collapse the column values with tabs
    col_string <- paste0(as.character(col_data), collapse = "\t")
    utils::writeClipboard(str = charToRaw(paste0(col_string, " ")), format = 1)
    
    if (totem$settings_list$copy_messages) outer_env$u__show_toast(session_name, "Wide column copied to clipboard")
  }

  i__all_event_functions[["Copy"]][["Vector Column full"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_full(obj_env$table_objects_list$current_row$column, vector = T)
  }

  i__all_event_functions[["Copy"]][["Vector Column filtered"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_filter(obj_env$table_objects_list$current_row$column, vector = T)
  }

  i__all_event_functions[["Copy"]][["Row"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    # Extract row and drop internal UI metrics so only the true data is copied
    row_data <- current_row$row
    clean_cols <- setdiff(colnames(row_data), c("r__", "n", "freq", "lines", "nchar"))
    row_data <- row_data[, clean_cols, drop = FALSE]
    
    # Collapse the row values with tabs
    row_string <- paste0(as.character(row_data[1, ]), collapse = "\t")
    utils::writeClipboard(str = charToRaw(paste0(row_string, " ")), format = 1)
    
    if (totem$settings_list$copy_messages) outer_env$u__show_toast(session_name, "Row copied to clipboard")
  }

  #--------------------------------------------

  # meta table

  #-------------------------------------------
  i__all_event_functions[["Meta Table"]][["Trigger Value Summary"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    row_i <- current_row$row_i
    view_objects$event_mapping[["Meta Table|Trigger Value Summary"]](session_name, current_data[row_i, "variable", drop = T])
  }
  i__all_event_functions[["Meta Table"]][["Trigger Value Summary with Group By"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    row_i <- current_row$row_i
    view_objects$event_mapping[["Meta Table|Trigger Value Summary with Group By"]](session_name, current_data[row_i, "variable", drop = T])
  }
  i__all_event_functions[["Meta Table"]][["Trigger Value Summary with Unique By"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    row_i <- current_row$row_i
    view_objects$event_mapping[["Meta Table|Trigger Value Summary with Unique By"]](session_name, current_data[row_i, "variable", drop = T])
  }
  i__all_event_functions[["Meta Table"]][["Copy dataset layout"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_dataset_layout()
  }
  i__all_event_functions[["Meta Table"]][["Copy keep statement"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_keep()
  }
  i__all_event_functions[["Meta Table"]][["Copy label statement"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_label()
  }
  i__all_event_functions[["Meta Table"]][["Copy length statement"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$copy_length()
  }
  i__all_event_functions[["Meta Table"]][["Add Count to df"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    column <- current_data[current_row$row_i, "variable", drop = T]
    outer_env$u__add_count_to_df_summary(session_name, column)
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
  i__all_event_functions[["Meta Table"]][["Add Column to select"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    col_to_toggle <- current_data[current_row$row_i, "variable", drop = T]
    
    st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
    
    if (st != "") {
      # Split by comma, but use negative lookahead to ignore commas inside parentheses
      current_cols <- trimws(strsplit(st, split = ",(?![^(]*\\))", perl = TRUE)[[1]])
      
      if (col_to_toggle %in% current_cols) {
        # If it's already there, remove it (toggle off)
        current_cols <- setdiff(current_cols, col_to_toggle)
      } else {
        # If it's not there, add it (toggle on)
        current_cols <- c(current_cols, col_to_toggle)
      }
      # Rebuild the comma-separated string
      st <- paste0(current_cols, collapse = ", ")
    } else {
      st <- col_to_toggle
    }
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, st)
  }
  i__all_event_functions[["Meta Table"]][["Format by Column"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    col_to_set <- current_data[current_row$row_i, "variable", drop = T]
    
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$format_by_entry, col_to_set)
    
    outer_env[[session_name]]$data_view_list$slot1_list$full_table$update(outer_env[[session_name]]$data2)
    RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$slot2_box)
  }
  i__all_event_functions[["Meta Table"]][["Add'l format by Column"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    current_data <- obj_env$df_obj$current_data()
    col_to_set <- current_data[current_row$row_i, "variable", drop = T]
    
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$format_by_entry2, col_to_set)
    
    outer_env[[session_name]]$data_view_list$slot1_list$full_table$update(outer_env[[session_name]]$data2)
    RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$slot2_box)
  }
  i__all_event_functions[["Meta Table"]][["Pin for Comparison"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    action_pin(session_name, current_row, view_objects, outer_env, obj_env, "Meta Table")
  }
  i__all_event_functions[["Meta Table"]][["Compare with Pinned"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    action_compare(session_name, current_row, view_objects, outer_env, obj_env, "Meta Table")
  }
  i__all_event_functions[["Meta Table"]][["Freeze/Unfreeze Column"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    #Extract the true column name from the clicked row in the meta table.
    current_data <- obj_env$df_obj$current_data()
    col_name <- current_data[current_row$row_i, "variable", drop = T]
    
    #Route the freeze command directly to the Full Data Table view below.
    outer_env[[session_name]]$data_view_list$slot1_list$full_table$freeze_column(col_name)
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
  i__all_event_functions[["Full Data Table"]][["Trigger Value Summary with Unique By"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    column <- current_row$column
    view_objects$event_mapping[["Full Data Table|Trigger Value Summary with Unique By"]](session_name, column)
  }
  i__all_event_functions[["Full Data Table"]][["Add Column to select"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
    col_to_toggle <- obj_env$table_objects_list$current_row$column
    
    if (st != "") {
      # Split by comma, but use negative lookahead to ignore commas inside parentheses
      current_cols <- trimws(strsplit(st, split = ",(?![^(]*\\))", perl = TRUE)[[1]])
      
      if (col_to_toggle %in% current_cols) {
        # If it's already there, remove it (toggle off)
        current_cols <- setdiff(current_cols, col_to_toggle)
      } else {
        # If it's not there, add it (toggle on)
        current_cols <- c(current_cols, col_to_toggle)
      }
      # Rebuild the comma-separated string
      st <- paste0(current_cols, collapse = ", ")
    } else {
      st <- col_to_toggle
    }
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, st)
  }
  i__all_event_functions[["Full Data Table"]][["Add to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data(session_name, obj_env$table_objects_list$current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Add Bucket to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data_bucket(session_name, obj_env$table_objects_list$current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Add to Main Filter Exclude"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data(session_name, obj_env$table_objects_list$current_row, exclude = T)
  }
  i__all_event_functions[["Full Data Table"]][["Add to Main Filter (no combining)"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_full_data(session_name, obj_env$table_objects_list$current_row, combine = F)
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
  i__all_event_functions[["Full Data Table"]][["Add grepl to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    col <- current_row$column
    val <- current_row$value
    
    # Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", col)) { 
      clean_col <- paste0("`", col, "`") 
    } else {
      clean_col <- col
    }
    
    # Build the string and append it to the code area
    cmd <- paste0("df <- df %>% filter(grepl('", val, "', ", clean_col, ", ignore.case = T))")
    outer_env$u__append_before_code(session_name, cmd)
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
  i__all_event_functions[["Full Data Table"]][["Scatterplot Summary"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__scatter_summary(session_name, current_row)
  }

  i__all_event_functions[["Full Data Table"]][["Move column before"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$move_column(0, session_name, current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Move column after"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$move_column(1, session_name, current_row)
  }
  i__all_event_functions[["Full Data Table"]][["Format by Column"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    col_to_set <- current_row$column
    
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$format_by_entry, col_to_set)
    
    outer_env[[session_name]]$data_view_list$slot1_list$full_table$update(outer_env[[session_name]]$data2)
    RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$slot2_box)
  }
  i__all_event_functions[["Full Data Table"]][["Add'l format by Column"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    col_to_set <- current_row$column
    
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$format_by_entry2, col_to_set)
    
    outer_env[[session_name]]$data_view_list$slot1_list$full_table$update(outer_env[[session_name]]$data2)
    RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$slot2_box)
  }
  i__all_event_functions[["Full Data Table"]][["Pin for Comparison"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    action_pin(session_name, current_row, view_objects, outer_env, obj_env, "Full Data Table")
  }
  i__all_event_functions[["Full Data Table"]][["Compare with Pinned"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    action_compare(session_name, current_row, view_objects, outer_env, obj_env, "Full Data Table")
  }
  i__all_event_functions[["Full Data Table"]][["Freeze/Unfreeze Column"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    obj_env$df_obj$freeze_column(current_row$column)
  }

  #--------------------------------------------

  # Summary table

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
  i__all_event_functions[["Summary Table"]][["Add to Main Filter (no combining)"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter(session_name, obj_env$table_objects_list$current_row, combine = F)
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
  i__all_event_functions[["Summary Table"]][["Add grepl to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    col <- current_row$column
    val <- current_row$value
    
    # Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", col)) { 
      clean_col <- paste0("`", col, "`") 
    } else {
      clean_col <- col
    }
    
    # Build the string and append it to the code area
    cmd <- paste0("df <- df %>% filter(grepl('", val, "', ", clean_col, ", ignore.case = T))")
    outer_env$u__append_before_code(session_name, cmd)
  }
  i__all_event_functions[["Summary Table"]][["Add Table to Main Filter"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__add_before_filter_table(session_name, current_row)
  }
  i__all_event_functions[["Summary Table"]][["Copy Mapping"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$u__copy_mapping(session_name, current_row)
  }
  i__all_event_functions[["Summary Table"]][["Copy Data Columns"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    # Fetch the full summary table data currently on screen
    current_data <- obj_env$df_obj$current_data()
    
    # Exclude the metrics and UI columns to isolate the grouping/focus columns
    cross_tab_names <- setdiff(colnames(current_data), c("r__", "n", "freq", "lines", "nchar"))
    
    # Subset the data frame
    data_to_copy <- current_data[, cross_tab_names, drop = FALSE]
    
    # Copy to clipboard using clipr (matches the behavior of other table copies)
    clipr::write_clip(data_to_copy, allow_non_interactive = T)
    
    # Trigger the toast notification
    if (totem$settings_list$copy_messages) outer_env$u__show_toast(session_name, "Data columns copied to clipboard")
  }
  i__all_event_functions[["Summary Table"]][["Pin for Comparison"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    action_pin(session_name, current_row, view_objects, outer_env, obj_env, "Summary Table")
  }
  i__all_event_functions[["Summary Table"]][["Compare with Pinned"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    action_compare(session_name, current_row, view_objects, outer_env, obj_env, "Summary Table")
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

  i__all_event_functions[["File History Table"]][["New Session"]] <- function(session_name, current_row, view_objects, outer_env = totem, obj_env = inner_env) {
    outer_env$start(current_row$row[, "path", drop = T])
    # outer_env$hide_file_history_window()
  }


  #--------------------------------------------

  # Tail

  #-------------------------------------------


  return(i__all_event_functions)
}
