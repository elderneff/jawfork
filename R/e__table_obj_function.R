
#' e__table_obj_function_df2
#'
#' @param df TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__table_obj_function_df2 <- function(df, outer_env = totem,obj_env=inner_env) {

  if (nrow(df) == 0) {
    df2 <- matrix("#F1F1F1", ncol = 2, nrow = nrow(df))
    colnames(df2) <- c("f___1", "f___2")
    return(df2)
  }

  df2 <- matrix("#F1F1F1", ncol = 2, nrow = nrow(df))

  #Get format by variable
  if ("format_by_entry" %in% names(outer_env[[session_name]])) {
    format_var <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$format_by_entry)
  } else {
    format_var <- "USUBJID"
  }
  #Get add'l format by variable
  if ("format_by_entry2" %in% names(outer_env[[session_name]])) {
    format_var2 <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$format_by_entry2)
  } else {
    format_var2 <- ""
  }

  if (format_var %in% colnames(df)) {
    #One format by variable
    if (format_var2 %in% colnames(df) == F) {
      # levels <- dplyr::consecutive_id(df[, format_var])
      
      levels <- as.numeric(as.factor(df[, format_var, drop = T]))
      levels[is.na(levels)] <- -98  
      levels_temp <- c(-99, levels[1:(length(levels) - 1)])
      levels <- cumsum((levels != levels_temp) * 1)
    }
    #Two format by variables
    else if (format_var2 %in% colnames(df)) {
      # levels <- dplyr::consecutive_id(df[, format_var])
      # levels2 <- dplyr::consecutive_id(df[, format_var2])
      
      levels <- as.numeric(as.factor(df[, format_var, drop = T]))
      levels[is.na(levels)] <- -98  
      levels_temp <- c(-99, levels[1:(length(levels) - 1)])
      levels <- cumsum((levels != levels_temp) * 1)
      
      levels2 <- as.numeric(as.factor(df[, format_var2, drop = T]))
      levels2[is.na(levels2)] <- -98  
      # levels_temp2 <- c(-99, levels[1:(length(levels2) - 1)])
      # levels2 <- cumsum((levels2 != levels_temp2) * 1)
    }
  
    tryCatch(
      {
        #One format by variable
        if (format_var2 %in% colnames(df) == F) {
          df2[, 2] <- ifelse((levels %% 2) == 0, ifelse((1:nrow(df) %% 2) == 0, "#fcf7e8", "#f4efe1"),
            ifelse((1:nrow(df) %% 2) == 0, "#e8edfc", "#e1e5f4")
          )
        }
        #Two format by variables
        else if (format_var2 %in% colnames(df)) {
          df2[, 2] <- ifelse((levels %% 2) == 0 & (levels2 %% 2) == 0, ifelse((1:nrow(df) %% 2) == 0, "#fcf7e8", "#f4efe1"),
             ifelse((levels %% 2) == 0 & (levels2 %% 2) == 1, ifelse((1:nrow(df) %% 2) == 0, "#FCEEE8", "#F4E3E1"),
             ifelse((levels %% 2) == 1 & (levels2 %% 2) == 1, ifelse((1:nrow(df) %% 2) == 0, "#e8edfc", "#e1e5f4"),
            ifelse((1:nrow(df) %% 2) == 0, "#D1D1EC", "#C9C9E9")
          )))
        }
      },
      #Colors for when there is no Format by:
      error = function(e) {
        df2[, 2] <- ifelse((1:nrow(df) %% 2) == 0, "#F1F1F1", "#FFFFFF")
      }
    )
  } else {
    #Colors for when there is no Format by:
    df2[, 2] <- ifelse((1:nrow(df) %% 2) == 0, "#FFFFFF", "#F1F1F1")
  }

  # Check local filter states to see if any contain text
  has_filter <- !is.null(obj_env$filter_obj) && obj_env$filter_obj$get() != ""
  has_arrange <- !is.null(obj_env$order_by_obj) && obj_env$order_by_obj$get() != ""
  has_select <- !is.null(obj_env$select_obj) && obj_env$select_obj$get() != ""
  
  #r__ Color
  if (has_filter || has_arrange || has_select) {
    df2[, 1] <- "#F4D9D9"
  } else {
    df2[, 1] <- "#9bb5f5"
  }

  colnames(df2) <- c("f___1", "f___2")
  return(df2)
}


#' e__table_obj_function
#'
#' @param box TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__table_obj_function <- function(box, outer_env = totem,obj_env=inner_env) {

  obj_env$table_objects_list <- list()
  obj_env$table_objects_list$current_row <- NA


  obj_env$table_objects_list$inner_box <- RGtk2::gtkVBox()
  RGtk2::gtkBoxPackStart(box, obj_env$table_objects_list$inner_box, T, T)


  obj_env$table_objects_list$current_columns <- c("x")
  obj_env$table_objects_list$raw_df <- data.frame("x" = character())
  obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(obj_env$table_objects_list$raw_df)
  obj_env$table_objects_list$view <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)
  obj_env$table_objects_list$allColumns <- vector("list", 1)

  update_table <- function(df) {


    if ((paste0(obj_env$table_objects_list$current_columns, collapse = "|") == paste0(colnames(df), collapse = "|")) == F) {
      obj_env$table_objects_list$current_columns <- colnames(df)

      df2 <- obj_env$table_obj_function_df2(df)
      df <- cbind(df, df2)

      RGtk2::gtkWidgetDestroy(obj_env$table_objects_list$inner_box)
      obj_env$table_objects_list$inner_box <- RGtk2::gtkVBox()
      RGtk2::gtkBoxPackStart(box, obj_env$table_objects_list$inner_box, T, T)

      # Create the shared model
      obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(df)

      # Create two views sharing the same model
      obj_env$table_objects_list$view <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)
      obj_env$table_objects_list$view_frozen <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)

      RGtk2::gtkTreeViewSetFixedHeightMode(obj_env$table_objects_list$view, F)
      RGtk2::gtkTreeViewSetFixedHeightMode(obj_env$table_objects_list$view_frozen, F)
      
      obj_env$table_objects_list$allColumns <- vector("list", ncol(df) - 2)

      # Append columns to their respective views
      for (j in seq_len(ncol(df) - 2)) {
        tmp <- obj_env$new_tree_view_column(df, j)

        # If it's the row number column, put it in the frozen view
        if (colnames(df)[j] == "r__") {
          RGtk2::gtkTreeViewAppendColumn(obj_env$table_objects_list$view_frozen, tmp$column)
        } else {
          RGtk2::gtkTreeViewAppendColumn(obj_env$table_objects_list$view, tmp$column)
        }
        
        obj_env$table_objects_list$allColumns[[j]] <- tmp
      }


      selectedColor <- RGtk2::as.GdkColor(c(198, 213, 253) * 256) # Linux

      # Styling for Main View
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], "#e7e3cd")
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], "#e7e3cd")
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], RGtk2::as.GdkColor("black"))
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], RGtk2::as.GdkColor("black"))

      # Styling for Frozen View
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["selected"], "#e7e3cd")
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["active"], "#e7e3cd")
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["selected"], RGtk2::as.GdkColor("black"))
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["active"], RGtk2::as.GdkColor("black"))

      #Failed attempt at dark mode
      #RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], "#302459")
      #RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], "#302459")
      #RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], RGtk2::as.GdkColor("grey"))
      #RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], RGtk2::as.GdkColor("grey"))

      # Main Scrolled Window
      sw <- RGtk2::gtkScrolledWindow()
      RGtk2::gtkScrolledWindowSetPolicy(sw, "automatic", "automatic")
      RGtk2::gtkContainerAdd(sw, obj_env$table_objects_list$view)

      # Frozen Scrolled Window (No scrollbars visible)
      sw_frozen <- RGtk2::gtkScrolledWindow()
      RGtk2::gtkScrolledWindowSetPolicy(sw_frozen, "never", "never") 
      RGtk2::gtkContainerAdd(sw_frozen, obj_env$table_objects_list$view_frozen)

      # Synchronize the vertical adjustments
      adj <- RGtk2::gtkScrolledWindowGetVadjustment(sw)
      RGtk2::gtkScrolledWindowSetVadjustment(sw_frozen, adj)

      # Pack main and frozen side-by-side in an HBox
      table_hbox <- RGtk2::gtkHBox()
      RGtk2::gtkBoxPackStart(table_hbox, sw_frozen, F, F) # Expand=F keeps it tight to the column
      RGtk2::gtkBoxPackStart(table_hbox, sw, T, T)        # Expand=T gives the rest of the screen to the data

      RGtk2::gtkBoxPackStart(obj_env$table_objects_list$inner_box, table_hbox, T, T)

      # Bind cell click events to both views
      RGtk2::gSignalConnect(obj_env$table_objects_list$view, "button-press-event", obj_env$tree_view_column_btn_press, data = obj_env)
      RGtk2::gSignalConnect(obj_env$table_objects_list$view_frozen, "button-press-event", obj_env$tree_view_column_btn_press, data = obj_env)

      # Sync Main View -> Frozen View
      RGtk2::gSignalConnect(obj_env$table_objects_list$view, "cursor-changed", function(widget, data) {
        frozen_view <- data
        cursor <- RGtk2::gtkTreeViewGetCursor(widget)
        
        if (!is.null(cursor$path)) {
          frozen_cursor <- RGtk2::gtkTreeViewGetCursor(frozen_view)
          # Only update if the paths are different to avoid an infinite loop
          if (is.null(frozen_cursor$path) || RGtk2::gtkTreePathToString(cursor$path) != RGtk2::gtkTreePathToString(frozen_cursor$path)) {
            RGtk2::gtkTreeViewSetCursor(frozen_view, cursor$path, NULL, FALSE)
          }
        }
        return(FALSE)
      }, data = obj_env$table_objects_list$view_frozen)

      # Sync Frozen View -> Main View
      RGtk2::gSignalConnect(obj_env$table_objects_list$view_frozen, "cursor-changed", function(widget, data) {
        main_view <- data
        cursor <- RGtk2::gtkTreeViewGetCursor(widget)
        
        if (!is.null(cursor$path)) {
          main_cursor <- RGtk2::gtkTreeViewGetCursor(main_view)
          # Only update if the paths are different to avoid an infinite loop
          if (is.null(main_cursor$path) || RGtk2::gtkTreePathToString(cursor$path) != RGtk2::gtkTreePathToString(main_cursor$path)) {
            RGtk2::gtkTreeViewSetCursor(main_view, cursor$path, NULL, FALSE)
          }
        }
        return(FALSE)
      }, data = obj_env$table_objects_list$view)

      # Shift + Scroll for horizontal scrolling
      RGtk2::gSignalConnect(obj_env$table_objects_list$view, "scroll-event", function(widget, event, data) {
        sw_main <- data
        
        # Extract the state integer (Shift mask is always bit 1 in GTK)
        state_int <- as.integer(event[["state"]])
        is_shift <- bitwAnd(state_int, 1) > 0
        
        if (is_shift) {
          direction <- event[["direction"]]
          hadj <- RGtk2::gtkScrolledWindowGetHadjustment(sw_main)
          
          # Determine scroll amount (multiplying the default step by 3 usually feels natural)
          step <- hadj$stepIncrement * 3
          if (is.null(step) || step == 0) step <- 50
          
          if (direction == RGtk2::GdkScrollDirection["up"]) {
            # Scroll Up = Move Left
            new_val <- max(hadj$lower, hadj$value - step)
            RGtk2::gtkAdjustmentSetValue(hadj, new_val)
            return(TRUE) # Return TRUE to tell GTK we handled the event (stops vertical scroll)
            
          } else if (direction == RGtk2::GdkScrollDirection["down"]) {
            # Scroll Down = Move Right
            new_val <- min(hadj$upper - hadj$pageSize, hadj$value + step)
            RGtk2::gtkAdjustmentSetValue(hadj, new_val)
            return(TRUE) # Return TRUE to tell GTK we handled the event (stops vertical scroll)
          }
        }
        
        return(FALSE) # Return FALSE to let GTK handle normal vertical scrolling
      }, data = sw)
    } else {
      obj_env$table_objects_list$raw_df <- df
      df2 <- obj_env$table_obj_function_df2(df)
      df <- cbind(df, df2)

      obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(df)

      RGtk2::gtkTreeViewSetModel(obj_env$table_objects_list$view, obj_env$table_objects_list$model)
      RGtk2::gtkTreeViewSetModel(obj_env$table_objects_list$view_frozen, obj_env$table_objects_list$model)
      
      RGtk2::gtkTreeViewColumnsAutosize(obj_env$table_objects_list$view)
      RGtk2::gtkTreeViewColumnsAutosize(obj_env$table_objects_list$view_frozen)
      
      if (is_full_data_table) {
        for (j in setdiff(seq_len(ncol(df) - 2), 1)) {
          data3 <- outer_env[[session_name]]$data3
          my_row <- data3[j - 1, ]
          my_tool_tip <- paste0(
            my_row[, "label"], "\nLength: ", my_row[, "length"],
            "\nClass: ", my_row[, "class"], "\nDistinct: ", my_row[, "distinct"],
            "\nUnique: ", my_row[, "unique"], "\nMissing: ", my_row[, "missing"],
            "\nBlank: ", my_row[, "blank"]
          )

          #######################################################
          # Reset labels so they accurately reflect subset data #
          #######################################################
          #####################
          # Get column labels #
          #####################
          if (totem$settings_list$columnlabel | totem$settings_list$columnunique) {
            if (totem$settings_list$columnlabel) {
              if (is.na(my_row[, "label"])) { pre_y <- "---" }
              else { pre_y <- my_row[, "label"] }
              
              col_length <- max(nchar((outer_env[[session_name]]$data2[[j - 1]])))
              if (is.na(col_length)) {col_length <- 0}
              ############################################################
              # Insert line breaks to prevent labels from being too long #
              ############################################################
              #Set max length based on max length of column values
              max_length <- max(20, col_length)
              # Split the text into words
              words <- strsplit(pre_y, " ")[[1]]
              # Initialize an empty result
              result <- ""
              # Track the current line length
              current_length <- 0      
              # Loop through each word
              for (word in words) {
                # Check if adding the word would exceed the max_length
                if (current_length + nchar(word) > max_length) {
                  # If so, add a line break and reset current_length
                  result <- paste0(result, " \n", word)
                  current_length <- nchar(word)
                } else {
                  # Otherwise, add the word to the current line
                  if (current_length > 0) result <- paste0(result, " ")
                  result <- paste0(result, word)
                  current_length <- current_length + nchar(word) + 1
                }
              }
            }
            if (totem$settings_list$columnlabel & totem$settings_list$columnunique) { RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[j]]$evt$y, paste0(result, " \nU: ", my_row[, "unique"])) }
            else if (totem$settings_list$columnlabel & !totem$settings_list$columnunique) { RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[j]]$evt$y, paste0(result, " ")) }
            else if (!totem$settings_list$columnlabel & totem$settings_list$columnunique) { RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[j]]$evt$y, paste0("U: ", my_row[, "unique"])) }
          }
          
          RGtk2::gtkWidgetSetTooltipText(obj_env$table_objects_list$allColumns[[j]]$evt$evb, my_tool_tip)
          if (my_row[, "class"] == "numeric") {
            RGtk2::gtkWidgetModifyBg(object = obj_env$table_objects_list$allColumns[[j]]$evt$evb, state = "normal", color = "#FFFFFF")
          } else {
            RGtk2::gtkWidgetModifyBg(object = obj_env$table_objects_list$allColumns[[j]]$evt$evb, state = "normal", color = "#FFFFFF")
          }
        }
      }
    }
    #Resize r__ column header to match all others
    if (is_full_data_table) {
      max_newlines <- 0
      
      # 1. Loop through all data columns to find the maximum number of newlines
      for (j in setdiff(seq_len(ncol(df) - 2), 1)) {
         col_text <- RGtk2::gtkLabelGetText(obj_env$table_objects_list$allColumns[[j]]$evt$y)
         if (!is.null(col_text) && col_text != "") {
            newlines <- stringr::str_count(col_text, "\n")
            max_newlines <- max(max_newlines, newlines)
         }
      }
      
      # 2. Apply that many newlines as a blank string to the r__ column
      if (max_newlines > 0) {
         blank_label <- paste0(rep(" \n", max_newlines), collapse = "")
         RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[1]]$evt$y, blank_label)
      } else {
         RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[1]]$evt$y, "")
      }
    }
  }


  clear_filters <- function() {
    obj_env$filter_obj$clean_inner()
  }


  clear_arrange <- function() {
    obj_env$order_by_obj$clean_inner()
  }

  clear_select <- function() {
    obj_env$select_obj$clean_inner()
  }

  add_select <- function(txt) {
    obj_env$select_obj$add(txt)
  }





  get_current_row <- function() {
    return(obj_env$table_objects_list$current_row)
  }
  hide <- function() {
    RGtk2::gtkWidgetHide(obj_env$table_objects_list$inner_box)
    return(T)
  }
  show <- function() {
    RGtk2::gtkWidgetShow(obj_env$table_objects_list$inner_box)
    return(T)
  }


  return(list(
    update_table = update_table,
    clear_filters = clear_filters, clear_arrange = clear_arrange, clear_select = clear_select, add_select = add_select, get_current_row = get_current_row, hide = hide, show = show
  ))
}
