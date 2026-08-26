
#' e__table_obj_function_df2
#'
#' @param df TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__table_obj_function_df2 <- function(df, outer_env = totem, obj_env = inner_env) {
  is_dark <- totem$settings_list$dark_mode
  
  if (nrow(df) == 0) {
  #Update ncol to 3 and add f___3 to the column names
  df2 <- matrix(ifelse(is_dark, "#2D2D2D", "#F1F1F1"), ncol = 3, nrow = nrow(df))
  colnames(df2) <- c("f___1", "f___2", "f___3")
  return(df2)
}

  df2 <- matrix(ifelse(is_dark, "#2D2D2D", "#F1F1F1"), ncol = 2, nrow = nrow(df))

  #Extract alternating theme sets based on style preferences
  #Primary: blues
  c_primary_1   <- ifelse(is_dark, "#263238", "#e8edfc")
  c_primary_2   <- ifelse(is_dark, "#21272A", "#e1e5f4")
  c_primary_3   <- ifelse(is_dark, "#384252", "#D1D1EC")
  c_primary_4   <- ifelse(is_dark, "#2A293B", "#C9C9E9")
  #Secondary: yellows
  c_secondary_1 <- ifelse(is_dark, "#3D2F27", "#fcf7e8")
  c_secondary_2 <- ifelse(is_dark, "#342720", "#f4efe1")
  c_secondary_3 <- ifelse(is_dark, "#4A3430", "#FCEEE8")
  c_secondary_4 <- ifelse(is_dark, "#392A27", "#F4E3E1")
  #Fallback to black and gray if no format set
  c_fallback_1  <- ifelse(is_dark, "#1E1E1E", "#FFFFFF")
  c_fallback_2  <- ifelse(is_dark, "#252525", "#F1F1F1")

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

  has_var1 <- format_var %in% colnames(df)
  has_var2 <- format_var2 %in% colnames(df)

  if (has_var1 || has_var2) {
    tryCatch({
      #Promote var2 to primary if var1 is missing
      active_var1 <- if (has_var1) format_var else format_var2
      active_var2 <- if (has_var1 && has_var2) format_var2 else ""

      vals1 <- df[, active_var1, drop = T]
      vals1[is.na(vals1)] <- "NA_VAL"
      vals1_prev <- c("FIRST_ROW_DUMMY", vals1[1:(length(vals1) - 1)])
      
      changed1 <- (vals1 != vals1_prev)
      levels <- cumsum(changed1)

      if (active_var2 == "") {
        df2[, 2] <- ifelse((levels %% 2) == 1,
          ifelse((1:nrow(df) %% 2) == 1, c_primary_1, c_primary_2),
          ifelse((1:nrow(df) %% 2) == 1, c_secondary_1, c_secondary_2)
        )
      } else {
        vals2 <- df[, active_var2, drop = T]
        vals2[is.na(vals2)] <- "NA_VAL"
        vals2_prev <- c("FIRST_ROW_DUMMY", vals2[1:(length(vals2) - 1)])

        changed2 <- changed1 | (vals2 != vals2_prev)
        levels2 <- ave(changed2, levels, FUN = cumsum)

        df2[, 2] <- ifelse((levels %% 2) == 1 & (levels2 %% 2) == 1, ifelse((1:nrow(df) %% 2) == 1, c_primary_1, c_primary_2),
                    ifelse((levels %% 2) == 1 & (levels2 %% 2) == 0, ifelse((1:nrow(df) %% 2) == 1, c_primary_3, c_primary_4),
                    ifelse((levels %% 2) == 0 & (levels2 %% 2) == 1, ifelse((1:nrow(df) %% 2) == 1, c_secondary_1, c_secondary_2),
                    ifelse((1:nrow(df) %% 2) == 1, c_secondary_3, c_secondary_4)
        )))
      }
    }, 
    error = function(e) {
      df2[, 2] <- ifelse((1:nrow(df) %% 2) == 1, c_fallback_1, c_fallback_2)
    })
  } else {
    df2[, 2] <- ifelse((1:nrow(df) %% 2) == 1, c_fallback_1, c_fallback_2)
  }

  #Local filter state checking remains unchanged
  has_filter <- !is.null(obj_env$filter_obj) && obj_env$filter_obj$get() != ""
  has_arrange <- !is.null(obj_env$order_by_obj) && obj_env$order_by_obj$get() != ""
  has_select <- !is.null(obj_env$select_obj) && obj_env$select_obj$get() != ""
  
  if (has_filter || has_arrange || has_select) {
    df2[, 1] <- ifelse(is_dark, "#5C2E2E", "#F4D9D9")
  } else {
    df2[, 1] <- ifelse(is_dark, "#1A365D", "#9bb5f5")
  }
  
  text_color <- ifelse(is_dark, "#E0E0E0", "#000000")
  df2 <- cbind(df2, rep(text_color, nrow(df2)))

  colnames(df2) <- c("f___1", "f___2", "f___3")
  return(df2)
}


#' e__table_obj_function
#'
#' @param box
#' @param outer_env
#' @param obj_env
#'
#' @return list

e__table_obj_function <- function(box, outer_env = totem, obj_env=inner_env) {

  obj_env$table_objects_list <- list()
  obj_env$table_objects_list$current_row <- NA

  obj_env$table_objects_list$inner_box <- RGtk2::gtkVBox()
  RGtk2::gtkBoxPackStart(box, obj_env$table_objects_list$inner_box, T, T)

  obj_env$table_objects_list$current_columns <- c("x")
  obj_env$table_objects_list$current_classes <- c("y")
  obj_env$table_objects_list$current_dark_mode <- NA
  #Track the explicitly frozen columns to trigger rebuilds
  obj_env$table_objects_list$current_frozen <- c()
  
  obj_env$table_objects_list$raw_df <- data.frame("x" = character())
  obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(obj_env$table_objects_list$raw_df)
  obj_env$table_objects_list$view <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)
  obj_env$table_objects_list$allColumns <- vector("list", 1)

  update_table <- function(df) {
    #Pad r__ to match the tallest cell in the row
    if (nrow(df) > 0 && "r__" %in% colnames(df)) {
      #Count newlines in each cell across all columns
      newlines_list <- lapply(seq_len(ncol(df)), function(j) {
        counts <- stringr::str_count(df[, j], "\n")
        counts[is.na(counts)] <- 0
        counts
      })
      
      newlines_mat <- do.call(cbind, newlines_list)
      
      #Find the max newlines per row and pad the r__ column
      if (nrow(df) == 1) {
        max_n <- max(newlines_mat)
        if (max_n > 0) df[, "r__"] <- paste0(df[, "r__"], paste0(rep("\n", max_n), collapse = ""))
      } else {
        max_n <- apply(newlines_mat, 1, max)
        pad <- sapply(max_n, function(n) if (n > 0) paste0(rep("\n", n), collapse = "") else "")
        df[, "r__"] <- paste0(df[, "r__"], pad)
      }
    }
    
    #Grab true column classes dynamically for the current table
    if (!is.null(obj_env$df_obj_list$column_classes)) {
      new_classes_str <- paste0(obj_env$df_obj_list$column_classes, collapse = "|")
    } else {
      new_classes_str <- ""
    }
    
    is_dark <- totem$settings_list$dark_mode
    
    #Parse the currently frozen columns dynamically from the UI text field
    frozen_cols <- c()
    if (!is.null(outer_env[[session_name]]$data_view_list$freeze_cb)) {
      if (RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$freeze_cb)) {
        freeze_txt <- trimws(RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$freeze_entry))
        if (freeze_txt != "") {
          frozen_cols <- trimws(strsplit(freeze_txt, ",")[[1]])
        }
      }
    }
    #Save it to the obj_env so e__add_column_label knows to tint the background when appending!
    obj_env$table_objects_list$frozen_columns <- frozen_cols
    
    #Rebuild if the column names, classes, theme, OR frozen list changes
    if ((paste0(obj_env$table_objects_list$current_columns, collapse = "|") == paste0(colnames(df), collapse = "|")) == F || 
        (paste0(obj_env$table_objects_list$current_classes, collapse = "|") == new_classes_str) == F || 
        !identical(obj_env$table_objects_list$current_dark_mode, is_dark) || 
        !identical(obj_env$table_objects_list$current_frozen, frozen_cols)) {
      
      #Capture old horizontal scroll position before destroying the table
      old_h_val <- 0
      if (!is.null(obj_env$table_objects_list$sw_main)) {
        hadj <- RGtk2::gtkScrolledWindowGetHadjustment(obj_env$table_objects_list$sw_main)
        if (!is.null(hadj)) old_h_val <- RGtk2::gtkAdjustmentGetValue(hadj)
      }
      
      obj_env$table_objects_list$current_columns <- colnames(df)
      obj_env$table_objects_list$current_classes <- new_classes_str
      obj_env$table_objects_list$current_dark_mode <- is_dark
      obj_env$table_objects_list$current_frozen <- frozen_cols
      
      df2 <- obj_env$table_obj_function_df2(df)
      df <- cbind(df, df2)
      
      RGtk2::gtkWidgetDestroy(obj_env$table_objects_list$inner_box)
      obj_env$table_objects_list$inner_box <- RGtk2::gtkVBox()
      RGtk2::gtkBoxPackStart(box, obj_env$table_objects_list$inner_box, T, T)
      
      #Create the shared model
      obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(df)
      
      #Create two views sharing the same model
      obj_env$table_objects_list$view <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)
      obj_env$table_objects_list$view_frozen <- RGtk2::gtkTreeViewNewWithModel(obj_env$table_objects_list$model)
      
      RGtk2::gtkTreeViewSetGridLines(obj_env$table_objects_list$view, "none")
      RGtk2::gtkTreeViewSetGridLines(obj_env$table_objects_list$view_frozen, "none")
      
      #Build all columns into a list first
      obj_env$table_objects_list$allColumns <- vector("list", ncol(df) - 3)
      for (j in seq_len(ncol(df) - 3)) {
        tmp <- obj_env$new_tree_view_column(df, j)
        obj_env$table_objects_list$allColumns[[j]] <- tmp
      }
      
      #1. Always append r__ to the frozen view first
      r_idx <- which(colnames(df)[1:(ncol(df) - 3)] == "r__")
      if (length(r_idx) > 0) {
        RGtk2::gtkTreeViewAppendColumn(obj_env$table_objects_list$view_frozen, obj_env$table_objects_list$allColumns[[r_idx[1]]]$column)
      }
      
      #2. Append user-specified frozen columns in their EXACT typed order
      for (col_name in frozen_cols) {
        col_idx <- which(colnames(df)[1:(ncol(df) - 3)] == col_name)
        if (length(col_idx) > 0) {
          RGtk2::gtkTreeViewAppendColumn(obj_env$table_objects_list$view_frozen, obj_env$table_objects_list$allColumns[[col_idx[1]]]$column)
        }
      }
      
      #3. Append the remaining columns to the normal view
      for (j in seq_len(ncol(df) - 3)) {
        col_name <- colnames(df)[j]
        if (col_name != "r__" && !(col_name %in% frozen_cols)) {
          RGtk2::gtkTreeViewAppendColumn(obj_env$table_objects_list$view, obj_env$table_objects_list$allColumns[[j]]$column)
        }
      }
      
      selectedColor <- RGtk2::as.GdkColor(c(198, 213, 253) * 256)
      
      #Styling for main view
      c_select <- ifelse(is_dark, "#244429", "#C3DFC8")
      text_color <- ifelse(is_dark, "#E0E0E0", "#000000")
      
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], c_select)
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], c_select)
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["selected"], text_color)
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view, RGtk2::GtkStateType["active"], text_color)
      
      #Styling for frozen view
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["selected"], c_select)
      RGtk2::gtkWidgetModifyBase(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["active"], c_select)
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["selected"], text_color)
      RGtk2::gtkWidgetModifyText(obj_env$table_objects_list$view_frozen, RGtk2::GtkStateType["active"], text_color)
      
      #Main scrolled window
      sw <- RGtk2::gtkScrolledWindow()
      RGtk2::gtkScrolledWindowSetPolicy(sw, "automatic", "automatic")
      RGtk2::gtkContainerAdd(sw, obj_env$table_objects_list$view)
      
      #Store reference to main scrolled window for future scroll tracking
      obj_env$table_objects_list$sw_main <- sw
      
      #Restore scroll position after GTK renders and calculates the new width boundaries
      if (old_h_val > 0) {
        RGtk2::gIdleAdd(function(data) {
          hadj <- RGtk2::gtkScrolledWindowGetHadjustment(data$sw)
          new_val <- min(data$old_val, max(0, hadj$upper - hadj$pageSize))
          RGtk2::gtkAdjustmentSetValue(hadj, new_val)
          return(FALSE)
        }, data = list(sw = sw, old_val = old_h_val))
      }
      
      #Frozen scrolled window
      sw_frozen <- RGtk2::gtkScrolledWindow()
      RGtk2::gtkScrolledWindowSetPolicy(sw_frozen, "never", "never") 
      RGtk2::gtkContainerAdd(sw_frozen, obj_env$table_objects_list$view_frozen)
      
      #Synchronize the vertical adjustments
      adj <- RGtk2::gtkScrolledWindowGetVadjustment(sw)
      RGtk2::gtkScrolledWindowSetVadjustment(sw_frozen, adj)
      
      #Pack main and frozen side-by-side in an Hbox
      table_hbox <- RGtk2::gtkHBox()
      RGtk2::gtkBoxPackStart(table_hbox, sw_frozen, F, F) 
      RGtk2::gtkBoxPackStart(table_hbox, sw, T, T)
      
      RGtk2::gtkBoxPackStart(obj_env$table_objects_list$inner_box, table_hbox, T, T)
      
      #Bind cell click events to both views
      RGtk2::gSignalConnect(obj_env$table_objects_list$view, "button-press-event", obj_env$tree_view_column_btn_press, data = obj_env)
      RGtk2::gSignalConnect(obj_env$table_objects_list$view_frozen, "button-press-event", obj_env$tree_view_column_btn_press, data = obj_env)
      
      #Sync main view to frozen view
      RGtk2::gSignalConnect(obj_env$table_objects_list$view, "cursor-changed", function(widget, data) {
        frozen_view <- data
        cursor <- RGtk2::gtkTreeViewGetCursor(widget)
        
        if (!is.null(cursor$path)) {
          frozen_cursor <- RGtk2::gtkTreeViewGetCursor(frozen_view)
          #Only update if the paths are different to avoid an infinite loop
          if (is.null(frozen_cursor$path) || RGtk2::gtkTreePathToString(cursor$path) != RGtk2::gtkTreePathToString(frozen_cursor$path)) {
            RGtk2::gtkTreeViewSetCursor(frozen_view, cursor$path, NULL, FALSE)
          }
        }
        return(FALSE)
      }, data = obj_env$table_objects_list$view_frozen)
      
      #Sync frozen view to main view
      RGtk2::gSignalConnect(obj_env$table_objects_list$view_frozen, "cursor-changed", function(widget, data) {
        main_view <- data
        cursor <- RGtk2::gtkTreeViewGetCursor(widget)
        
        if (!is.null(cursor$path)) {
          main_cursor <- RGtk2::gtkTreeViewGetCursor(main_view)
          #Only update if the paths are different to avoid an infinite loop
          if (is.null(main_cursor$path) || RGtk2::gtkTreePathToString(cursor$path) != RGtk2::gtkTreePathToString(main_cursor$path)) {
            RGtk2::gtkTreeViewSetCursor(main_view, cursor$path, NULL, FALSE)
          }
        }
        return(FALSE)
      }, data = obj_env$table_objects_list$view)
      
      #Shift and scroll for horizontal scrolling
      RGtk2::gSignalConnect(obj_env$table_objects_list$view, "scroll-event", function(widget, event, data) {
        sw_main <- data
        
        #Extract the state integer
        state_int <- as.integer(event[["state"]])
        is_shift <- bitwAnd(state_int, 1) > 0
        
        direction <- event[["direction"]]
        hadj <- RGtk2::gtkScrolledWindowGetHadjustment(sw_main)
        
        #Determine scroll amount
        step <- hadj$stepIncrement * 1
        if (is.null(step) || step == 0) step <- 50
        
        if ((is_shift && direction == RGtk2::GdkScrollDirection["up"]) || direction == RGtk2::GdkScrollDirection["left"]) {
          #Scroll up to move left
          new_val <- max(hadj$lower, hadj$value - step)
          RGtk2::gtkAdjustmentSetValue(hadj, new_val)
          #Return TRUE to tell GTK we handled the event
          return(TRUE)
          
        } else if ((is_shift && direction == RGtk2::GdkScrollDirection["down"]) || direction == RGtk2::GdkScrollDirection["right"]) {
          #Scroll down to move right
          new_val <- min(hadj$upper - hadj$pageSize, hadj$value + step)
          RGtk2::gtkAdjustmentSetValue(hadj, new_val)
          #Return TRUE to tell GTK we handled the event
          return(TRUE) 
        }
        
        #Return FALSE to let GTK handle normal vertical scrolling
        return(FALSE)
      }, data = sw)
    } else {
      obj_env$table_objects_list$raw_df <- df
      df2 <- obj_env$table_obj_function_df2(df)
      df <- cbind(df, df2)
      
      obj_env$table_objects_list$model <- RGtk2::rGtkDataFrame(df)
      
      RGtk2::gtkTreeViewSetModel(obj_env$table_objects_list$view, obj_env$table_objects_list$model)
      RGtk2::gtkTreeViewSetModel(obj_env$table_objects_list$view_frozen, obj_env$table_objects_list$model)
      
      if (is_full_data_table) {
        for (j in setdiff(seq_len(ncol(df) - 3), 1)) {
          data3 <- outer_env[[session_name]]$data3
          my_row <- data3[j - 1, ]
          my_tool_tip <- paste0(
            my_row[, "label"], "\nLength: ", my_row[, "length"],
            "\nClass: ", my_row[, "class"], "\nDistinct: ", my_row[, "distinct"],
            "\nUnique: ", my_row[, "unique"], "\nMissing: ", my_row[, "missing"],
            "\nBlank: ", my_row[, "blank"]
          )
          
          #Reset labels so they accurately reflect subset data
          if (totem$settings_list$columnlabel | totem$settings_list$columnunique) {
            if (totem$settings_list$columnlabel) {
              if (is.na(my_row[, "label"])) { pre_y <- "---" } else { pre_y <- my_row[, "label"] }
              
              col_length <- max(nchar(as.character(head(outer_env[[session_name]]$data2[[j - 1]], 500))))
              if (is.na(col_length)) {col_length <- 0}
              #Insert line breaks to prevent labels from being too long
              max_length <- max(20, col_length)
              words <- strsplit(pre_y, " ")[[1]]
              result <- ""
              current_length <- 0      
              for (word in words) {
                if (current_length + nchar(word) > max_length) {
                  result <- paste0(result, " \n", word)
                  current_length <- nchar(word)
                } else {
                  if (current_length > 0) result <- paste0(result, " ")
                  result <- paste0(result, word)
                  current_length <- current_length + nchar(word) + 1
                }
              }
            }
            if (totem$settings_list$columnlabel & totem$settings_list$columnunique) { RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[j]]$evt$y, paste0(result, " \nU: ", my_row[, "unique"])) } else if (totem$settings_list$columnlabel & !totem$settings_list$columnunique) { RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[j]]$evt$y, paste0(result, " ")) } else if (!totem$settings_list$columnlabel & totem$settings_list$columnunique) { RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[j]]$evt$y, paste0("U: ", my_row[, "unique"])) }
          }
          
          is_dark <- totem$settings_list$dark_mode
          
          #Check if the current column is in the frozen UI list to preserve the tint
          is_frozen <- isTRUE(colnames(df)[j] %in% frozen_cols)
          if (is_frozen) {
            header_bg <- ifelse(is_dark, "#404040", "#E0E0E0")
          } else {
            header_bg <- ifelse(is_dark, "#2D2D2D", "#FFFFFF")
          }
          
          RGtk2::gtkWidgetSetTooltipText(obj_env$table_objects_list$allColumns[[j]]$evt$evb, my_tool_tip)
          RGtk2::gtkWidgetModifyBg(object = obj_env$table_objects_list$allColumns[[j]]$evt$evb, state = "normal", color = header_bg)
        }
      }
    }
    
    #Resize r__ column header to match all others
    max_x_newlines <- 0
    max_y_newlines <- 0
    has_y_text <- FALSE
    
    #Loop through all data columns to find the maximum number of newlines
    for (j in setdiff(seq_len(ncol(df) - 3), 1)) {
      
       #Check x label (primary column name - bold)
       col_text_x <- RGtk2::gtkLabelGetText(obj_env$table_objects_list$allColumns[[j]]$evt$x)
       if (!is.null(col_text_x) && col_text_x != "") {
          max_x_newlines <- max(max_x_newlines, stringr::str_count(col_text_x, "\n"))
       }
      
       #Check y label (secondary metadata - normal weight)
       col_text_y <- RGtk2::gtkLabelGetText(obj_env$table_objects_list$allColumns[[j]]$evt$y)
       if (!is.null(col_text_y) && col_text_y != "") {
          has_y_text <- TRUE
          max_y_newlines <- max(max_y_newlines, stringr::str_count(col_text_y, "\n"))
       }
    }
    
    #Apply the exact newline math to x and y independently
    blank_x <- paste0("r__", paste0(rep("\n", max_x_newlines), collapse = ""))
    RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[1]]$evt$x, blank_x)
    
    if (has_y_text) {
       #Show the secondary label and size it to match the tallest y label
       RGtk2::gtkWidgetShow(obj_env$table_objects_list$allColumns[[1]]$evt$y)
       blank_y <- paste0(" ", paste0(rep("\n", max_y_newlines), collapse = ""))
       RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[1]]$evt$y, blank_y)
    } else {
       #Hide the secondary label completely so it takes up zero vertical space
       RGtk2::gtkWidgetHide(obj_env$table_objects_list$allColumns[[1]]$evt$y)
       RGtk2::gtkLabelSetText(obj_env$table_objects_list$allColumns[[1]]$evt$y, "")
    }

    #Force GTK to respect custom header widths and wipe autosize caches
    RGtk2::gIdleAdd(function(data) {
      enforce_widths <- function(view) {
        cols <- RGtk2::gtkTreeViewGetColumns(view)
        for (col in cols) {
          #Fetch custom widget to enforce minimum header width
          hw <- RGtk2::gtkTreeViewColumnGetWidget(col)
          if (!is.null(hw)) {
            req <- RGtk2::gtkWidgetSizeRequest(hw)$requisition
            #Twelve pixel padding covers GTK native column separators and margins
            RGtk2::gtkTreeViewColumnSetMinWidth(col, req$width + 12)
          }

          #Toggle sizing mode to wipe GTK internal width cache
          RGtk2::gtkTreeViewColumnSetSizing(col, RGtk2::GtkTreeViewColumnSizing["fixed"])
          RGtk2::gtkTreeViewColumnSetSizing(col, RGtk2::GtkTreeViewColumnSizing["autosize"])
        }
        RGtk2::gtkTreeViewColumnsAutosize(view)
        RGtk2::gtkWidgetQueueDraw(view)
      }

      enforce_widths(data$view)
      enforce_widths(data$frozen)

      return(FALSE)
    }, data = list(
      view = obj_env$table_objects_list$view,
      frozen = obj_env$table_objects_list$view_frozen
    ))
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

  freeze_column <- function(col_name) {
    #Read current entry
    freeze_txt <- trimws(RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$freeze_entry))
    if (freeze_txt == "") {
      frozen_cols <- c()
    } else {
      frozen_cols <- trimws(strsplit(freeze_txt, ",")[[1]])
    }
    
    #Toggle column in the vector
    if (col_name %in% frozen_cols) {
      frozen_cols <- setdiff(frozen_cols, col_name)
    } else {
      frozen_cols <- c(frozen_cols, col_name)
    }
    
    #Force the checkbox ON if they are actively using the right-click command
    RGtk2::gtkToggleButtonSetActive(outer_env[[session_name]]$data_view_list$freeze_cb, TRUE)
    
    #Push text to the UI field
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$freeze_entry, paste0(frozen_cols, collapse = ", "))
    
    #Redraw handles the split-pane rebuild natively via identical() check
    obj_env$df_obj$draw_table()
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
    clear_filters = clear_filters, clear_arrange = clear_arrange, clear_select = clear_select, add_select = add_select, get_current_row = get_current_row, hide = hide, show = show, freeze_column = freeze_column
  ))
}
