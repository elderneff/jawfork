#' e__graph_summary
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__graph_summary <- function(session_name, current_row,outer_env=totem) {
  # Trigger loading screen. on.exit ensures it hides even if the function errors out early.
  outer_env$show_load_window()
  on.exit(try({ outer_env$hide_load_window() }, silent = TRUE), add = TRUE)

  temp_df <- outer_env[[session_name]]$data2
  temp_df$random_char_str <- "a"
  temp_df2 <- as.matrix(temp_df)
  temp_df <- temp_df[, -ncol(temp_df)]
  temp_df2 <- temp_df2[, -ncol(temp_df2)]

  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines"))

  my_filter <- rep(T, nrow(temp_df))
  my_title <- ""

  for (x in cross_tab_names) {
    my_title <- paste0(my_title, "| ", x, "==", current_row$row[, x, drop = T])
    my_filter <- my_filter & (temp_df2[, x, drop = T] %in% current_row$row[, x, drop = T])
  }

  ################################################################
  # Begin JNEFF code
  ################################################################
  
  # ERROR CATCH: Ensure the selected column is numeric
  if (!any(class(temp_df[[current_row$column]]) %in% c("numeric", "integer"))) {
    err_dialog <- RGtk2::gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "error", 
      buttons = "close", 
      paste0("Cannot produce boxplot: '", current_row$column, "' is not a numeric variable.")
    )
    err_dialog$run()
    RGtk2::gtkWidgetDestroy(err_dialog)
    return()
  }

  # Escape target variable for formula evaluation
  safe_y <- paste0("`", current_row$column, "`")
  
  #Output graphics in separate window
  options(device = "windows")
  
  # Check if group by checkbox is checked before pulling value
  if (RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$group_by_cb)) {
    group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)
    
    if (group_by_entry != "") {
      group_vars_check <- trimws(strsplit(group_by_entry, ",")[[1]])
      group_vars_check <- group_vars_check[group_vars_check != ""]
      
      # Identify variables that are in the entry but NOT in the dataset
      missing_vars <- setdiff(group_vars_check, colnames(temp_df))
      
      if (length(missing_vars) > 0) {
        err_dialog <- RGtk2::gtkMessageDialog(
          parent = outer_env[[session_name]]$windows$main_window, 
          flags = "destroy-with-parent", 
          type = "error", 
          buttons = "close", 
          paste0("Cannot proceed. The following Group By variables do not exist in the dataset:\n\n", paste(missing_vars, collapse = ", "))
        )
        err_dialog$run()
        RGtk2::gtkWidgetDestroy(err_dialog)
        return()
      }
    }
    # ----------------------------
  } else {
    group_by_entry <- ""
  }

  ### Handle when there are grouping variables ###
  if (group_by_entry != "") {
    
    # 1. Prepare for an overall group
    group_vars <- stringr::str_split(group_by_entry, ", ")[[1]]
    temp_df_combined <- temp_df
    
    # Convert grouping vars to character to prevent factor level warnings
    for (g in group_vars) {
      if (is.factor(temp_df_combined[[g]])) {
        temp_df_combined[[g]] <- as.character(temp_df_combined[[g]])
      }
    }
    
    # 2. Duplicate data and assign a safe "OVERALL" category
    temp_df_overall <- temp_df_combined
    for (g in group_vars) {
      safe_overall <- "OVERALL"
      while (safe_overall %in% temp_df_combined[[g]]) {
        safe_overall <- paste0(safe_overall, " ")
      }
      temp_df_overall[[g]] <- safe_overall
    }
    temp_df_combined <- rbind(temp_df_combined, temp_df_overall)

    # 3. Escape variables for the formula parser
    safe_group_vars <- paste0("`", group_vars, "`")
    group_by_entry_asterisks <- paste(safe_group_vars, collapse = " * ")
    
    # Unescaped string for plot titles
    title <- sprintf('%s ~ %s', current_row$column, paste(group_vars, collapse = " * "))
    
    # 4. Plot using the combined dataframe
    eval(parse(text = sprintf('plot <- boxplot(%s ~ %s, temp_df_combined, ylab = "%s", main = "%s")', safe_y, group_by_entry_asterisks, current_row$column, title)))
    text(1:length(plot$n), min(temp_df_combined[[current_row$column]], na.rm = T) - ((max(temp_df_combined[[current_row$column]], na.rm = T) - min(temp_df_combined[[current_row$column]], na.rm = T)) * 0.02), paste("n=", plot$n))
    grid()
    
  ### Handle when there are no grouping variables ###
  } else {
    eval(parse(text = sprintf('plot <- boxplot(temp_df[[current_row$column]], xlab = "", ylab = "%s", main = "%s")', current_row$column, current_row$column)))
    text(1:length(plot$n), min(temp_df[[current_row$column]], na.rm = T) - ((max(temp_df[[current_row$column]], na.rm = T) - min(temp_df[[current_row$column]], na.rm = T)) * 0.02), paste("n=", plot$n))
    grid()
  }
}


#' e__scatter_summary
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__scatter_summary <- function(session_name, current_row, outer_env = totem) {
  # Trigger loading screen. on.exit ensures it hides even if the function errors out early.
  outer_env$show_load_window()
  on.exit(try({ outer_env$hide_load_window() }, silent = TRUE), add = TRUE)
  
  temp_df <- outer_env[[session_name]]$data2
  y_col <- current_row$column
  
  # Constraint 1: Target column must be numeric
  if (!any(class(temp_df[[y_col]]) %in% c("numeric", "integer"))) {
    err_dialog <- RGtk2::gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "error", 
      buttons = "close", 
      paste0("Cannot produce scatterplot: Target column '", y_col, "' is not numeric.")
    )
    err_dialog$run()
    RGtk2::gtkWidgetDestroy(err_dialog)
    return()
  }

  if (RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$group_by_cb)) {
    group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)
  } else {
    group_by_entry <- ""
  }

  # Clean the group by input to count unique variables
  group_vars <- trimws(strsplit(group_by_entry, ",")[[1]])
  group_vars <- group_vars[group_vars != ""]

  # Constraint 2: Exactly 1 grouping variable
  if (length(group_vars) != 1) {
    err_dialog <- RGtk2::gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "error", 
      buttons = "close", 
      "Scatterplot requires exactly 1 Group By variable."
    )
    err_dialog$run()
    RGtk2::gtkWidgetDestroy(err_dialog)
    return()
  }

  x_col <- group_vars[1]

  # Constraint 3: Group By column must be numeric
  if (!any(class(temp_df[[x_col]]) %in% c("numeric", "integer"))) {
    err_dialog <- RGtk2::gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "error", 
      buttons = "close", 
      paste0("Cannot produce scatterplot: Group By column '", x_col, "' is not numeric.")
    )
    err_dialog$run()
    RGtk2::gtkWidgetDestroy(err_dialog)
    return()
  }

  # Escape variables for parsing logic
  safe_y <- paste0("`", y_col, "`")
  safe_x <- paste0("`", x_col, "`")

  # Render Plot
  options(device = "windows")
  title <- sprintf('%s vs %s', y_col, x_col)
  
  # Pass escaped variables to the formula, but raw names to the axis titles
  eval(parse(text = sprintf('plot(%s ~ %s, data = temp_df, xlab = "%s", ylab = "%s", main = "%s")', safe_y, safe_x, x_col, y_col, title)))
  grid()
}
