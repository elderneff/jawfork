#' e__graph_summary
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__graph_summary <- function(session_name, current_row,outer_env=totem) {
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
  if (!(class(temp_df[[current_row$column]]) %in% c("numeric", "integer"))) {
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
  
  #Output graphics in separate window
  options(device = "windows")
  
  #Check if group by checkbox is checked before pulling value
  if (RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$group_by_cb)) {
    group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)
  } else {
    group_by_entry <- ""
  }

  ### Handle when there are grouping variables ###
  if (group_by_entry != "") {
    #Separate group_by_entry by asterisks instead of commas
    group_by_entry_asterisks <- gsub(", ", "*", group_by_entry)
    #Concatenate the independent and dependent variables into a string
    title <- sprintf('%s ~ %s', current_row$column, group_by_entry_asterisks)
    eval(parse(text = sprintf('plot <- boxplot(%s ~ %s, temp_df, ylab = "%s", main = "%s")', current_row$column, group_by_entry_asterisks, current_row$column, title)))
    text(1:length(plot$n), min(temp_df[[current_row$column]], na.rm = T) - ((max(temp_df[[current_row$column]], na.rm = T) - min(temp_df[[current_row$column]], na.rm = T)) * 0.02), paste("n=", plot$n))
    grid()
    
  ### Handle when there are no grouping variables ###
  } else {
    eval(parse(text = sprintf('plot <- boxplot(temp_df[[current_row$column]], xlab = "", ylab = "%s", main = "%s")', current_row$column, current_row$column)))
    text(1:length(plot$n), min(temp_df[[current_row$column]], na.rm = T) - ((max(temp_df[[current_row$column]], na.rm = T) - min(temp_df[[current_row$column]], na.rm = T)) * 0.02), paste("n=", plot$n))
    grid()
  }
}
