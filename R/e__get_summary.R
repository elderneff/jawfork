#' e__get_summary
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__get_summary <- function(session_name, current_row,outer_env=totem) {
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
      paste0("Cannot produce summary: '", current_row$column, "' is not a numeric variable.")
    )
    err_dialog$run()
    RGtk2::gtkWidgetDestroy(err_dialog)
    return()
  }

  #Check if group by checkbox is checked before pulling value
  if (RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$group_by_cb)) {
    group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)
  } else {
    group_by_entry <- ""
  }

  ### Handle when there are grouping variables ###
  if (group_by_entry != "") {
    Output <- temp_df %>% group_by_(.dots = stringr::str_split(group_by_entry, ", ")[[1]]) %>% summarise(N = sum(!is.na(eval(parse(text = current_row$column)))),
                                                                                                  Mean = mean(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                  SD = sd(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                  Median = quantile(eval(parse(text = current_row$column)), prob = c(0.50), type = 2, na.rm = T, names = F),
                                                                                                  Q1 = quantile(eval(parse(text = current_row$column)), prob = c(0.25), type = 2, na.rm = T, names = F),
                                                                                                  Q3 = quantile(eval(parse(text = current_row$column)), prob = c(0.75), type = 2, na.rm = T, names = F),
                                                                                                  Min = quantile(eval(parse(text = current_row$column)), prob = c(0.00), type = 2, na.rm = T, names = F),
                                                                                                  Max = quantile(eval(parse(text = current_row$column)), prob = c(1.00), type = 2, na.rm = T, names = F),
                                                                                                  preSum = sum(eval(parse(text = current_row$column)), na.rm = T))
    Output$MeanSD <- paste0(round(Output$Mean, digits = 4), " (", round(Output$SD, digits = 4), ")")
    Output$Mean <- Output$MeanSD
    Output$Q1Q3 <- paste0("(", Output$Q1, ", ", Output$Q3, ")")
    Output$MinMax <- paste0(Output$Min, ", ", Output$Max)
    Output$Sum <- Output$preSum
    tOutput <- t(Output[, !names(Output) %in% c("MeanSD", "SD", "Q1", "Q3", "Min", "Max", "preSum")])
    
    Label <- vector("character", nrow(tOutput))
    Label[nrow(tOutput)] <- "Sum"
    Label[nrow(tOutput) - 1] <- "Min, Max"
    Label[nrow(tOutput) - 2] <- "(Q1, Q3)"
    Label[nrow(tOutput) - 3] <- "Median"
    Label[nrow(tOutput) - 4] <- "Mean (SD)"
    Label[nrow(tOutput) - 5] <- "N"
    n_groups <- stringr::str_count(group_by_entry, ",") + 1
    for (i in 1:n_groups) {
      Label[i] <- stringr::word(group_by_entry, start = i, end = i, sep = ", ")  
    }   
    
    tOutput <- cbind(Label, tOutput)
    y <- data.frame(tOutput)
    
  ### Handle when there are no grouping variables ###
  } else {
    col <- temp_df[[current_row$column]]
    Label <- c("N", "Mean (SD)", "Median", "(Q1, Q3)", "Min, Max", "Sum")
    quantiles <- quantile(col, prob = c(0.50, 0.25, 0.75, 0.00, 1.00), type = 2, na.rm = T, names = F)
    Value <- as.character(c(sum(!is.na(col)), paste0(round(mean(col, na.rm = T), digits = 4), " (", round(sd(col, na.rm = T), digits = 4), ")"), quantiles[1], paste0("(", quantiles[2], ", ", quantiles[3], ")"), paste0(quantiles[4], ", ", quantiles[5]), sum(col, na.rm = T)))
    
    y <- data.frame(Label, Value)
  }
    
  ### Create gtk output ###
  outer_env$u__df_view(y,
    paste0(current_row$column, " Summary: ", outer_env[[session_name]]$sas_file_basename, " | ", as.character(Sys.time())),
    height = 300, width = 500
  )
}
