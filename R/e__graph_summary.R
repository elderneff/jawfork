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
  # Begin JNEFF code, I do not even want to touch anything above #
  ################################################################
  #Output graphics in separate window
  options(device = "windows")
  
  group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)

  ### Handle when there are grouping variables ###
  if (group_by_entry != "") {
    ### Get sum if the selected column is numeric ###
    if (class(temp_df[[current_row$column]]) %in% c("numeric", "integer")) {
      #utils::writeClipboard(str = "Group by, target column is numeric", format = 1)
      utils::writeClipboard(str = group_by_entry, format = 1)
      Output <- temp_df %>% group_by_(.dots = stringr::str_split(group_by_entry, ", ")[[1]]) %>% summarise(N = sum(!is.na(eval(parse(text = current_row$column)))),
                                                                                                          Mean = mean(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                          SD = sd(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                          Median = quantile(eval(parse(text = current_row$column)), prob = c(0.50), type = 2, na.rm = T, names = F),
                                                                                                          Q1 =     quantile(eval(parse(text = current_row$column)), prob = c(0.25), type = 2, na.rm = T, names = F),
                                                                                                          Q3 =     quantile(eval(parse(text = current_row$column)), prob = c(0.75), type = 2, na.rm = T, names = F),
                                                                                                          Min =    quantile(eval(parse(text = current_row$column)), prob = c(0.00), type = 2, na.rm = T, names = F),
                                                                                                          Max =    quantile(eval(parse(text = current_row$column)), prob = c(1.00), type = 2, na.rm = T, names = F),
                                                                                                          preSum = sum(eval(parse(text = current_row$column)), na.rm = T))
    ### Otherwise no boxplot ###
    } else {   
      utils::writeClipboard(str = "Target column is not numeric", format = 1)

      stop('Cannot produce boxplot of character values')
    }
  ### Handle when there are no grouping variables ###
  } else {
    ### Make boxplot if the selected column is numeric ###
    if (class(temp_df[[current_row$column]]) %in% c("numeric", "integer")) {   
      utils::writeClipboard(str = "Target column is numeric", format = 1) 

      boxplot(temp_df[[current_row$column]])
    ### Otherwise no boxplot ###
    } else {    
      utils::writeClipboard(str = "Target column is not numeric", format = 1)

      stop('Cannot produce boxplot of character values')
    }
  }
}