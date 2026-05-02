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
  # Begin JNEFF code, I do not even want to touch anything above #
  ################################################################
  #Check if group by checkbox is checked before pulling value
  if (RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$group_by_cb)) {
    group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)
  } else {
    group_by_entry <- ""
  }

  ### Handle when there are grouping variables ###
  if (group_by_entry != "") {
    ### Get sum if the selected column is numeric ###
    if (class(temp_df[[current_row$column]]) %in% c("numeric", "integer")) {
      #utils::writeClipboard(str = "Group by, target column is numeric", format = 1)
      Output <- temp_df %>% group_by_(.dots = stringr::str_split(group_by_entry, ", ")[[1]]) %>% summarise(N = sum(!is.na(eval(parse(text = current_row$column)))),
                                                                                                          Mean = mean(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                          SD = sd(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                          Median = quantile(eval(parse(text = current_row$column)), prob = c(0.50), type = 2, na.rm = T, names = F),
                                                                                                          Q1 =     quantile(eval(parse(text = current_row$column)), prob = c(0.25), type = 2, na.rm = T, names = F),
                                                                                                          Q3 =     quantile(eval(parse(text = current_row$column)), prob = c(0.75), type = 2, na.rm = T, names = F),
                                                                                                          Min =    quantile(eval(parse(text = current_row$column)), prob = c(0.00), type = 2, na.rm = T, names = F),
                                                                                                          Max =    quantile(eval(parse(text = current_row$column)), prob = c(1.00), type = 2, na.rm = T, names = F),
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
    ### Otherwise no sum ###
    } else {
      #utils::writeClipboard(str = "Group by, target column is not numeric", format = 1)
      Output <- temp_df %>% group_by_(.dots = stringr::str_split(group_by_entry, ", ")[[1]]) %>% summarise(N = sum(!is.na(eval(parse(text = current_row$column)))),
                                                                                                          Mean = mean(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                          SD = sd(eval(parse(text = current_row$column)), na.rm = T),
                                                                                                          Median = 'N/A',
                                                                                                          Q1 =     quantile(eval(parse(text = current_row$column)), prob = c(0.25), type = 2, na.rm = T, names = F),
                                                                                                          Q3 =     quantile(eval(parse(text = current_row$column)), prob = c(0.75), type = 2, na.rm = T, names = F),
                                                                                                          Min =    quantile(eval(parse(text = current_row$column)), prob = c(0.00), type = 2, na.rm = T, names = F),
                                                                                                          Max =    quantile(eval(parse(text = current_row$column)), prob = c(1.00), type = 2, na.rm = T, names = F))
      Output$MeanSD <- paste0(round(Output$Mean, digits = 4), " (", round(Output$SD, digits = 4), ")")
      Output$Mean <- Output$MeanSD
      Output$Q1Q3 <- paste0("(", Output$Q1, ", ", Output$Q3, ")")
      Output$MinMax <- paste0(Output$Min, ", ", Output$Max)
      tOutput <- t(Output[, !names(Output) %in% c("MeanSD", "SD", "Q1", "Q3", "Min", "Max")])
      
      Label <- vector("character", nrow(tOutput))
      Label[nrow(tOutput)] <- "Min, Max"
      Label[nrow(tOutput) - 1] <- "(Q1, Q3)"
      Label[nrow(tOutput) - 2] <- "Median"
      Label[nrow(tOutput) - 3] <- "Mean (SD)"
      Label[nrow(tOutput) - 4] <- "N"
      n_groups <- stringr::str_count(group_by_entry, ",") + 1
      for (i in 1:n_groups) {
        Label[i] <- stringr::word(group_by_entry, start = i, end = i, sep = ", ")  
      }   
      
      tOutput <- cbind(Label, tOutput)
          
      y <- data.frame(tOutput)
    }
  ### Handle when there are no grouping variables ###
  } else {
    ### Get sum if the selected column is numeric ###
    if (class(temp_df[[current_row$column]]) %in% c("numeric", "integer")) {   
      #utils::writeClipboard(str = "Target column is numeric", format = 1) 
      col <- temp_df[[current_row$column]]
      Label <- c("N", "Mean (SD)", "Median", "(Q1, Q3)", "Min, Max", "Sum")
      quantiles <- quantile(col, prob = c(0.50, 0.25, 0.75, 0.00, 1.00), type = 2, na.rm = T, names = F)
      Value <- as.character(c(sum(!is.na(col)), paste0(round(mean(col, na.rm = T), digits = 4), " (", round(sd(col, na.rm = T), digits = 4), ")"), quantiles[1], paste0("(", quantiles[2], ", ", quantiles[3], ")"), paste0(quantiles[4], ", ", quantiles[5]), sum(col, na.rm = T)))
      
      y <- data.frame(Label, Value)
    ### Otherwise no sum ###
    } else {    
      #utils::writeClipboard(str = "Target column is not numeric", format = 1) 
      col <- temp_df[[current_row$column]]
      Label <- c("N", "Mean (SD)", "Median", "(Q1, Q3)", "Min, Max")
      quantiles <- quantile(col, prob = c(0.50, 0.25, 0.75, 0.00, 1.00), type = 2, na.rm = T, names = F)
      Value <- as.character(c(sum(!is.na(col)), paste0(round(mean(col, na.rm = T), digits = 4), " (", round(sd(col, na.rm = T), digits = 4), ")"), quantiles[1], paste0("(", quantiles[2], ", ", quantiles[3], ")"), paste0(quantiles[4], ", ", quantiles[5])))
      
      y <- data.frame(Label, Value)
    }
  }
    
  ### Create gtk output ###
  outer_env$u__df_view(y,
    paste0(current_row$column, " Summary: ", outer_env[[session_name]]$sas_file_basename, " | ", as.character(Sys.time())),
    height = 300, width = 500
  )
}
