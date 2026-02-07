
#' e__add_before_filter_full_data_bucket
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_full_data_bucket <- function(session_name, current_row, exclude = F, outer_env = totem) {
  cross_tab_names <- current_row$column

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    temp_string <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$status_bar$box_bucket_entry)
    #Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
      clean_x <- paste0("`", x, "`") 
    } else {
      clean_x <- x
    }
    my_title[[i]] <- paste0(clean_x, " %in% c(", temp_string, ")")
    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])
    my_title[[i]] <- gsub(" +", " ", my_title[[i]])

    i <- i + 1
  }

  if (exclude) {
    cmd <- paste0("df <- df %>% filter((", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- df %>% filter(", paste0(my_title, collapse = " & "), ")")
  }

  outer_env$u__append_before_code(session_name, gsub('"NA"', 'NA', cmd))
}


#' e__add_before_filter_full_data_column
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_full_data_column <- function(session_name, current_row, df_obj, exclude = F, outer_env = totem) {
  filtered_data <- df_obj$current_data()
  temp_df <- outer_env[[session_name]]$data2

  cross_tab_names <- current_row$column

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    #Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
      clean_x <- paste0("`", x, "`") 
    } else {
      clean_x <- x
    }
    #Character - put quotes around values
    if (is.character(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% c(\"", paste0(sort(unique(filtered_data[, x, drop = T]), na.last = T), collapse = "\", \""), "\")")
    } 
    #Numeric - no quotes around values
    else if (is.numeric(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% c(", paste0(sort(unique(filtered_data[, x, drop = T]), na.last = T), collapse = ", "), ")")
    } 
    #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
    else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", paste0(as.character(sort(unique(filtered_data[, x, drop = T]), na.last = T)), collapse = "\", \""), "\"))")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }  
    #POSIXct/POSIXt (numeric datetime columns) and hms/difftime (time columns) - wrap column in "as.character" and wrap values in quotes
    else if (sum(class(temp_df[[x]]) %in% c("hms", "difftime", "POSIXct", "POSIXt")) > 0) {
      my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", paste0(as.character(sort(unique(filtered_data[, x, drop = T]), na.last = T)), collapse = "\", \""), "\")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }

    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])   
    my_title[[i]] <- gsub(" +", " ", my_title[[i]]) 
    
    i <- i + 1
  }

  if (exclude) {
    cmd <- paste0("df <- df %>% filter((", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- df %>% filter(", paste0(my_title, collapse = " & "), ")")
  }

  outer_env$u__append_before_code(session_name, cmd)
}



#' e__add_before_filter_full_data
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_full_data <- function(session_name, current_row, exclude = F, outer_env = totem) {
  temp_df <- outer_env[[session_name]]$data2

  cross_tab_names <- current_row$column

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    #Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
      clean_x <- paste0("`", x, "`") 
    } else {
      clean_x <- x
    }
    #Character - put quotes around values
    if (is.character(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% c(\"", current_row$row[, x, drop = T], "\")")
    } 
    #Numeric - no quotes around values
    else if (is.numeric(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% c(", current_row$row[, x, drop = T], ")")
    } 
    #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
    else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", as.character(current_row$row[, x, drop = T]), "\"))")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }  
    #POSIXct/POSIXt (numeric datetime columns) and hms/difftime (time columns) - wrap column in "as.character" and wrap values in quotes
    else if (sum(class(temp_df[[x]]) %in% c("hms", "difftime", "POSIXct", "POSIXt")) > 0) {
      my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", as.character(current_row$row[, x, drop = T]), "\")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }

    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]]) 
    my_title[[i]] <- gsub(" +", " ", my_title[[i]])

    i <- i + 1
  }

  if (exclude) {
    cmd <- paste0("df <- df %>% filter((", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- df %>% filter(", paste0(my_title, collapse = " & "), ")")
  }

  outer_env$u__append_before_code(session_name, cmd)
}

#' e__add_before_filter
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter <- function(session_name, current_row, exclude = F, outer_env = totem) {
  temp_df <- outer_env[[session_name]]$data2

  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines", "nchar"))

  my_title <- rep(NA, length(cross_tab_names))
  i <- 1
  for (x in cross_tab_names) {
    #Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
      clean_x <- paste0("`", x, "`") 
    } else {
      clean_x <- x
    }
    #Character - put quotes around values
    if (is.character(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% c(\"", current_row$row[, x, drop = T], "\")")
    } 
    #Numeric - no quotes around values
    else if (is.numeric(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% c(", current_row$row[, x, drop = T], ")")
    }  
    #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
    else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", as.character(current_row$row[, x, drop = T]), "\"))")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    } 
    #POSIXct/POSIXt (numeric datetime columns) and hms/difftime (time columns) - wrap column in "as.character" and wrap values in quotes
    else if (sum(class(temp_df[[x]]) %in% c("hms", "difftime", "POSIXct", "POSIXt")) > 0) {
      my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", as.character(current_row$row[, x, drop = T]), "\")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }

    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])    
    my_title[[i]] <- gsub(" +", " ", my_title[[i]])
    
    i <- i + 1
  }


  if (exclude) {
    cmd <- paste0("df <- df %>% filter((", paste0(my_title, collapse = " & "), ")==F)")
  } else {
    cmd <- paste0("df <- df %>% filter(", paste0(my_title, collapse = " & "), ")")
  }

  outer_env$u__append_before_code(session_name, cmd)
}

#' e__add_before_filter_table
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_table <- function(session_name, current_row, exclude = F, outer_env = totem) {
  temp_df <- outer_env[[session_name]]$data2

  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines", "nchar"))

  i <- 1
  j <- 1
  table_title <- rep(NA, nrow(temp_df))
  my_title <- rep(NA, length(cross_tab_names))
  
  while (j <= nrow(temp_df)) {
    current_row$row <- table[j, ]
    
    for (x in cross_tab_names) {
      #Sandwich column name with backticks if it has special characters
      if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
        clean_x <- paste0("`", x, "`") 
      } else {
        clean_x <- x
      }
      #Character - put quotes around values
      if (is.character(temp_df[[x]])) {
        my_title[[i]] <- paste0(clean_x, " %in% c(\"", current_row$row[, x, drop = T], "\")")
      } 
      #Numeric - no quotes around values
      else if (is.numeric(temp_df[[x]])) {
        my_title[[i]] <- paste0(clean_x, " %in% c(", current_row$row[, x, drop = T], ")")
      }  
      #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
      else if (lubridate::is.Date(temp_df[[x]])) {
        my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", as.character(current_row$row[, x, drop = T]), "\"))")
        #Remove quotes from around NA
        my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
      } 
      #POSIXct/POSIXt (numeric datetime columns) and hms/difftime (time columns) - wrap column in "as.character" and wrap values in quotes
      else if (sum(class(temp_df[[x]]) %in% c("hms", "difftime", "POSIXct", "POSIXt")) > 0) {
        my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", as.character(current_row$row[, x, drop = T]), "\")")
        #Remove quotes from around NA
        my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
      }
      
      #Clean filter
      my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])    
      my_title[[i]] <- gsub(" +", " ", my_title[[i]])
      
      i <- i + 1
    }
    #Combine filter for row
    table_title[[j]] <- paste0(my_title, collapse = " & ")
    cmd <- rep(NA, length(cross_tab_names))
    
    i <- 1
    j <- j + 1
  }

  outer_env$u__append_before_code(session_name, cmd)
}



#' e__add_count_to_df_summary
#'
#' @param session_name TODO
#' @param cross_tab_names TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_count_to_df_summary <- function(session_name, cross_tab_names, outer_env = totem) {
  cmd <- paste0("df$n__1 <- add_cross_counts(df, c(\"", paste0(cross_tab_names, collapse = "\", \""), "\"))")
  outer_env$u__append_before_code(session_name, gsub('"NA"', 'NA', cmd))
}

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

  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines", "nchar"))

  my_filter <- rep(T, nrow(temp_df))
  my_title <- ""

  for (x in cross_tab_names) {
    my_title <- paste0(my_title, "| ", x, "==", current_row$row[, x, drop = T])
    my_filter <- my_filter & (temp_df2[, x, drop = T] %in% current_row$row[, x, drop = T])
  }   

  y <- temp_df[my_filter, , drop = F]

  outer_env$u__df_view(y,
    paste0("Flat: ", outer_env[[session_name]]$sas_file_basename, " (", nrow(y), " x ", ncol(y), ")", my_title, "|", as.character(Sys.time())),
    height = 300, width = 500
  )
}

#' e__append_before_code
#'
#' @param session_name TODO
#' @param cmd TODO
#' @param outer_env TODO
#'
#' @return TODO

e__append_before_code <- function(session_name, cmd, outer_env = totem) {
  source_file <- RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$file_source_cb)
  if (source_file == T) {
    outer_env$u__code_r_add_cmd(session_name, cmd)
  } else {
    u__text_area_append_text(outer_env[[session_name]]$text_area_1, cmd)
  }
}

#' e__set_before_code
#'
#' @param session_name TODO
#' @param cmd TODO
#' @param outer_env TODO
#'
#' @return TODO

e__set_before_code <- function(session_name, cmd, outer_env = totem) {
  u__text_area_set_text(outer_env[[session_name]]$text_area_1, cmd)
}
