
#' e__add_before_filter_full_data_bucket
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param exclude TODO
#' @param outer_env TODO
#'
#' @return TODO

e__add_before_filter_full_data_bucket <- function(session_name, current_row, exclude = F, outer_env = totem) {
  temp_df <- outer_env[[session_name]]$data2
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

    # Determine how to format the right side of %in% based on column class
    if (is.character(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% c(", temp_string, ")")
    } 
    else if (is.numeric(temp_df[[x]])) {
      my_title[[i]] <- paste0("round(", clean_x, ", 5) %in% round(c(", temp_string, "), 5)")
    }
    else if (is.logical(temp_df[[x]])) {
      clean_temp <- gsub('"', '', temp_string)
      my_title[[i]] <- paste0(clean_x, " %in% c(", clean_temp, ")")
    }
    else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(", temp_string, "))")
    }
    else if (inherits(temp_df[[x]], "difftime")) {
      clean_temp <- gsub('"', '', temp_string)
      clean_temp <- gsub("[a-z ]", "", clean_temp) # Strip letters and spaces
      my_title[[i]] <- paste0("as.numeric(", clean_x, ") %in% c(", clean_temp, ")")
    }
    else if (sum(class(temp_df[[x]]) %in% c("hms", "POSIXct", "POSIXt")) > 0) {
      my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(", temp_string, ")")
    } else {
      my_title[[i]] <- paste0(clean_x, " %in% c(", temp_string, ")")
    }

    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])
    my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])

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
    #Numeric - no quotes around values, wrapped in rounding for float precision and trimws for spacing
    #Check if values extend to 5+ decimal places to avoid unnecessary round() clutter
    else if (is.numeric(temp_df[[x]])) {
      vals <- as.numeric(sort(unique(filtered_data[, x, drop = T]), na.last = T))
      
      if (any(vals != round(vals, 4), na.rm = TRUE)) {
        my_title[[i]] <- paste0("round(", clean_x, ", 5) %in% round(c(", paste0(trimws(vals), collapse = ", "), "), 5)")
      } else {
        my_title[[i]] <- paste0(clean_x, " %in% c(", paste0(trimws(vals), collapse = ", "), ")")
      }
    }
    #Logical - no quotes around values
    else if (is.logical(temp_df[[x]])) {
      vals <- as.logical(sort(unique(filtered_data[, x, drop = T]), na.last = T))
      my_title[[i]] <- paste0(clean_x, " %in% c(", paste0(vals, collapse = ", "), ")")
    }
    #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
    else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", paste0(as.character(sort(unique(filtered_data[, x, drop = T]), na.last = T)), collapse = "\", \""), "\"))")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }  
    #Difftime - strip string artifacts introduced by matrix coercion
    else if (inherits(temp_df[[x]], "difftime")) {
      raw_vals <- sort(unique(filtered_data[, x, drop = T]), na.last = T)
      clean_vals <- gsub("[^0-9.-]", "", raw_vals)
      clean_vals[clean_vals == ""] <- NA
      vals <- as.numeric(clean_vals)
      my_title[[i]] <- paste0("as.numeric(", clean_x, ") %in% c(", paste0(trimws(vals), collapse = ", "), ")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }
    #POSIXct/POSIXt (numeric datetime columns) and hms (time columns) - wrap column in "as.character" and wrap values in quotes
    else if (sum(class(temp_df[[x]]) %in% c("hms", "POSIXct", "POSIXt")) > 0) {
      my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", paste0(as.character(sort(unique(filtered_data[, x, drop = T]), na.last = T)), collapse = "\", \""), "\")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }

    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])   
    
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
    #Numeric - no quotes around values, wrapped in rounding for float precision and trimws for spacing
    #Check if values extend to 5+ decimal places to avoid unnecessary round() clutter
    else if (is.numeric(temp_df[[x]])) {
      vals <- as.numeric(current_row$row[, x, drop = T])
      
      if (any(vals != round(vals, 4), na.rm = TRUE)) {
        my_title[[i]] <- paste0("round(", clean_x, ", 5) %in% round(c(", trimws(vals), "), 5)")
      } else {
        my_title[[i]] <- paste0(clean_x, " %in% c(", trimws(vals), ")")
      }
    }
    #Logical - no quotes around values
    else if (is.logical(temp_df[[x]])) {
      vals <- as.logical(current_row$row[, x, drop = T])
      my_title[[i]] <- paste0(clean_x, " %in% c(", paste0(vals, collapse = ", "), ")")
    }
    #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
    else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", as.character(current_row$row[, x, drop = T]), "\"))")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }  
    #Difftime - strip string artifacts introduced by matrix coercion
    else if (inherits(temp_df[[x]], "difftime")) {
      raw_vals <- current_row$row[, x, drop = T]
      clean_vals <- gsub("[^0-9.-]", "", raw_vals)
      clean_vals[clean_vals == ""] <- NA
      vals <- as.numeric(clean_vals)
      my_title[[i]] <- paste0("as.numeric(", clean_x, ") %in% c(", paste0(trimws(vals), collapse = ", "), ")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }
    #POSIXct/POSIXt (numeric datetime columns) and hms (time columns) - wrap column in "as.character" and wrap values in quotes
    else if (sum(class(temp_df[[x]]) %in% c("hms", "POSIXct", "POSIXt")) > 0) {
      my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", as.character(current_row$row[, x, drop = T]), "\")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }

    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]]) 

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
    #Numeric - no quotes around values, wrapped in rounding for float precision and trimws for spacing
    #Check if values extend to 5+ decimal places to avoid unnecessary round() clutter
    else if (is.numeric(temp_df[[x]])) {
      vals <- as.numeric(current_row$row[, x, drop = T])
      
      if (any(vals != round(vals, 4), na.rm = TRUE)) {
        my_title[[i]] <- paste0("round(", clean_x, ", 5) %in% round(c(", trimws(vals), "), 5)")
      } else {
        my_title[[i]] <- paste0(clean_x, " %in% c(", trimws(vals), ")")
      }
    }
    #Logical - no quotes around values
    else if (is.logical(temp_df[[x]])) {
      vals <- as.logical(current_row$row[, x, drop = T])
      my_title[[i]] <- paste0(clean_x, " %in% c(", paste0(vals, collapse = ", "), ")")
    }
    #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
    else if (lubridate::is.Date(temp_df[[x]])) {
      my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", as.character(current_row$row[, x, drop = T]), "\"))")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    } 
    #Difftime - strip string artifacts introduced by matrix coercion
    else if (inherits(temp_df[[x]], "difftime")) {
      raw_vals <- current_row$row[, x, drop = T]
      clean_vals <- gsub("[^0-9.-]", "", raw_vals)
      clean_vals[clean_vals == ""] <- NA
      vals <- as.numeric(clean_vals)
      my_title[[i]] <- paste0("as.numeric(", clean_x, ") %in% c(", paste0(trimws(vals), collapse = ", "), ")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }
    #POSIXct/POSIXt (numeric datetime columns) and hms (time columns) - wrap column in "as.character" and wrap values in quotes
    else if (sum(class(temp_df[[x]]) %in% c("hms", "POSIXct", "POSIXt")) > 0) {
      my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", as.character(current_row$row[, x, drop = T]), "\")")
      #Remove quotes from around NA
      my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
    }

    #Clean filter
    my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])
    
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
  data2 <- outer_env[[session_name]]$data2
  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines", "nchar"))
  temp_df <- unique(data2[, cross_tab_names])

  i <- 1
  j <- 1
  table_title <- rep(NA, nrow(temp_df))
  my_title <- rep(NA, length(cross_tab_names))
  
  while (j <= nrow(temp_df)) {
    meta_row <- temp_df[j, ]
    
    for (x in cross_tab_names) {
      #Sandwich column name with backticks if it has special characters
      if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
        clean_x <- paste0("`", x, "`") 
      } else {
        clean_x <- x
      }
      #Character - put quotes around values
      if (is.character(temp_df[[x]])) {
        my_title[[i]] <- paste0(clean_x, " %in% c(\"", meta_row[, x, drop = T], "\")")
      } 
      #Numeric - no quotes around values, wrapped in rounding for float precision and trimws for spacing
      #Check if values extend to 5+ decimal places to avoid unnecessary round() clutter
      else if (is.numeric(temp_df[[x]])) {
        vals <- as.numeric(meta_row[, x, drop = T])
        
        if (any(vals != round(vals, 4), na.rm = TRUE)) {
          my_title[[i]] <- paste0("round(", clean_x, ", 5) %in% round(c(", trimws(vals), "), 5)")
        } else {
          my_title[[i]] <- paste0(clean_x, " %in% c(", trimws(vals), ")")
        }
      }
      #Logical - no quotes around values
      else if (is.logical(temp_df[[x]])) {
        vals <- as.logical(meta_row[, x, drop = T])
        my_title[[i]] <- paste0(clean_x, " %in% c(", paste0(vals, collapse = ", "), ")")
      }
      #Date (numeric date columns without time portion) - wrap values in "as.Date" and quotes
      else if (lubridate::is.Date(temp_df[[x]])) {
        my_title[[i]] <- paste0(clean_x, " %in% as.Date(c(\"", as.character(meta_row[, x, drop = T]), "\"))")
        #Remove quotes from around NA
        my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
      } 
      #Difftime - strip string artifacts introduced by matrix coercion
      else if (inherits(temp_df[[x]], "difftime")) {
        raw_vals <- meta_row[, x, drop = T]
        clean_vals <- gsub("[^0-9.-]", "", raw_vals)
        clean_vals[clean_vals == ""] <- NA
        vals <- as.numeric(clean_vals)
        my_title[[i]] <- paste0("as.numeric(", clean_x, ") %in% c(", paste0(trimws(vals), collapse = ", "), ")")
        #Remove quotes from around NA
        my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
      }
      #POSIXct/POSIXt (numeric datetime columns) and hms (time columns) - wrap column in "as.character" and wrap values in quotes
      else if (sum(class(temp_df[[x]]) %in% c("hms", "POSIXct", "POSIXt")) > 0) {
        my_title[[i]] <- paste0("as.character(", clean_x, ") %in% c(\"", as.character(meta_row[, x, drop = T]), "\")")
        #Remove quotes from around NA
        my_title[[i]] <- gsub('"NA"', 'NA', my_title[[i]])
      }
      
      #Clean filter
      my_title[[i]] <- gsub("\\\\", "\\\\\\\\", my_title[[i]])
      
      i <- i + 1
    }
    #Combine filter for row
    table_title[[j]] <- paste0(my_title, collapse = " & ")
    my_title <- rep(NA, length(cross_tab_names))
    
    i <- 1
    j <- j + 1
  }

  cmd <- paste0("df <- df %>% filter(", paste0(table_title, collapse = " | "), ")")
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
  source_file <- RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$file_source_cb)
  
  if (source_file == F) {
    raw_code <- u__text_area_get_text(outer_env[[session_name]]$text_area_1)
    code_lines <- strsplit(raw_code, "\n")[[1]]

    # Find the last active line containing an add_cross_counts call
    active_idx <- -1
    if (length(code_lines) > 0) {
      for (i in length(code_lines):1) {
        clean_line <- trimws(gsub("#.*", "", code_lines[i]))
        if (grepl("add_cross_counts\\s*\\([^,]*,\\s*c\\(", clean_line)) {
          active_idx <- i
          break
        }
      }
    }

    # Regex to capture the prefix, the values inside c(), and the suffix
    rgx <- "^(.*add_cross_counts\\s*\\([^,]*,\\s*c\\()((?:\"[^\"]*\"|'[^']*'|[^)])*)(\\).*)$"

    if (active_idx > 0 && grepl(rgx, code_lines[active_idx], perl = TRUE)) {
      last_line <- code_lines[active_idx]
      last_pfx <- sub(rgx, "\\1", last_line)
      last_val <- sub(rgx, "\\2", last_line)
      last_sfx <- sub(rgx, "\\3", last_line)

      # Build the new items to insert, ignoring exact duplicates
      cols_to_add <- c()
      for (col in cross_tab_names) {
        if (!grepl(paste0("['\"]", col, "['\"]"), last_val)) {
          cols_to_add <- c(cols_to_add, paste0("'", col, "'"))
        }
      }

      if (length(cols_to_add) > 0) {
        combined_val <- last_val
        
        # Add a comma separator if the existing vector isn't empty
        if (trimws(combined_val) != "" && !grepl(",\\s*$", combined_val)) {
          combined_val <- paste0(combined_val, ", ")
        }
        combined_val <- paste0(combined_val, paste0(cols_to_add, collapse = ", "))
        
        code_lines[active_idx] <- paste0(last_pfx, combined_val, last_sfx)
        
        # Overwrite the text area with the updated, combined block
        u__text_area_set_text(outer_env[[session_name]]$text_area_1, paste0(code_lines, collapse = "\n"))
        
        # Fetch the newly injected text and log the undo state
        buffer <- RGtk2::gtkTextViewGetBuffer(outer_env[[session_name]]$text_area_1$View)
        end_iter <- RGtk2::gtkTextBufferGetEndIter(buffer)
        start_iter <- RGtk2::gtkTextBufferGetStartIter(buffer)
        str <- RGtk2::gtkTextBufferGetText(buffer, start_iter$iter, end_iter$iter, include.hidden.chars = TRUE)
        
        outer_env$u__log_history(session_name, str, "button_click")
      }
      return(invisible())
    }
  }

  # Fallback if no matching line was found (or if Source is checked)
  cmd <- paste0("df$n__1 <- add_cross_counts(df, c(", paste0("'", cross_tab_names, "'", collapse = ", "), "))")
  outer_env$u__append_before_code(session_name, cmd)
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

  #Try to combine appended code with previous line if the filter is the same.
  replaced <- FALSE
  is_exact_duplicate <- FALSE
  
  if (source_file == F) {
    raw_code <- u__text_area_get_text(outer_env[[session_name]]$text_area_1)
    code_lines <- strsplit(raw_code, "\n")[[1]]

    #Find the last active line ignoring blanks and comments.
    active_idx <- -1
    if (length(code_lines) > 0) {
      for (i in length(code_lines):1) {
        clean_line <- trimws(gsub("#.*", "", code_lines[i]))
        if (clean_line != "") {
          active_idx <- i
          break
        }
      }
    }

    #First check if it is a perfect match of the entire command.
    if (active_idx > 0 && trimws(gsub("#.*", "", code_lines[active_idx])) == trimws(cmd)) {
        is_exact_duplicate <- TRUE
        replaced <- TRUE
    } else {
        #Greedy match up to the last opening parenthesis.
        rgx <- "^(.*%in%\\s*c\\()((?:\"[^\"]*\"|'[^']*'|[^)])+)(\\).*)$"

        if (active_idx > 0 && grepl(rgx, code_lines[active_idx], perl = TRUE) && grepl(rgx, cmd, perl = TRUE)) {
          last_line <- code_lines[active_idx]

          last_pfx <- sub(rgx, "\\1", last_line)
          last_val <- sub(rgx, "\\2", last_line)
          last_sfx <- sub(rgx, "\\3", last_line)

          cmd_pfx <- sub(rgx, "\\1", cmd)
          cmd_val <- sub(rgx, "\\2", cmd)
          cmd_sfx <- sub(rgx, "\\3", cmd)

          #If the structures match perfectly combine them.
          if (last_pfx == cmd_pfx && last_sfx == cmd_sfx) {
            #Prevent exact duplicate additions if the user spams the same button.
            if (last_val != cmd_val) {
              #Combine the values.
              combined_val <- paste0(last_val, ", ", cmd_val)
              code_lines[active_idx] <- paste0(last_pfx, combined_val, last_sfx)
            } else {
              is_exact_duplicate <- TRUE
            }
            replaced <- TRUE
          }
        }
    }
  }

  if (replaced) {
    if (is_exact_duplicate) {
        #Show duplicate toast if exact match.
        if (outer_env$settings_list$copy_messages) {
            outer_env$u__show_toast(session_name, "Attempted filter is already present in code area", bg_color = "#FFACAC")
        }
    } else {
        #Overwrite the text area with the updated combined block.
        u__text_area_set_text(outer_env[[session_name]]$text_area_1, paste0(code_lines, collapse = "\n"))
    }
  } else {
    #Fallback to normal appending if it is a new logic block or writing to source file.
    if (source_file == T) {
      outer_env$u__code_r_add_cmd(session_name, cmd)
    } else {
      u__text_area_append_text(outer_env[[session_name]]$text_area_1, cmd)
    }
  }

  #If we did not skip it fetch the newly injected text and log history.
  if (!is_exact_duplicate) {
      buffer <- RGtk2::gtkTextViewGetBuffer(outer_env[[session_name]]$text_area_1$View)
      end_iter <- RGtk2::gtkTextBufferGetEndIter(buffer)
      start_iter <- RGtk2::gtkTextBufferGetStartIter(buffer)
      str <- RGtk2::gtkTextBufferGetText(buffer, start_iter$iter, end_iter$iter, include.hidden.chars = TRUE)
      
      #Send it to the timeline tracker as a unique event.
      outer_env$u__log_history(session_name, str, "button_click")
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
  # 1. Overwrite the text area with the past code
  u__text_area_set_text(outer_env[[session_name]]$text_area_1, cmd)

  # 2. Fetch the newly injected text from the buffer
  buffer <- RGtk2::gtkTextViewGetBuffer(outer_env[[session_name]]$text_area_1$View)
  end_iter <- RGtk2::gtkTextBufferGetEndIter(buffer)
  start_iter <- RGtk2::gtkTextBufferGetStartIter(buffer)
  str <- RGtk2::gtkTextBufferGetText(buffer, start_iter$iter, end_iter$iter, include.hidden.chars = TRUE)
  
  # 3. Log it to the timeline tracker as a unique event so Ctrl+Z works!
  outer_env$u__log_history(session_name, str, "past_code_load")
}
