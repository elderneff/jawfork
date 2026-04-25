#' e__add_column_label
#'
#' @param treeviewcolumn TODO
#' @param label TODO
#' @param j TODO
#' @param var_class TODO
#' @param tool_tip TODO
#' @param sec_label TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__add_column_label <- function(treeviewcolumn, label, j, var_class = NULL, tool_tip = NULL, sec_label,outer_env=totem,obj_env=inner_env) {
  color_header_1 <- "#9bb5f5"
  font.str <- "Serif, bold 9"
  font <- RGtk2::pangoFontDescriptionFromString(font.str)


  evb <- RGtk2::gtkEventBox()
  hb <- RGtk2::gtkVBox()
  RGtk2::gtkContainerAdd(evb, hb)
  #Initialize y
  y <- RGtk2::gtkLabel("")


  if (is.null(var_class) == F) {
    ############################
    # Get column name and type #
    ############################
    if (var_class == "numeric") {
      x <- RGtk2::gtkLabel(paste0(label, " [n] "))
    } else if (toupper(var_class) == toupper("Date")) {
      x <- RGtk2::gtkLabel(paste0(label, " [d] "))
    } else if (toupper(var_class) == toupper("character")) {
      x <- RGtk2::gtkLabel(paste0(label, " [c] "))
    } else if (toupper(var_class) == toupper(paste0(class(as.POSIXct(Sys.time())), collapse = "/"))) {
      x <- RGtk2::gtkLabel(paste0(label, " [t] "))
    } else if (toupper(var_class) == toupper(paste0(class(as.POSIXlt(Sys.time())), collapse = "/"))) {
      x <- RGtk2::gtkLabel(paste0(label, " [t] "))
    } else {
      x <- RGtk2::gtkLabel(paste0(label, " "))
    }
    x$xalign <- 0
    RGtk2::gtkWidgetModifyFont(x, font)
    RGtk2::gtkBoxPackStart(hb, x, F, T, padding = 1)
    #####################
    # Get column labels #
    #####################
    if (totem$settings_list$columnlabel | totem$settings_list$columnunique) {
      if (totem$settings_list$columnlabel) {
        data3 <- outer_env[[session_name]]$data3
        my_row <- data3[j - 1, ]
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
      ###########################################
      # Combine column labels and unique values #
      ###########################################
      if (totem$settings_list$columnlabel & totem$settings_list$columnunique) { y <- RGtk2::gtkLabel(paste0(result, " \n", sec_label)) }
      else if (totem$settings_list$columnlabel & !totem$settings_list$columnunique) { y <- RGtk2::gtkLabel(paste0(result, " ")) }
      else if (!totem$settings_list$columnlabel & totem$settings_list$columnunique) { y <- RGtk2::gtkLabel(sec_label) }
      y$xalign <- 0
      RGtk2::gtkBoxPackStart(hb, y, F, F, padding = 1)
    }
  } else {
    y <- RGtk2::gtkLabel("")
    y$xalign <- 0
    x <- RGtk2::gtkLabel(paste0(label, " "))
    x$xalign <- 0
    RGtk2::gtkWidgetModifyFont(x, font)
    RGtk2::gtkBoxPackStart(hb, x, F, F, padding = 1)
    
    # Only pack the extra y label (and its padding) if this is the Full Data Table
    # Only pack the extra y label if unique values and/or column headers are enabled in settings
    if (exists("is_full_data_table") && is_full_data_table && (totem$settings_list$columnlabel | totem$settings_list$columnunique)) {
      RGtk2::gtkBoxPackStart(hb, y, F, F, padding = 1)
    }
  }


  if (is.null(var_class) == F) {
    if (var_class == "numeric") {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    } else {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    }
  } else {
    if ((j %% 2) == 0) {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    } else {
      RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#FFFFFF")
    }
  }

  if (is.null(tool_tip) == F) {
    RGtk2::gtkWidgetSetTooltipText(evb, tool_tip)
  }

  RGtk2::gtkTreeViewColumnSetWidget(treeviewcolumn, widget = evb)

  return(list(evb = evb, y = y))
}
