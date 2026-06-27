#' e__move_column
#'
#' @param placement TODO
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__move_column <- function(placement, session_name, current_row, outer_env=totem) {
  require(RGtk2)
  
  #Get selection
  selection <- as.character(current_row$column)
  
  #Get current column order directly from the already-arranged data2
  temp_df <- outer_env[[session_name]]$data2
  current_cols <- colnames(temp_df)
  
  #Define choices for the UI dropdowns
  col_order <- current_cols
  
  ##############################
  # Get user's "before" target #
  ##############################
  if (placement == 0) {
    #Generate dialog to ask about placement
    dialog <- gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "question", 
      buttons = "ok-cancel", 
      paste0("Select a column to move ", toupper(selection), " before"))
    
    #Add options
    choices <- col_order
    combo <- gtkComboBoxNewText()
    combo$show()
    for (choice in choices) {
      combo$appendText(choice)
    }
    combo$setActive(0)
    
    #Make a frame for the options
    frame <- gtkFrame(paste0("Column to move ", toupper(selection), " before"))
    frame$add(combo)
    dialog[["vbox"]]$add(frame)
    #Require response before interacting with table
    response <- dialog$run()  
    #Find selection
    target <- col_order[gtkComboBoxGetActive(combo) + 1]
    #Destroy dialog box
    gtkWidgetDestroy(dialog)
  }
  #############################
  # Get user's "after" target #
  #############################
  if (placement == 1) {
    #Generate dialog to ask about placement
    dialog <- gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "question", 
      buttons = "ok-cancel", 
      paste0("Select a column to move ", toupper(selection), " after"))
    
    #Add options
    choices <- col_order
    combo <- gtkComboBoxNewText()
    combo$show()
    for (choice in choices) {
      combo$appendText(choice)
    }
    combo$setActive(0)
    
    #Make a frame for the options
    frame <- gtkFrame(paste0("Column to move ", toupper(selection), " after"))
    frame$add(combo)
    dialog[["vbox"]]$add(frame)
    #Require response before interacting with table
    response <- dialog$run()  
    #Find selection
    target <- col_order[gtkComboBoxGetActive(combo) + 1]
    #Destroy dialog box
    gtkWidgetDestroy(dialog)
  }
    
  #####################
  # Rearrange columns #
  #####################
  if (response %in% c(GtkResponseType["close"], GtkResponseType["delete-event"], GtkResponseType["cancel"]) == F) {
    
    #Determine the new absolute column order
    old_index <- which(current_cols == selection)
    new_index <- which(current_cols == target) + placement
    if (new_index > old_index) {
      delete_index <- old_index
    } else {
      delete_index <- old_index + 1
    }
    target_cols <- append(current_cols, selection, after = new_index - 1)
    target_cols <- target_cols[-delete_index]
    
    #Parse the existing select string to keep it clean and minimal
    st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
    
    if (st != "") {
      #Split by comma while ignoring commas inside parentheses
      vst <- trimws(unlist(strsplit(st, split = ",(?![^(]*\\))", perl = TRUE)))
      
      #Isolate explicit columns vs tidyselect helpers
      helpers <- vst[grepl("\\(\\)", vst)]
      explicit_cols <- vst[!grepl("\\(\\)", vst)]
      explicit_cols <- gsub("`", "", explicit_cols)
      
      #Ensure the moved columns are tracked explicitly
      explicit_cols <- unique(c(explicit_cols, selection, target))
      
      #Sort explicit columns by their new relative order
      explicit_cols <- target_cols[target_cols %in% explicit_cols]
      
      #Re-apply backticks if needed
      explicit_cols <- sapply(explicit_cols, function(x) {
        if (!grepl("^[a-zA-Z0-9]*$", x)) paste0("`", x, "`") else x
      }, USE.NAMES = FALSE)
      
      #Combine explicit columns and helpers
      newst <- paste(c(explicit_cols, helpers), collapse = ", ")
    } else {
     # If the string was empty, explicitly list all columns up to the moved items 
      # to lock in their absolute structural position, then add everything()
      max_idx <- max(which(target_cols == selection), which(target_cols == target))
      explicit_cols <- target_cols[1:max_idx]
      
      explicit_cols <- sapply(explicit_cols, function(x) {
        if (!grepl("^[a-zA-Z0-9]*$", x)) paste0("`", x, "`") else x
      }, USE.NAMES = FALSE)
      newst <- paste(c(explicit_cols, "everything()"), collapse = ", ")
    }

    # Force the select checkbox to be active so the move actually applies
    RGtk2::gtkToggleButtonSetActive(outer_env[[session_name]]$data_view_list$select_cb, TRUE)
    
    #Replace select field with new column order and run code
    RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, newst)
    
    outer_env$show_load_window()
    outer_env$u__load_dataset_filter(session_name)
    outer_env$hide_load_window()
  }
}
