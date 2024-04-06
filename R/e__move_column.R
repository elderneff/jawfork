#' e__move_column
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__move_column <- function(placement, session_name, current_row, outer_env=totem) {
  require(RGtk2)
  
  #Get selection
  selection <- as.character(current_row$column)
  #Get column order of dataset
  st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
  temp_df <- outer_env[[session_name]]$data2
  if (st != "") {
    #dataset2 <- select(temp_df[0, ], st)
    dataset2 <- select(temp_df, st)
    col_order <- colnames(dataset2)
  } else {
    #col_order <- colnames(temp_df[0, ])
    col_order <- colnames(temp_df)
  }

  #Handle moving column before target
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
    combo$setActive(o)
    
    #Make a frame for the buttons
    frame <- gtkFrame(paste0("Column to move ", toupper(selection), " before"))
    frame$add(combo)
    dialog[["vbox"]]$add(frame)
    #Require response before interacting with table
    response <- dialog$run()
  
    #Find selection
    for (i in 1:length(combo)) {
      if (gtkToggleButtonGetActive(combo[[i]])) {
        selectn <- i
      }
    }
    target <- choices[selectn]
    
    #Destroy dialog box
    gtkWidgetDestroy(dialog)
    
    if (response %in% c(GtkResponseType["close"], GtkResponseType["delete-event"], GtkResponseType["cancel"]) == F) {
      utils::writeClipboard(str = "hello world", format = 1)
    }
  }
}