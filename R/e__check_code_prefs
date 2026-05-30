#' e__check_code_prefs
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return Boolean indicating if user proceeded (TRUE) or cancelled (FALSE)

e__check_code_prefs <- function(session_name, outer_env = totem) {
  require(RGtk2)
  
  if (is.null(outer_env$settings_list$code_case) || outer_env$settings_list$code_case == "Prompt" ||
      is.null(outer_env$settings_list$code_spacing) || outer_env$settings_list$code_spacing == "Prompt") {
    
    dialog <- gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "question", 
      buttons = "ok-cancel", 
      "Select preferences for generated code (these can be changed later in Settings):"
    )
    
    # Add Case Options
    choices_case <- c("Lowercase", "Uppercase")
    radio_buttons_case <- NULL
    vbox_case <- gtkVBox(F, 0)
    for (choice in choices_case) {
      button <- gtkRadioButton(radio_buttons_case, choice)
      vbox_case$add(button)
      radio_buttons_case <- c(radio_buttons_case, button)
    }
    frame_case <- gtkFrame("Letter case")
    frame_case$add(vbox_case)
    dialog[["vbox"]]$add(frame_case)
    
    # Add Spacing Options
    choices_space <- c("Spaced (x = y)", "Compact (x=y)")
    radio_buttons_space <- NULL
    vbox_space <- gtkVBox(F, 0)
    for (choice in choices_space) {
      button <- gtkRadioButton(radio_buttons_space, choice)
      vbox_space$add(button)
      radio_buttons_space <- c(radio_buttons_space, button)
    }
    frame_space <- gtkFrame("Operator Spacing")
    frame_space$add(vbox_space)
    dialog[["vbox"]]$add(frame_space)
    
    response <- dialog$run()
    
    # Escape out if user cancels
    if (response %in% c(GtkResponseType["close"], GtkResponseType["delete-event"], GtkResponseType["cancel"])) {
      gtkWidgetDestroy(dialog)
      return(FALSE)
    }
    
    # Save selections to Global Settings
    for (i in 1:length(radio_buttons_case)) {
      if (gtkToggleButtonGetActive(radio_buttons_case[[i]])) outer_env$settings_list$code_case <- choices_case[i]
    }
    for (i in 1:length(radio_buttons_space)) {
      if (gtkToggleButtonGetActive(radio_buttons_space[[i]])) outer_env$settings_list$code_spacing <- choices_space[i]
    }
    
    gtkWidgetDestroy(dialog)
    save_settings(outer_env)
  }
  
  return(TRUE)
}
