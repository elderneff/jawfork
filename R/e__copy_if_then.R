
#' e__copy_if_then
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_if_then <- function(session_name, current_row, df_obj, outer_env = totem) {
  require(RGtk2)
  
  # 1. Check if we need to prompt the user
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
      return()
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
  
  # 2. Extract preferences
  c_case <- outer_env$settings_list$code_case
  c_space <- outer_env$settings_list$code_spacing
  sp <- ifelse(c_space == "Spaced (x = y)", " = ", "=")
  
  column_classes <- df_obj$get_column_classes()
  column_values <- df_obj$get_column_values(current_row$column)
  sep <- ifelse(column_classes[current_row$column] == "numeric", "", "\"")
  
  string_builder <- rep(NA, length(column_values) + 2)
  
  # 3. Generate Code
  if (toupper(c_case) == "UPPERCASE") {
    string_builder[1] <- paste0("IF MISSING(", current_row$column, ") THEN VAR", sp, "\"\";")
  } else {
    string_builder[1] <- paste0("if missing(", current_row$column, ") then var", sp, "\"\";")
  }
  
  j <- 2
  for (i in column_values) {
    if (toupper(c_case) == "UPPERCASE") {
      string_builder[j] <- paste0("ELSE IF ", current_row$column, sp, sep, i, sep, " THEN VAR", sp, "\"\";")
    } else {
      string_builder[j] <- paste0("else if ", current_row$column, sp, sep, i, sep, " then var", sp, "\"\";")
    }
    j <- j + 1
  }
  
  if (toupper(c_case) == "UPPERCASE") {
    string_builder[j] <- paste0("ELSE DO;\n    ERR_MSG", sp, "CATX(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    PUT ERR_MSG;\nEND;")
  } else {
    string_builder[j] <- paste0("else do;\n    err_msg", sp, "catx(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;")
  }
  
  utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
}

#' e__copy_if_then_do
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_if_then_do <- function(session_name, current_row, df_obj, outer_env = totem) {
  require(RGtk2)
  
  # 1. Check if we need to prompt the user
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
      return()
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
  
  # 2. Extract preferences
  c_case <- outer_env$settings_list$code_case
  c_space <- outer_env$settings_list$code_spacing
  sp <- ifelse(c_space == "Spaced (x = y)", " = ", "=")
  
  column_classes <- df_obj$get_column_classes()
  column_values <- df_obj$get_column_values(current_row$column)
  sep <- ifelse(column_classes[current_row$column] == "numeric", "", "\"")
  
  string_builder <- rep(NA, length(column_values) + 2)
  
  # 3. Generate Code
  if (toupper(c_case) == "UPPERCASE") {
    string_builder[1] <- paste0("IF MISSING(", current_row$column, ") THEN DO;\n    VAR", sp, "\"\";\nEND;")
  } else {
    string_builder[1] <- paste0("if missing(", current_row$column, ") then do;\n    var", sp, "\"\";\nend;")
  }
  j <- 2
  for (i in column_values) {
    if (toupper(c_case) == "UPPERCASE") {
      string_builder[j] <- paste0("ELSE IF ", current_row$column, sp, sep, i, sep, " THEN DO;\n    VAR, sp, "\"\";\nEND;")
    } else {
      string_builder[j] <- paste0("else if ", current_row$column, sp, sep, i, sep, " then do;\n    var", sp, "\"\";\nend;")
    }
    j <- j + 1
  }
  if (toupper(c_case) == "UPPERCASE") {
    string_builder[j] <- paste0("ELSE DO;\n    ERR_MSG", sp, "CATX(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    PUT ERR_MSG;\nEND;")
  } else {
    string_builder[j] <- paste0("else do;\n    err_msg", sp, "catx(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;")
  }
  
  utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
}
