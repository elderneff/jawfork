
#' e__copy_if_then
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_if_then <- function(session_name, current_row, df_obj,outer_env=totem) {
  require(RGtk2)
  
  #Generate dialog to ask about code case
  dialog <- gtkMessageDialog(
    parent = outer_env[[session_name]]$windows$main_window, 
    flags = "destroy-with-parent", 
    type = "question", 
    buttons = "ok-cancel", 
    "Select an option for the copied code")
  
  #Add options
  choices <- c("Lowercase", "Uppercase")
  radio_buttons <- NULL
  vbox <- gtkVBox(F, 0)
  for (choice in choices) {
    button <- gtkRadioButton(radio_buttons, choice)
    vbox$add(button)
    radio_buttons <- c(radio_buttons, button)
  }
  
  #Make a frame for the buttons
  frame <- gtkFrame("Letter case")
  frame$add(vbox)
  dialog[["vbox"]]$add(frame)
  #Require response before interacting with table
  response <- dialog$run()

  #Find selection
  for (i in 1:length(radio_buttons)) {
    if (gtkToggleButtonGetActive(radio_buttons[[i]])) {
      selectn <- i
    }
  }
  selection <- choices[selectn]
  
  #Destroy dialog box
  gtkWidgetDestroy(dialog)
  
  if (response %in% c(GtkResponseType["close"], GtkResponseType["delete-event"], GtkResponseType["cancel"]) == F) {
    column_classes <- df_obj$get_column_classes()
    column_values <- df_obj$get_column_values(current_row$column)
    if (column_classes[current_row$column] == "numeric") {
      sep <- ""
    } else {
      sep <- "\""
    }
  
    string_builder <- rep(NA, length(column_values) + 2)
    if (toupper(selection) == "UPPERCASE") {
      string_builder[1] <- paste0("IF MISSING(", current_row$column, ") THEN <var> = \"\";")
    } else {
      string_builder[1] <- paste0("if missing(", current_row$column, ") then <var>=\"\";")
    }
    j <- 2
    for (i in column_values) {
      if (toupper(selection) == "UPPERCASE") {
        string_builder[j] <- paste0("ELSE IF ", current_row$column, " = ", sep, i, sep, " THEN <var> = \"\";")
      } else {
        string_builder[j] <- paste0("else if ", current_row$column, "=", sep, i, sep, " then <var>=\"\";")
      }
  
      j <- j + 1
    }
    if (toupper(selection) == "UPPERCASE") {
      string_builder[j] <- paste0("ELSE DO;\n    ERR_MSG = CATX(\"|\",\"ERROR: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    PUT ERR_MSG;\nEND;")
    } else {
      string_builder[j] <- paste0("else do;\n    err_msg=catx(\"|\",\"ERROR: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;")
    }
  
    #Copy final string to clipboard
    utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
  }
}

#' e__copy_if_then_do
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_if_then_do <- function(session_name, current_row, df_obj,outer_env=totem) {
  require(RGtk2)
  
  #Generate dialog to ask about code case
  dialog <- gtkMessageDialog(
    parent = outer_env[[session_name]]$windows$main_window, 
    flags = "destroy-with-parent", 
    type = "question", 
    buttons = "ok-cancel", 
    "Select an option for the copied code")
  
  #Add options
  choices <- c("Lowercase", "Uppercase")
  radio_buttons <- NULL
  vbox <- gtkVBox(F, 0)
  for (choice in choices) {
    button <- gtkRadioButton(radio_buttons, choice)
    vbox$add(button)
    radio_buttons <- c(radio_buttons, button)
  }
  
  #Make a frame for the buttons
  frame <- gtkFrame("Letter case")
  frame$add(vbox)
  dialog[["vbox"]]$add(frame)
  #Require response before interacting with table
  response <- dialog$run()

  #Find selection
  for (i in 1:length(radio_buttons)) {
    if (gtkToggleButtonGetActive(radio_buttons[[i]])) {
      selectn <- i
    }
  }
  selection <- choices[selectn]
  
  #Destroy dialog box
  gtkWidgetDestroy(dialog)

  if (response %in% c(GtkResponseType["close"], GtkResponseType["delete-event"], GtkResponseType["cancel"]) == F) {
    column_classes <- df_obj$get_column_classes()
    column_values <- df_obj$get_column_values(current_row$column)
    if (column_classes[current_row$column] == "numeric") {
      sep <- ""
    } else {
      sep <- "\""
    }
  
    string_builder <- rep(NA, length(column_values) + 2)
    if (toupper(selection) == "UPPERCASE") {
      string_builder[1] <- paste0("IF MISSING(", current_row$column, ") THEN DO;\n    <var> = \"\";\nEND;")
    } else {
      string_builder[1] <- paste0("if missing(", current_row$column, ") then do;\n    <var>=\"\";\nend;")
    }
    j <- 2
    for (i in column_values) {
      if (toupper(selection) == "UPPERCASE") {
        string_builder[j] <- paste0("ELSE IF ", current_row$column, " = ", sep, i, sep, " THEN DO;\n    <var> = \"\";\nEND;")
      } else {
        string_builder[j] <- paste0("else if ", current_row$column, "=", sep, i, sep, " then do;\n    <var>=\"\";\nend;")
      }
  
      j <- j + 1
    }
    if (toupper(selection) == "UPPERCASE") {
      string_builder[j] <- paste0("ELSE DO;\n    ERR_MSG = CATX(\"|\",\"ERROR: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    PUT ERR_MSG;\nEND;")
    } else {
      string_builder[j] <- paste0("else do;\n    err_msg=catx(\"|\",\"ERROR: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;")
    }  

    #Copy final string to clipboard
    utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
  }
}
