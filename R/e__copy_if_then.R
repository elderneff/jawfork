
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
  
  # 1. Check if we need to prompt the user for code preferences (Case/Spacing)
  if (!outer_env$u__check_code_prefs(session_name)) return()
  
  # 2. Prompt user for IF/THEN specific options
  dialog <- gtkMessageDialog(
    parent = outer_env[[session_name]]$windows$main_window, 
    flags = "destroy-with-parent", 
    type = "question", 
    buttons = "ok-cancel", 
    "Select options for IF THEN generation:"
  )
  
  vbox <- dialog[["vbox"]]
  
  # Missing check toggle
  cb_missing <- gtkCheckButtonNewWithLabel("Include check for missing values as the first branch")
  gtkToggleButtonSetActive(cb_missing, TRUE)
  gtkBoxPackStart(vbox, cb_missing, FALSE, FALSE, 0)
  
  # Warning toggle
  cb_warning <- gtkCheckButtonNewWithLabel("Include warning for unexpected values")
  gtkToggleButtonSetActive(cb_warning, TRUE)
  gtkBoxPackStart(vbox, cb_warning, FALSE, FALSE, 0)
  
  # Var Type Radio Buttons
  frame_type <- gtkFrame("Target Variable Type")
  vbox_type <- gtkVBox(FALSE, 0)
  rb_char <- gtkRadioButton(NULL, "Character (\"\")")
  rb_num <- gtkRadioButton(rb_char, "Numeric (.)")
  gtkBoxPackStart(vbox_type, rb_char, FALSE, FALSE, 0)
  gtkBoxPackStart(vbox_type, rb_num, FALSE, FALSE, 0)
  gtkContainerAdd(frame_type, vbox_type)
  gtkBoxPackStart(vbox, frame_type, FALSE, FALSE, 0)
  
  gtkWidgetShowAll(vbox)
  
  # Require response
  response <- dialog$run()
  if (response != RGtk2::GtkResponseType["ok"]) {
    gtkWidgetDestroy(dialog)
    return()
  }
  
  # Extract values and destroy dialog
  inc_missing <- gtkToggleButtonGetActive(cb_missing)
  inc_warning <- gtkToggleButtonGetActive(cb_warning)
  is_num_target <- gtkToggleButtonGetActive(rb_num)
  gtkWidgetDestroy(dialog)

  # 3. Extract preferences
  c_case <- outer_env$settings_list$code_case
  c_space <- outer_env$settings_list$code_spacing
  sp <- ifelse(c_space == "Spaced (x = y)", " = ", "=")
  tgt_val <- ifelse(is_num_target, ".", '""')
  
  column_classes <- df_obj$get_column_classes()
  column_values <- df_obj$get_column_values(current_row$column)
  sep <- ifelse(column_classes[current_row$column] == "numeric", "", "\"")
  
  string_builder <- c()
  is_upper <- toupper(c_case) == "UPPERCASE"
  first_cond <- TRUE
  
  # 4. Generate Code
  if (inc_missing) {
    if (is_upper) {
      string_builder <- c(string_builder, paste0("IF MISSING(", current_row$column, ") THEN VAR", sp, tgt_val, ";"))
    } else {
      string_builder <- c(string_builder, paste0("if missing(", current_row$column, ") then var", sp, tgt_val, ";"))
    }
    first_cond <- FALSE
  }
  
  for (i in column_values) {
    if (is_upper) {
      prefix <- ifelse(first_cond, "IF", "ELSE IF")
      string_builder <- c(string_builder, paste0(prefix, " ", current_row$column, sp, sep, i, sep, " THEN VAR", sp, tgt_val, ";"))
    } else {
      prefix <- ifelse(first_cond, "if", "else if")
      string_builder <- c(string_builder, paste0(prefix, " ", current_row$column, sp, sep, i, sep, " then var", sp, tgt_val, ";"))
    }
    first_cond <- FALSE
  }
  
  if (inc_warning) {
    if (is_upper) {
      string_builder <- c(string_builder, paste0("ELSE DO;\n    ERR_MSG", sp, "CATX(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    PUT ERR_MSG;\nEND;"))
    } else {
      string_builder <- c(string_builder, paste0("else do;\n    err_msg", sp, "catx(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;"))
    }
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
  
  # 1. Check if we need to prompt the user for code preferences (Case/Spacing)
  if (!outer_env$u__check_code_prefs(session_name)) return()
  
  # 2. Prompt user for IF THEN DO specific options
  dialog <- gtkMessageDialog(
    parent = outer_env[[session_name]]$windows$main_window, 
    flags = "destroy-with-parent", 
    type = "question", 
    buttons = "ok-cancel", 
    "Select options for IF THEN DO generation:"
  )
  
  vbox <- dialog[["vbox"]]
  
  # Missing check toggle
  cb_missing <- gtkCheckButtonNewWithLabel("Include check for missing values as the first branch")
  gtkToggleButtonSetActive(cb_missing, TRUE)
  gtkBoxPackStart(vbox, cb_missing, FALSE, FALSE, 0)
  
  # Warning toggle
  cb_warning <- gtkCheckButtonNewWithLabel("Include warning for unexpected values")
  gtkToggleButtonSetActive(cb_warning, TRUE)
  gtkBoxPackStart(vbox, cb_warning, FALSE, FALSE, 0)
  
  # Var Type Radio Buttons
  frame_type <- gtkFrame("Target Variable Type")
  vbox_type <- gtkVBox(FALSE, 0)
  rb_char <- gtkRadioButton(NULL, "Character (\"\")")
  rb_num <- gtkRadioButton(rb_char, "Numeric (.)")
  gtkBoxPackStart(vbox_type, rb_char, FALSE, FALSE, 0)
  gtkBoxPackStart(vbox_type, rb_num, FALSE, FALSE, 0)
  gtkContainerAdd(frame_type, vbox_type)
  gtkBoxPackStart(vbox, frame_type, FALSE, FALSE, 0)
  
  gtkWidgetShowAll(vbox)
  
  # Require response
  response <- dialog$run()
  if (response != RGtk2::GtkResponseType["ok"]) {
    gtkWidgetDestroy(dialog)
    return()
  }
  
  # Extract values and destroy dialog
  inc_missing <- gtkToggleButtonGetActive(cb_missing)
  inc_warning <- gtkToggleButtonGetActive(cb_warning)
  is_num_target <- gtkToggleButtonGetActive(rb_num)
  gtkWidgetDestroy(dialog)

  # 3. Extract preferences
  c_case <- outer_env$settings_list$code_case
  c_space <- outer_env$settings_list$code_spacing
  sp <- ifelse(c_space == "Spaced (x = y)", " = ", "=")
  tgt_val <- ifelse(is_num_target, ".", '""')
  
  column_classes <- df_obj$get_column_classes()
  column_values <- df_obj$get_column_values(current_row$column)
  sep <- ifelse(column_classes[current_row$column] == "numeric", "", "\"")
  
  string_builder <- c()
  is_upper <- toupper(c_case) == "UPPERCASE"
  first_cond <- TRUE
  
  # 4. Generate Code
  if (inc_missing) {
    if (is_upper) {
      string_builder <- c(string_builder, paste0("IF MISSING(", current_row$column, ") THEN DO;\n    VAR", sp, tgt_val, ";\nEND;"))
    } else {
      string_builder <- c(string_builder, paste0("if missing(", current_row$column, ") then do;\n    var", sp, tgt_val, ";\nend;"))
    }
    first_cond <- FALSE
  }
  
  for (i in column_values) {
    if (is_upper) {
      prefix <- ifelse(first_cond, "IF", "ELSE IF")
      string_builder <- c(string_builder, paste0(prefix, " ", current_row$column, sp, sep, i, sep, " THEN DO;\n    VAR", sp, tgt_val, ";\nEND;"))
    } else {
      prefix <- ifelse(first_cond, "if", "else if")
      string_builder <- c(string_builder, paste0(prefix, " ", current_row$column, sp, sep, i, sep, " then do;\n    var", sp, tgt_val, ";\nend;"))
    }
    first_cond <- FALSE
  }
  
  if (inc_warning) {
    if (is_upper) {
      string_builder <- c(string_builder, paste0("ELSE DO;\n    ERR_MSG", sp, "CATX(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    PUT ERR_MSG;\nEND;"))
    } else {
      string_builder <- c(string_builder, paste0("else do;\n    err_msg", sp, "catx(\"|\",\"#STUB: Unexpected value for ", current_row$column, "\", ", current_row$column, ");\n    put err_msg;\nend;"))
    }
  }
  
  utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
}
