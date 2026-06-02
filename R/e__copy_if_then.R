
#' e__copy_if_then
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param df_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_if_then <- function(session_name, current_row, df_obj, outer_env = totem) {
  # 1. Check if we need to prompt the user for code preferences (Case/Spacing)
  if (!outer_env$u__check_code_prefs(session_name)) return()
  
  # 2. Build a custom Dialog safely
  dialog <- RGtk2::gtkDialogNewWithButtons(
    "IF THEN Generation Options",
    outer_env[[session_name]]$windows$main_window,
    c("modal", "destroy-with-parent"),
    "gtk-ok", RGtk2::GtkResponseType["ok"],
    "gtk-cancel", RGtk2::GtkResponseType["cancel"]
  )
  
  vbox <- dialog[["vbox"]]
  
  # Missing check toggle
  cb_missing <- RGtk2::gtkCheckButtonNewWithLabel("Include check for missing values as the first branch")
  RGtk2::gtkToggleButtonSetActive(cb_missing, TRUE)
  RGtk2::gtkBoxPackStart(vbox, cb_missing, FALSE, FALSE, 5)
  
  # Warning toggle
  cb_warning <- RGtk2::gtkCheckButtonNewWithLabel("Include warning for unexpected values")
  RGtk2::gtkToggleButtonSetActive(cb_warning, TRUE)
  RGtk2::gtkBoxPackStart(vbox, cb_warning, FALSE, FALSE, 5)
  
  # Var Type Radio Buttons inside a Frame
  frame_type <- RGtk2::gtkFrameNew("Target Variable Type")
  vbox_type <- RGtk2::gtkVBoxNew(FALSE, 5)
  RGtk2::gtkContainerSetBorderWidth(vbox_type, 5)
  
  rb_char <- RGtk2::gtkRadioButtonNewWithLabel(group = NULL, label = "Character (\"\")")
  rb_num <- RGtk2::gtkRadioButtonNewWithLabelFromWidget(group = rb_char, label = "Numeric (.)")
  
  RGtk2::gtkBoxPackStart(vbox_type, rb_char, FALSE, FALSE, 0)
  RGtk2::gtkBoxPackStart(vbox_type, rb_num, FALSE, FALSE, 0)
  RGtk2::gtkContainerAdd(frame_type, vbox_type)
  RGtk2::gtkBoxPackStart(vbox, frame_type, FALSE, FALSE, 5)
  
  RGtk2::gtkWidgetShowAll(vbox)
  
  # Require response
  response <- dialog$run()
  
  # 3. Extract logic preferences (accounting for GTK 'ok' integer response)
  if (response == RGtk2::GtkResponseType["ok"] || response == -5) {
    inc_missing <- RGtk2::gtkToggleButtonGetActive(cb_missing)
    inc_warning <- RGtk2::gtkToggleButtonGetActive(cb_warning)
    is_num_target <- RGtk2::gtkToggleButtonGetActive(rb_num)
    RGtk2::gtkWidgetDestroy(dialog)
  } else {
    RGtk2::gtkWidgetDestroy(dialog)
    return()
  }

  c_case <- outer_env$settings_list$code_case
  c_space <- outer_env$settings_list$code_spacing
  sp <- ifelse(c_space == "Spaced (x = y)", " = ", "=")
  tgt_val <- ifelse(is_num_target, ".", '""')
  
  # 4. Safely extract column data directly from the active environment data
  temp_df <- outer_env[[session_name]]$data2
  target_col <- temp_df[[current_row$column]]
  
  if (is.factor(target_col)) {
    column_values <- levels(target_col)
  } else {
    column_values <- sort(na.omit(unique(target_col)))
  }
  
  is_numeric_col <- is.numeric(target_col)
  sep <- ifelse(is_numeric_col, "", "\"")
  
  string_builder <- c()
  is_upper <- toupper(c_case) == "UPPERCASE"
  first_cond <- TRUE
  
  # 5. Generate Code
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
  # 1. Check if we need to prompt the user for code preferences (Case/Spacing)
  if (!outer_env$u__check_code_prefs(session_name)) return()
  
  # 2. Build a custom Dialog safely
  dialog <- RGtk2::gtkDialogNewWithButtons(
    "IF THEN DO Generation Options",
    outer_env[[session_name]]$windows$main_window,
    c("modal", "destroy-with-parent"),
    "gtk-ok", RGtk2::GtkResponseType["ok"],
    "gtk-cancel", RGtk2::GtkResponseType["cancel"]
  )
  
  vbox <- dialog[["vbox"]]
  
  # Missing check toggle
  cb_missing <- RGtk2::gtkCheckButtonNewWithLabel("Include check for missing values as the first branch")
  RGtk2::gtkToggleButtonSetActive(cb_missing, TRUE)
  RGtk2::gtkBoxPackStart(vbox, cb_missing, FALSE, FALSE, 5)
  
  # Warning toggle
  cb_warning <- RGtk2::gtkCheckButtonNewWithLabel("Include warning for unexpected values")
  RGtk2::gtkToggleButtonSetActive(cb_warning, TRUE)
  RGtk2::gtkBoxPackStart(vbox, cb_warning, FALSE, FALSE, 5)
  
  # Var Type Radio Buttons inside a Frame
  frame_type <- RGtk2::gtkFrameNew("Target Variable Type")
  vbox_type <- RGtk2::gtkVBoxNew(FALSE, 5)
  RGtk2::gtkContainerSetBorderWidth(vbox_type, 5)
  
  rb_char <- RGtk2::gtkRadioButtonNewWithLabel(group = NULL, label = "Character (\"\")")
  rb_num <- RGtk2::gtkRadioButtonNewWithLabelFromWidget(group = rb_char, label = "Numeric (.)")
  
  RGtk2::gtkBoxPackStart(vbox_type, rb_char, FALSE, FALSE, 0)
  RGtk2::gtkBoxPackStart(vbox_type, rb_num, FALSE, FALSE, 0)
  RGtk2::gtkContainerAdd(frame_type, vbox_type)
  RGtk2::gtkBoxPackStart(vbox, frame_type, FALSE, FALSE, 5)
  
  RGtk2::gtkWidgetShowAll(vbox)
  
  # Require response
  response <- dialog$run()
  
  # 3. Extract logic preferences (accounting for GTK 'ok' integer response)
  if (response == RGtk2::GtkResponseType["ok"] || response == -5) {
    inc_missing <- RGtk2::gtkToggleButtonGetActive(cb_missing)
    inc_warning <- RGtk2::gtkToggleButtonGetActive(cb_warning)
    is_num_target <- RGtk2::gtkToggleButtonGetActive(rb_num)
    RGtk2::gtkWidgetDestroy(dialog)
  } else {
    RGtk2::gtkWidgetDestroy(dialog)
    return()
  }

  c_case <- outer_env$settings_list$code_case
  c_space <- outer_env$settings_list$code_spacing
  sp <- ifelse(c_space == "Spaced (x = y)", " = ", "=")
  tgt_val <- ifelse(is_num_target, ".", '""')
  
  # 4. Safely extract column data directly from the active environment data
  temp_df <- outer_env[[session_name]]$data2
  target_col <- temp_df[[current_row$column]]
  
  if (is.factor(target_col)) {
    column_values <- levels(target_col)
  } else {
    column_values <- sort(na.omit(unique(target_col)))
  }
  
  is_numeric_col <- is.numeric(target_col)
  sep <- ifelse(is_numeric_col, "", "\"")
  
  string_builder <- c()
  is_upper <- toupper(c_case) == "UPPERCASE"
  first_cond <- TRUE
  
  # 5. Generate Code
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
