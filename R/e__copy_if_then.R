
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
  cb_missing <- RGtk2::gtkCheckButtonNewWithLabel("Include branch for missing values")
  RGtk2::gtkToggleButtonSetActive(cb_missing, TRUE)
  RGtk2::gtkBoxPackStart(vbox, cb_missing, FALSE, FALSE, 5)
  
  # Warning toggle
  cb_warning <- RGtk2::gtkCheckButtonNewWithLabel("Include warning for unexpected values")
  RGtk2::gtkToggleButtonSetActive(cb_warning, TRUE)
  RGtk2::gtkBoxPackStart(vbox, cb_warning, FALSE, FALSE, 5)
  
  # Var Type Radio Buttons inside a Frame
  frame_type <- RGtk2::gtkFrameNew("Result Variable Type")
  vbox_type <- RGtk2::gtkVBoxNew(FALSE, 5)
  RGtk2::gtkContainerSetBorderWidth(vbox_type, 5)
  
  rb_char <- RGtk2::gtkRadioButtonNewWithLabel(group = NULL, label = "Character")
  rb_num <- RGtk2::gtkRadioButtonNewWithLabelFromWidget(group = rb_char, label = "Numeric")
  
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
  cb_missing <- RGtk2::gtkCheckButtonNewWithLabel("Include branch for missing values")
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
  
  rb_char <- RGtk2::gtkRadioButtonNewWithLabel(group = NULL, label = "Character")
  rb_num <- RGtk2::gtkRadioButtonNewWithLabelFromWidget(group = rb_char, label = "Numeric")
  
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

#' e__copy_mapping
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__copy_mapping <- function(session_name, current_row, outer_env = totem) {
  # 1. Check if user needs to set case/spacing preferences
  if (!outer_env$u__check_code_prefs(session_name)) return()

  temp_df <- outer_env[[session_name]]$data2

  # 2. Extract grouping columns from the Summary Table context
  # The last column is the Target, everything before it are the Conditions
  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines"))

  if (length(cross_tab_names) < 2) {
    err_dialog <- RGtk2::gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window,
      flags = "destroy-with-parent",
      type = "error",
      buttons = "close",
      "There must be at least 2 variables in the value summary to create a mapping."
    )
    err_dialog$run()
    RGtk2::gtkWidgetDestroy(err_dialog)
    return()
  }

  cond_cols <- cross_tab_names[-length(cross_tab_names)]
  target_col <- cross_tab_names[length(cross_tab_names)]

  # 3. Get all unique combinations from the dataset
  unique_df <- unique(temp_df[, cross_tab_names, drop = FALSE])

  # 4. Strictly validate 1-to-1 relationships
  bad_lhs <- c()
  bad_rhs <- c()

  # Check LHS uniqueness (One-to-Many: Do identical conditions map to different targets?)
  if (length(cond_cols) == 1) {
    lhs_vals <- as.character(unique_df[[cond_cols[1]]])
  } else {
    lhs_vals <- apply(unique_df[, cond_cols, drop = FALSE], 1, paste, collapse = " | ")
  }
  lhs_counts <- table(lhs_vals)
  if (any(lhs_counts > 1)) {
    bad_lhs <- names(lhs_counts[lhs_counts > 1])
  }

  # Check RHS uniqueness (Many-to-One: Do multiple conditions map to the same target?)
  rhs_vals <- as.character(unique_df[[target_col]])
  rhs_counts <- table(rhs_vals)
  if (any(rhs_counts > 1)) {
    bad_rhs <- names(rhs_counts[rhs_counts > 1])
  }

  # 5. Dialog Warning if the data isn't 1-to-1
  if (length(bad_lhs) > 0 || length(bad_rhs) > 0) {
    warn_msg <- "Warning: The values in these columns are not strictly 1-to-1.\n\n"
    if (length(bad_lhs) > 0) {
      warn_msg <- paste0(warn_msg, "These condition(s) map to multiple different targets:\n",
                         paste(head(bad_lhs, 5), collapse = "\n"),
                         ifelse(length(bad_lhs) > 5, "\n...", ""), "\n\n")
    }
    if (length(bad_rhs) > 0) {
      warn_msg <- paste0(warn_msg, "These target(s) are mapped to by multiple different conditions:\n",
                         paste(head(bad_rhs, 5), collapse = "\n"),
                         ifelse(length(bad_rhs) > 5, "\n...", ""), "\n\n")
    }
    warn_msg <- paste0(warn_msg, "Do you still want to generate the code?")

    dialog <- RGtk2::gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window,
      flags = "destroy-with-parent",
      type = "warning",
      buttons = "yes-no",
      warn_msg
    )
    response <- dialog$run()
    RGtk2::gtkWidgetDestroy(dialog)

    # -8 is GTK_RESPONSE_YES
    if (response != RGtk2::GtkResponseType["yes"] && response != -8) {
      return()
    }
  }

  # 6. Prepare Code Generation Preferences
  c_case <- outer_env$settings_list$code_case
  c_space <- outer_env$settings_list$code_spacing
  sp <- ifelse(c_space == "Spaced (x = y)", " = ", "=")
  is_upper <- toupper(c_case) == "UPPERCASE"

  k_if <- ifelse(is_upper, "IF", "if")
  k_else_if <- ifelse(is_upper, "ELSE IF", "else if")
  k_then <- ifelse(is_upper, "THEN", "then")
  k_and <- ifelse(is_upper, "AND", "and")

  string_builder <- c()
  first_cond <- TRUE

  # Helper to properly format SAS values (strings vs numbers vs missing)
  format_val <- function(val, is_num) {
    if (is.na(val)) {
      return(ifelse(is_num, ".", '""'))
    } else {
      sep <- ifelse(is_num, "", "\"")
      return(paste0(sep, val, sep))
    }
  }

  # 7. Generate Code!
  for (i in 1:nrow(unique_df)) {
    cond_strings <- c()
    
    # Build the left-hand side
    for (col in cond_cols) {
      is_num <- is.numeric(temp_df[[col]])
      val_str <- format_val(unique_df[i, col], is_num)
      cond_strings <- c(cond_strings, paste0(col, sp, val_str))
    }

    # Build the right-hand side
    target_is_num <- is.numeric(temp_df[[target_col]])
    target_val_str <- format_val(unique_df[i, target_col], target_is_num)
    target_str <- paste0(target_col, sp, target_val_str)

    prefix <- ifelse(first_cond, k_if, k_else_if)
    combined_conds <- paste(cond_strings, collapse = paste0(" ", k_and, " "))

    line <- paste0(prefix, " ", combined_conds, " ", k_then, " ", target_str, ";")
    string_builder <- c(string_builder, line)
    first_cond <- FALSE
  }

  utils::writeClipboard(str = charToRaw(paste0(paste0(string_builder, collapse = "\n"), " ")), format = 1)
}
