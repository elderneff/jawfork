
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
  if (!outer_env$u__check_code_prefs(session_name)) return()
  
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
  if (!outer_env$u__check_code_prefs(session_name)) return()
  
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
      string_builder[j] <- paste0("ELSE IF ", current_row$column, sp, sep, i, sep, " THEN DO;\n    VAR", sp, "\"\";\nEND;")
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
