#' e__flat_column
#'
#' @param session_name TODO
#' @param current_row TODO
#' @param outer_env TODO
#'
#' @return TODO

e__flat_column <- function(session_name, current_row,outer_env=totem) {
  temp_df <- outer_env[[session_name]]$data2
  temp_df$random_char_str <- "a"
  temp_df2 <- as.matrix(temp_df)
  temp_df <- temp_df[, -ncol(temp_df)]
  temp_df2 <- temp_df2[, -ncol(temp_df2)]


  cross_tab_names <- setdiff(colnames(current_row$row), c("r__", "n", "freq", "lines", "nchar"))

  my_filter <- rep(T, nrow(temp_df))

  my_title <- ""

  for (x in cross_tab_names) {
    my_title <- paste0(my_title, "| ", x, "==", current_row$row[, x, drop = T])

    my_filter <- my_filter & (temp_df2[, x, drop = T] %in% current_row$row[, x, drop = T])
  }


  y <- temp_df[my_filter, , drop = F]
  z <- y[current_row$column]
  


  outer_env$u__df_view(z,
    paste0("Flat: ", outer_env[[session_name]]$sas_file_basename, " (", nrow(y), " x ", ncol(y), ")", my_title, "|", as.character(Sys.time())),
    height = 300, width = 500
  )
}
