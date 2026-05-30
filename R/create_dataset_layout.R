

#' create_dataset_layout
#'
#' @param full_df TODO
#'
#' @return TODO

create_dataset_layout <- function(full_df) {
  string_builder <- c("proc sql;\n\tcreate table dataset_layout")

  string_builder <- c(string_builder, "\t(")

  total_rows <- nrow(full_df)
  for (i in seq_len(total_rows)) {
    string_builder <- c(
      string_builder,
      paste0(
        "\t\t", full_df$variable[i], " ",
        ifelse(full_df$class[i] == "character", "varchar", "num"), " ",
        ifelse(is.na(full_df$length[i]) == T, "", paste0("length=", full_df$length[i])), " ",
        ifelse(is.na(full_df$label[i]) == T, "", paste0("label=\"", full_df$label[i], "\"")),
        ifelse(i < total_rows, ",", "")
      )
    )
  }

  string_builder <- c(string_builder, "\t);")

  string_builder <- c(string_builder, "quit;\n")

  return(paste0(string_builder, collapse = "\n"))
}


#' create_dataset_layout_label
#'
#' @param full_df TODO
#'
#' @return TODO

create_dataset_layout_label <- function(full_df, sp = "=") {
  string_builder <- c()
  if (any(is.na(full_df$label) == F)) {
    string_builder <- c(string_builder, "\tlabel")
    for (i in seq_len(nrow(full_df))) {
      if (is.na(full_df$label[i]) == F) {
        string_builder <- c(
          string_builder,
          paste0("\t\t", full_df$variable[i], sp, "\"", full_df$label[i], "\"")
        )
      }
    }
    string_builder <- c(string_builder, "\t;")
  }
  return(paste0(string_builder, collapse = "\n"))
}



#' create_dataset_layout_length
#'
#' @param full_df TODO
#'
#' @return TODO

create_dataset_layout_length <- function(full_df) {
  string_builder <- c("\tlength ")

  for (i in seq_len(nrow(full_df))) {
    string_builder <- c(
      string_builder,
      paste0("\t\t", full_df$variable[i], " ", ifelse(full_df$class[i] == "character", "$", ""), full_df$length[i])
    )
  }

  string_builder <- c(string_builder, "\t;")


  return(paste0(string_builder, collapse = "\n"))
}



#' create_dataset_layout_keep
#'
#' @param full_df TODO
#'
#' @return TODO

create_dataset_layout_keep <- function(full_df) {
  string_builder <- c("\tkeep")

  for (i in seq_len(nrow(full_df))) {
    string_builder <- c(
      string_builder,
      paste0(" ", full_df$variable[i])
    )
  }

  string_builder <- c(string_builder, ";")


  return(paste0(string_builder, collapse = ""))
}
