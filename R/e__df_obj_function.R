
#' e__df_obj_function
#'
#' @param box TODO
#' @param outer_env TODO
#' @param obj_env TODO
#'
#' @return TODO

e__df_obj_function <- function(box, outer_env = totem,obj_env=inner_env) {


  obj_env$df_obj_list <- list()


  table_obj <- obj_env$table_obj_function(box)

  obj_env$df_obj_list$column_classes <- NULL
  obj_env$df_obj_list$full_df <- NULL
  obj_env$df_obj_list$filtered_df <- NULL


  e <- new.env()


  call_generate_full_df <- function(df) {

    column_classes_temp <- rep(NA, ncol(df))
    names(column_classes_temp) <- colnames(df)
    for (i in seq_len(ncol(df))) {
      column_classes_temp[colnames(df)[i]] <- paste0(class(df[[colnames(df)[i]]]), collapse = "/")
    }
    obj_env$df_obj_list$column_classes <- column_classes_temp

    df$r__ <- seq_len(nrow(df))
    df <- df[c("r__", setdiff(colnames(df), "r__"))]


    obj_env$page_obj$to_page(1)
    obj_env$df_obj_list$full_df <- df
    update_filter()
  }

  get_filtered_df_nrow <- function() {
    return(nrow(obj_env$df_obj_list$filtered_df))
  }

  update_filter <- function() {

    obj_env$page_obj$to_page(1)

    if (is_full_data_table) {
      obj_env$df_obj_list$filtered_df <- obj_env$generic_filter_function(e, obj_env$df_obj_list$full_df, obj_env$order_by_obj$get(), obj_env$filter_obj$get(), obj_env$select_obj$get(), table_obj)
    } else {
      obj_env$df_obj_list$filtered_df <- obj_env$generic_filter_function(e, obj_env$df_obj_list$full_df, obj_env$order_by_obj$get(), obj_env$filter_obj$get(), "", table_obj)
    }



    if (nrow(obj_env$df_obj_list$full_df) == nrow(obj_env$df_obj_list$filtered_df)) {
      obj_env$filter_obj$set_dim(paste0(nrow(obj_env$df_obj_list$filtered_df), " x ", ncol(obj_env$df_obj_list$filtered_df)))
    } else {
      obj_env$filter_obj$set_dim(paste0(nrow(obj_env$df_obj_list$filtered_df), "(", round((nrow(obj_env$df_obj_list$filtered_df) / nrow(obj_env$df_obj_list$full_df)) * 100, 2), "%) x ", ncol(obj_env$df_obj_list$filtered_df)))
    }


    draw_table()
  }

  draw_table <- function() {

    page <- obj_env$page_obj$get_page()
    if (nrow(obj_env$df_obj_list$filtered_df) >= page) {
      df <- obj_env$df_obj_list$filtered_df[page:(min(nrow(obj_env$df_obj_list$filtered_df), page + rows_length)), , drop = F]
    } else {
      df <- obj_env$df_obj_list$filtered_df
    }


    table_obj$update_table(as.matrix(df))
    return(T)
  }


  full_data <- function() {
    return(obj_env$df_obj_list$full_df)
  }

  current_data <- function() {
    return(obj_env$df_obj_list$filtered_df)
  }
  get_current_row <- function() {
    return(table_obj$get_current_row())
  }

  get_column_classes <- function() {
    return(obj_env$df_obj_list$column_classes)
  }

  get_column_values <- function(pass_col) {
    return(sort(unique(obj_env$df_obj_list$filtered_df[, pass_col, drop = T])))
  }

  clear_filters <- function() {
    table_obj$clear_filters()
  }

  view <- function() {
    outer_env$u__df_view(obj_env$df_obj_list$full_df, paste0("Full Data: ", as.character(Sys.time())), height = 400, width = 500)
  }

  copy_full <- function(pass_columns = NULL, vector = F) {
    if (is.null(pass_columns) == T) {
      clipr::write_clip(obj_env$df_obj_list$full_df, allow_non_interactive = T)
    } else {
      if (vector == F) {
        x <- obj_env$df_obj_list$full_df[, pass_columns, drop = F]
        clipr::write_clip(x, allow_non_interactive = T)
      } else {
        x <- datapasta::vector_construct(obj_env$df_obj_list$full_df[, pass_columns, drop = T])
        #If copying as a vector, remove all newlines
        x <- gsub("\n", "", x)
        #Use utils::writeClipboard instead of clipr::write_clip to remove linebreak
        utils::writeClipboard(str = charToRaw(paste0(x, " ")), format = 1)
      }
    }
  }

  copy_full_to_file <- function(pass_columns = NULL, vector = F) {
    temp_df<- data.frame(
      stringsAsFactors = FALSE,
      A = c(1L, 2L, 3L),
      B = c("q", "w", "e"),
      C = c(45L, 34L, 23L)
    )
    if (is.null(pass_columns) == T) {
      temp_df<- obj_env$df_obj_list$full_df
    } else {
      if (vector == F) {
        temp_df<- obj_env$df_obj_list$full_df[, pass_columns, drop = F]
      } else {
        temp_df<-  datapasta::vector_construct(obj_env$df_obj_list$full_df[, pass_columns, drop = T])
      }
    }

    #######################################
    # Generate dialog to get user options #
    #######################################
    require(RGtk2)
    
    dialog <- gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window, 
      flags = "destroy-with-parent", 
      type = "question", 
      buttons = "ok-cancel", 
      "Select options for the file")
  
    #Add NA options
    choices <- c("Blank", "NA")
    radio_buttons <- NULL
    vbox_NA <- gtkVBox(F, 0)
    for (choice in choices) {
      button <- gtkRadioButton(radio_buttons, choice)
      vbox_NA$add(button)
      radio_buttons <- c(radio_buttons, button)
    }
    
    #Add row index options
    choices <- c("Row index", "No row index")
    radio_buttons <- NULL
    vbox_index <- gtkVBox(F, 0)
    for (choice in choices) {
      button <- gtkRadioButton(radio_buttons, choice)
      vbox_index$add(button)
      radio_buttons <- c(radio_buttons, button)
    }
    
    #Make a frame for NA
    frame <- gtkFrame("Setting for NA numeric values")
    frame$add(vbox_NA)
    dialog[["vbox"]]$add(frame)
    #Require response before interacting with table
    response <- dialog$run()
    
    #Make a frame for row index
    frame <- gtkFrame("Setting for row index")
    frame$add(vbox_index)
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
    print(selection)
    
    #Destroy dialog box
    gtkWidgetDestroy(dialog)

    ##############
    # Write file #
    ##############
    sas_w_ext <- outer_env[[session_name]]$sas_file_basename
    sas_ext <- unlist(gregexpr('.', sas_w_ext, fixed = T))[1]
    sas_no_ext <- substr(sas_w_ext, 1, sas_ext - 1)
    temp <- paste0(tempdir(), "\\", sas_no_ext, ".csv")
    user_na <-""
    user_names <- T
    write.table(temp_df, sep = ",", file=temp, row.names = F, na = user_na, col.names = user_names)
    shell.exec(temp)
  }

  copy_filter <- function(pass_columns = NULL, vector = F) {
    if (is.null(pass_columns) == T) {
      clipr::write_clip(obj_env$df_obj_list$filtered_df, allow_non_interactive = T)
    } else {
      if (vector == F) {
        x <- obj_env$df_obj_list$filtered_df[, pass_columns, drop = F]
        clipr::write_clip(x, allow_non_interactive = T)
      } else {
        x <- datapasta::vector_construct(obj_env$df_obj_list$filtered_df[, pass_columns, drop = T])
        #If copying as a vector, remove all newlines
        x <- gsub("\n", "", x)
        #Use utils::writeClipboard instead of clipr::write_clip to remove linebreak
        utils::writeClipboard(str = charToRaw(paste0(x, " ")), format = 1)
      }
    }
  }


  copy_dataset_layout <- function() {
    x <- create_dataset_layout(obj_env$df_obj_list$full_df)
    clipr::write_clip(x, allow_non_interactive = T)
  }


  copy_keep <- function() {
    x <- create_dataset_layout_keep(obj_env$df_obj_list$full_df)
    clipr::write_clip(x, allow_non_interactive = T)
  }


  copy_label <- function() {
    x <- create_dataset_layout_label(obj_env$df_obj_list$full_df)
    clipr::write_clip(x, allow_non_interactive = T)
  }


  copy_length <- function() {
    x <- create_dataset_layout_length(obj_env$df_obj_list$full_df)
    clipr::write_clip(x, allow_non_interactive = T)
  }


  return(list(
    full_data = full_data, get_filtered_df_nrow = get_filtered_df_nrow, update_filter = update_filter,
    draw_table = draw_table, call_generate_full_df = call_generate_full_df,
    current_data = current_data, get_current_row = get_current_row, clear_filters = clear_filters,
    view = view, copy_full = copy_full, copy_full_to_file = copy_full_to_file, copy_filter = copy_filter,
    copy_dataset_layout = copy_dataset_layout, copy_keep = copy_keep,
    copy_label = copy_label, copy_length = copy_length, get_column_classes = get_column_classes, get_column_values = get_column_values
  ))
}
