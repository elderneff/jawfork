

#' e__load_dataset
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return TODO

e__load_dataset <- function(session_name,outer_env=totem) {
  ls_content <- ls(name = .GlobalEnv)
  if ((outer_env[[session_name]]$sas_file_path %in% ls_content) == F) {
    #sas7bdat
    if(tolower(outer_env[[session_name]]$passed_ext)=="sas7bdat"){
      outer_env[[session_name]]$data1 <- as.data.frame(haven::read_sas(data_file = outer_env[[session_name]]$sas_file_path))
      outer_env[[session_name]]$data1_contents <- sas_contents(outer_env[[session_name]]$sas_file_path)
    #sav
    }else if(tolower(outer_env[[session_name]]$passed_ext)=="sav"){
      outer_env[[session_name]]$data1 <- as.data.frame(sjlabelled::unlabel(haven::read_sav(outer_env[[session_name]]$sas_file_path)),stringsAsFactors = FALSE) 
      outer_env[[session_name]]$data1_contents <- data.frame(
          "variable" = colnames(outer_env[[session_name]]$data1),
          "length" = NA,
          "type" = NA,
          "label" = NA,
          "n" = NA,
          stringsAsFactors = FALSE
        )
    #rds
    }else if(tolower(outer_env[[session_name]]$passed_ext)=="rds"){
      outer_env[[session_name]]$data1 <- as.data.frame(readRDS(file=outer_env[[session_name]]$sas_file_path),stringsAsFactors = FALSE) 
      outer_env[[session_name]]$data1_contents <- data.frame(
          "variable" = colnames(outer_env[[session_name]]$data1),
          "length" = NA,
          "type" = NA,
          "label" = NA,
          "n" = NA,
          stringsAsFactors = FALSE
        )
    #csv
    }else if(tolower(outer_env[[session_name]]$passed_ext)=="csv"){
      outer_env[[session_name]]$data1 <- as.data.frame(read.csv(file=outer_env[[session_name]]$sas_file_path)
                                                       , header = TRUE, sep = ",", skip = 0, stringsAsFactors = FALSE)
      outer_env[[session_name]]$data1_contents <- data.frame(
          "variable" = colnames(outer_env[[session_name]]$data1),
          "length" = NA,
          "type" = NA,
          "label" = NA,
          "n" = NA,
          stringsAsFactors = FALSE
        )      
    }
  } else {
    outer_env[[session_name]]$data1 <- as.data.frame(get(x = outer_env[[session_name]]$sas_file_path, envir = .GlobalEnv))


    outer_env[[session_name]]$data1_contents <- data.frame(
      "variable" = colnames(outer_env[[session_name]]$data1),
      "length" = NA,
      "type" = NA,
      "label" = NA,
      "n" = NA,
      stringsAsFactors = FALSE
    )
  }

  file_history <- rbind(data.frame(
    "latest" = T,
    "mtime" = file.info(outer_env[[session_name]]$sas_file_path, extra_cols = TRUE)$mtime,
    "load_time" = as.character(Sys.time()),
    "dataset" = gsub(paste0("\\.",outer_env[[session_name]]$passed_ext), "", outer_env[[session_name]]$sas_file_basename),
    "full_path" = outer_env[[session_name]]$sas_file_path,
    stringsAsFactors = FALSE
  ), totem$settings_list$file_history)

  file_history <- file_history[duplicated(file_history[, -(1:3)]) == F, ]
  totem$settings_list$file_history <- file_history
  totem$file_history$file_history_window_table$update(file_history)


  outer_env$u__load_dataset_filter(session_name)
}


#' e__load_dataset_filter
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return TODO

e__load_dataset_filter <- function(session_name,outer_env=totem) {

  df <- outer_env$u__load_dataset_filter_inner(session_name)




  # outer_env[[session_name]]$data_row_num <- seq_len(nrow(df))
  # df <- df[c("r__2", setdiff(colnames(df), 'r__2'))]

  outer_env[[session_name]]$data2 <- df





  if (nrow(outer_env[[session_name]]$data1) == nrow(outer_env[[session_name]]$data2)) {
    str_row <- paste0(nrow(outer_env[[session_name]]$data2))
  } else {
    str_row <- paste0(nrow(outer_env[[session_name]]$data2), " (", round(100 * nrow(outer_env[[session_name]]$data2) / nrow(outer_env[[session_name]]$data1), 2), "%)")
  }



  if (ncol(outer_env[[session_name]]$data1) == ncol(outer_env[[session_name]]$data2)) {
    str_col <- paste0(ncol(outer_env[[session_name]]$data2))
  } else {
    str_col <- paste0(ncol(outer_env[[session_name]]$data2), " (", round(100 * ncol(outer_env[[session_name]]$data2) / ncol(outer_env[[session_name]]$data1), 2), "%)")
  }


  str_dim <- paste0(str_row, " x ", str_col)
  RGtk2::gtkLabelSetLabel(outer_env[[session_name]]$data_view_list$code_tool_bar_dim_label, str_dim)


  outer_env[[session_name]]$data3 <- df_meta_contents(outer_env[[session_name]]$data2, outer_env[[session_name]]$data1_contents)

  outer_env[[session_name]]$data_view_list$slot1_list$full_table$update(outer_env[[session_name]]$data2)
  outer_env[[session_name]]$data_view_list$slot1_list$meta_table$update(outer_env[[session_name]]$data3)
  RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$slot2_box)

}

#' e__load_dataset_filter_inner_select
#'
#' @param session_name TODO
#' @param df  TODO
#' @param outer_env TODO
#'
#' @return TODO

e__load_dataset_filter_inner_select <- function(session_name, df,outer_env=totem) {
  select_txt <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
  if (select_txt == "") {
    return(df)
  }






  outer_env[[session_name]]$e$df <- df



  cmd <- paste0("df <- df %>% select(", select_txt, ")")

  tryCatch(
    {
      eval(parse(text = cmd), envir = outer_env[[session_name]]$e)
    },
    error = function(e) {
      outer_env[[session_name]]$e$df <- df
      dialog <- RGtk2::gtkMessageDialog(
        parent = outer_env[[session_name]]$windows$main_window,
        flags = "destroy-with-parent",
        type = "question",
        buttons = "ok-cancel",
        "select Error: Should entry by cleared?"
      )
      dialog["secondary-text"] <- cmd


      ###################################################
      ### code chunk number 62: Pre-defined-dialogs.Rnw:49-58
      ###################################################
      response <- RGtk2::gtkDialogRun(dialog)
      if (response == RGtk2::GtkResponseType["cancel"] ||
        response == RGtk2::GtkResponseType["close"] ||
        response == RGtk2::GtkResponseType["delete-event"]) {
        ## pass
      } else if (response == RGtk2::GtkResponseType["ok"]) {
        RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, "")
      }
      RGtk2::gtkWidgetDestroy(dialog)
    }
  )

  if (ncol(outer_env[[session_name]]$e$df) == 0) {
    dialog <- RGtk2::gtkMessageDialog(
      parent = outer_env[[session_name]]$windows$main_window,
      flags = "destroy-with-parent",
      type = "question",
      buttons = "ok-cancel",
      "select statement results in zero columns: Do you want to clear it?"
    )
    # dialog['secondary-text'] <- cmd


    ###################################################
    ### code chunk number 62: Pre-defined-dialogs.Rnw:49-58
    ###################################################
    response <- RGtk2::gtkDialogRun(dialog)
    if (response == RGtk2::GtkResponseType["cancel"] ||
      response == RGtk2::GtkResponseType["close"] ||
      response == RGtk2::GtkResponseType["delete-event"]) {

    } else if (response == RGtk2::GtkResponseType["ok"]) {
      RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, "")
    }
    RGtk2::gtkWidgetDestroy(dialog)
    return(df)
  }


  return(outer_env[[session_name]]$e$df)
}


#' e__load_dataset_filter_inner
#'
#' @param session_name TODO
#' @param outer_env TODO
#'
#' @return TODO

e__load_dataset_filter_inner <- function(session_name,outer_env=totem) {
  source_file <- RGtk2::gtkToggleButtonGetActive(outer_env[[session_name]]$data_view_list$file_source_cb)
  if (source_file == T) {
    session_tag <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$file_source_entry)
    file_content <- read_text_file(totem$code_R)
    start_pat <- paste0("#start@", session_tag)
    end_pat <- paste0("#end@", session_tag)

    if (grepl(start_pat, file_content, fixed = T) == T & grepl(end_pat, file_content, fixed = T) == T) {
      sourced_code <- stringr::str_match(file_content, paste0(start_pat, "([\\s\\S]*)", end_pat))
      if (is.na(sourced_code[1, 2]) == F) {
        u__text_area_clear(outer_env[[session_name]]$text_area_1)
        u__text_area_set_text(outer_env[[session_name]]$text_area_1, trimws(sourced_code[1, 2]))
      }
    }
  }


  cmd <- u__text_area_get_text(outer_env[[session_name]]$text_area_1)



  if (cmd == "") {
    return(outer_env$u__load_dataset_filter_inner_select(session_name, outer_env[[session_name]]$data1))
  } else {
    tryCatch(
      {
        outer_env[[session_name]]$e$df <- outer_env[[session_name]]$data1

        eval(parse(text = cmd), envir = outer_env[[session_name]]$e)
        previous_code <- rbind(data.frame(
          "time" = as.character(Sys.time()),
          "dataset" = gsub(paste0("\\.",outer_env[[session_name]]$passed_ext), "", outer_env[[session_name]]$sas_file_basename),
          "code" = cmd,
          "full_path" = outer_env[[session_name]]$sas_file_path,
          stringsAsFactors = FALSE
        ), totem$settings_list$previous_code)

        previous_code <- previous_code[duplicated(previous_code[, -1]) == F, ]
        totem$settings_list$previous_code <- previous_code
        outer_env[[session_name]]$past_code_window_table$update(previous_code)




        return(outer_env$u__load_dataset_filter_inner_select(session_name, outer_env[[session_name]]$e$df))
      },
      error = function(e) {
        dialog <- RGtk2::gtkMessageDialog(
          parent = outer_env[[session_name]]$windows$main_window,
          flags = "destroy-with-parent",
          type = "question",
          buttons = "ok",
          "Code Error"
        )
        dialog["secondary-text"] <- as.character(e)


        ###################################################
        ### code chunk number 62: Pre-defined-dialogs.Rnw:49-58
        ###################################################
        response <- RGtk2::gtkDialogRun(dialog)
        if (response == RGtk2::GtkResponseType["cancel"] ||
          response == RGtk2::GtkResponseType["close"] ||
          response == RGtk2::GtkResponseType["delete-event"]) {
          ## pass
        } else if (response == RGtk2::GtkResponseType["ok"]) {
          message("Ok")
        }
        RGtk2::gtkWidgetDestroy(dialog)




        return(outer_env$u__load_dataset_filter_inner_select(session_name, outer_env[[session_name]]$data1))
      }
    )
  }
}
