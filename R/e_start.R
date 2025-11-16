#' e__start
#'
#' @param sas_file_path TODO
#' @param outer_env TODO
#' @param assign_env TODO
#'
#' @return TODO

e__start <- function(sas_file_path, outer_env = totem, assign_env=.GlobalEnv) {
  ls_content <- ls(name = .GlobalEnv)
  if ((sas_file_path %in% ls_content) == F) {
    sas_file_path <- gsub("\"", "", gsub("\\\\", "/", sas_file_path))
    if (file.exists(sas_file_path) == F) {
      message("File does not exist.")
      return(F)
    }
    passed_ext <- z__file_ext(sas_file_path)
    okay_files <- c("sas7bdat","sav","rds","csv","xpt")
    if ((tolower(passed_ext) %in% okay_files) == F) {
      message(paste0("Only works on files ending in: ",paste0(okay_files, collapse=", ")))
      return(F)
    }
    
  }

  tryCatch(
    {
      outer_env$show_load_window()
      session_name <- outer_env$add_session(sas_file_path)



      #----------------------------------------

      # Main window

      #----------------------------------------

      main_window <- RGtk2::gtkWindow(show = F)

      RGtk2::gtkWindowSetTitle(main_window, "JAW")
      RGtk2::gtkWidgetSetSizeRequest(main_window, 500, 300)

      RGtk2::gtkWindowSetDefaultSize(main_window, totem$settings_list$default_sizes$window[1], totem$settings_list$default_sizes$window[2])
      if (totem$settings_list$maximize == T) {
          RGtk2::gtkWindowMaximize(main_window)
      }





      RGtk2::gSignalConnect(main_window, "delete-event", f = function(window, event, data) {
        session_name <- data[[1]]
        outer_env <- data[[2]]
        outer_env$close_all_windows(session_name)
        return(T)
      }, data = list(session_name, outer_env))

      outer_env[[session_name]]$passed_ext <- passed_ext



      outer_env[[session_name]]$windows$main_window <- main_window
      outer_env[[session_name]]$format_by_entry <- RGtk2::gtkEntry()
      RGtk2::gtkEntrySetText(outer_env[[session_name]]$format_by_entry, "USUBJID")


      outer_env[[session_name]]$main <- list()
      outer_env[[session_name]]$main$main_box <- RGtk2::gtkVBox()
      RGtk2::gtkContainerAdd(outer_env[[session_name]]$windows$main_window, outer_env[[session_name]]$main$main_box)



      #----------------------------------------

      # past code

      #----------------------------------------


      outer_env[[session_name]]$past_code_window <- RGtk2::gtkWindow(show = F)


      past_code_window_main_box <- RGtk2::gtkVBox()

      RGtk2::gtkContainerAdd(outer_env[[session_name]]$past_code_window, past_code_window_main_box)
      # outer_env[[session_name]]$past_code_window$add(past_code_window_main_box)



      RGtk2::gtkWindowSetTitle(outer_env[[session_name]]$past_code_window, "Past Code")

      RGtk2::gtkWidgetSetSizeRequest(outer_env[[session_name]]$past_code_window, 600, 600)
      
      RGtk2::gSignalConnect(outer_env[[session_name]]$past_code_window, "delete-event", f = function(window, event, data) {
        session_name <- data[[1]]
        outer_env <- data[[2]]
        RGtk2::gtkWidgetHide(outer_env[[session_name]]$past_code_window)
        return(T)
      }, data = list(session_name, outer_env))



      outer_env[[session_name]]$past_code_window_table <- outer_env$u__df_tree(
        session_name = session_name,
        passed_box = past_code_window_main_box,
        rows_length = 1000,
        event_mapping = NULL,
        style_list = list(value = RGtk2::pangoFontDescriptionFromString("bold 10")),
        is_value_table = F, is_meta_table = F, is_data_code_table = T
      )
      
      outer_env[[session_name]]$show_past_code_window <- function(session_name, outer_env = totem) {
        outer_env[[session_name]]$past_code_window_table$update(totem$settings_list$previous_code)
        RGtk2::gtkWidgetShow(outer_env[[session_name]]$past_code_window)
      }



      #----------------------------------------

      # data view

      #----------------------------------------



      #----------------------------------------

      # data view: setup

      #----------------------------------------

      outer_env[[session_name]]$data_view_list <- list()
      outer_env[[session_name]]$data_view_list$box <- RGtk2::gtkVBox()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$main$main_box, outer_env[[session_name]]$data_view_list$box)




      outer_env[[session_name]]$data_view_list$top_tables_box <- RGtk2::gtkVBox()
      outer_env[[session_name]]$data_view_list$bottom_tables_frame <- RGtk2::gtkFrame()



      outer_env[[session_name]]$data_view_list$main_paned <- RGtk2::gtkVPaned()


      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$box, outer_env[[session_name]]$data_view_list$main_paned)

      RGtk2::gtkPanedPack1(outer_env[[session_name]]$data_view_list$main_paned, outer_env[[session_name]]$data_view_list$top_tables_box,
        resize = T
      )
      RGtk2::gtkPanedPack2(outer_env[[session_name]]$data_view_list$main_paned, outer_env[[session_name]]$data_view_list$bottom_tables_frame,
        resize = T
      )

      RGtk2::gtkPanedSetPosition(outer_env[[session_name]]$data_view_list$main_paned, totem$settings_list$default_sizes$main_pane)


      outer_env[[session_name]]$data_view_list$bottom_tables_box <- RGtk2::gtkVBox()

      RGtk2::gtkContainerAdd(outer_env[[session_name]]$data_view_list$bottom_tables_frame, outer_env[[session_name]]$data_view_list$bottom_tables_box)


      outer_env[[session_name]]$data_view_list$top_code_frame <- RGtk2::gtkFrame()
      outer_env[[session_name]]$data_view_list$top_data_box <- RGtk2::gtkVBox()

      RGtk2::gtkWidgetSetSizeRequest(outer_env[[session_name]]$data_view_list$top_code_frame, 900, 75)



      outer_env[[session_name]]$data_view_list$top_paned <- RGtk2::gtkVPaned()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$top_tables_box, outer_env[[session_name]]$data_view_list$top_paned, T, T)

      RGtk2::gtkPanedPack1(outer_env[[session_name]]$data_view_list$top_paned, outer_env[[session_name]]$data_view_list$top_code_frame,
        resize = F
      )
      RGtk2::gtkPanedPack2(outer_env[[session_name]]$data_view_list$top_paned, outer_env[[session_name]]$data_view_list$top_data_box,
        resize = T
      )

      RGtk2::gtkPanedSetPosition(outer_env[[session_name]]$data_view_list$top_paned, totem$settings_list$default_sizes$top_pane)




      outer_env[[session_name]]$data_view_list$top_code_box <- RGtk2::gtkVBox()


      RGtk2::gtkContainerAdd(outer_env[[session_name]]$data_view_list$top_code_frame, outer_env[[session_name]]$data_view_list$top_code_box)




      outer_env[[session_name]]$data_view_list$slot1_frame <- RGtk2::gtkFrame()
      outer_env[[session_name]]$data_view_list$slot2_frame <- RGtk2::gtkFrame()




      outer_env[[session_name]]$data_view_list$paned <- RGtk2::gtkHPaned()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$top_data_box, outer_env[[session_name]]$data_view_list$paned, T, T)

      RGtk2::gtkPanedPack1(outer_env[[session_name]]$data_view_list$paned, outer_env[[session_name]]$data_view_list$slot1_frame,
        resize = F
      )
      RGtk2::gtkPanedPack2(outer_env[[session_name]]$data_view_list$paned, outer_env[[session_name]]$data_view_list$slot2_frame,
        resize = T
      )

      RGtk2::gtkPanedSetPosition(outer_env[[session_name]]$data_view_list$paned, totem$settings_list$default_sizes$slot_pane)





      outer_env[[session_name]]$data_view_list$slot1_box <- RGtk2::gtkVBox()
      outer_env[[session_name]]$data_view_list$slot2_box <- RGtk2::gtkVBox()


      RGtk2::gtkContainerAdd(outer_env[[session_name]]$data_view_list$slot1_frame, outer_env[[session_name]]$data_view_list$slot1_box)
      RGtk2::gtkContainerAdd(outer_env[[session_name]]$data_view_list$slot2_frame, outer_env[[session_name]]$data_view_list$slot2_box)





      outer_env[[session_name]]$data_view_list$file_source_bar <- RGtk2::gtkHBox()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$top_code_box, outer_env[[session_name]]$data_view_list$file_source_bar, F, F)

      run_code <- function(session_name, outer_env = totem) {
        outer_env$show_load_window()
        outer_env$u__load_dataset_filter(session_name)
        outer_env$hide_load_window()
      }

      outer_env[[session_name]]$text_area_1 <- u__add_text_area("Code", run_code, session_name, timeline, time, outer_env)

      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$top_code_box, outer_env[[session_name]]$text_area_1$Frame, T, T)

      outer_env[[session_name]]$data_view_list$code_tool_bar <- RGtk2::gtkHBox()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$top_code_box, outer_env[[session_name]]$data_view_list$code_tool_bar, F, F)

      outer_env[[session_name]]$data_view_list$code_tool_bar2 <- RGtk2::gtkHBox()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$top_code_box, outer_env[[session_name]]$data_view_list$code_tool_bar2, F, F)




      #----------------------------------------

      # data view: build meta data

      #----------------------------------------

      load_value_function_inner <- function(session_name, temp_df, cvar, cvar2, outer_env = totem) {        
        if (nrow(temp_df) > 10000) {
          outer_env$show_load_window()
          Sys.sleep(0.05)
        }
        Sys.sleep(0.01)
        try({
          hist_bars <- paste0(rep("|", 100), collapse = "")

          if (length(cvar2) > 0) {
            temp_df <- unique(temp_df[, c(cvar, cvar2)])
            fcount_df <- temp_df %>%
              group_by(!!!syms(cvar2)) %>%
              summarise(n = n()) %>%
              ungroup() %>%
              mutate(
                freq = round(n / sum(n), 3),
                lines = sapply(ceiling(freq * 100), function(x) {
                  substr(hist_bars, 1, x)
                }),
                freq = sprintf("%.3f", freq)
              )

            colnames(fcount_df) <- c(cvar2, "n", "freq", "lines")

            #Find column before n
            resultcol <- grep("^n$", colnames(fcount_df)) - 1
            #Get nchar of column before n
            fcount_df$nchar <- apply(fcount_df[,resultcol], 2, nchar)
          } else {
            fcount_df <- temp_df %>%
              group_by(!!!syms(cvar)) %>%
              summarise(n = n()) %>%
              ungroup() %>%
              mutate(
                freq = round(n / sum(n), 3),
                lines = sapply(ceiling(freq * 100), function(x) {
                  substr(hist_bars, 1, x)
                }),
                freq = sprintf("%.3f", freq)
              )

            colnames(fcount_df) <- c(cvar, "n", "freq", "lines")
            
            #Find column before n
            resultcol <- grep("^n$", colnames(fcount_df)) - 1
            #Get nchar of column before n
            fcount_df$nchar <- apply(fcount_df[,resultcol], 2, nchar)
          }
          
          outer_env[[session_name]]$data_view_list$slot2_list$value_table$update(fcount_df)
          RGtk2::gtkWidgetShow(outer_env[[session_name]]$data_view_list$slot2_box)
        })
        if (nrow(temp_df) > 10000) {
          outer_env$hide_load_window()
        }
      }

      load_value_function <- function(session_name, cvar, outer_env = totem) {
        try({
          temp_df <- outer_env[[session_name]]$data2

          RGtk2::gtkLabelSetLabel(outer_env[[session_name]]$status_bar$info_label, paste0(
            cvar,
            " min length:", min(nchar(as.character(temp_df[[cvar]]))),
            ", max length:", max(nchar(as.character(temp_df[[cvar]])))
          ))

          group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)



          if (group_by_entry != "") {
            cvar <- c(trimws(strsplit(x = group_by_entry, split = ",", fixed = T)[[1]]), cvar)
            cvar <- cvar[cvar %in% colnames(temp_df)]
            cvar <- unique(cvar)
          }


          unique_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$unique_by_entry)



          if (unique_by_entry != "") {
            cvar2 <- trimws(strsplit(x = unique_by_entry, split = ",", fixed = T)[[1]])
            cvar2 <- cvar2[cvar2 %in% colnames(temp_df)]
            cvar2 <- unique(cvar2)
          } else {
            cvar2 <- c()
          }


          load_value_function_inner(session_name, temp_df, cvar, cvar2)
        })
        return(FALSE)
      }


      add_group_by_function <- function(session_name, cvar, outer_env = totem) {
        try({
          temp_df <- outer_env[[session_name]]$data2

          group_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$group_by_entry)




          if (group_by_entry != "") {
            x <- trimws(strsplit(x = group_by_entry, split = ",", fixed = T)[[1]])
            if (cvar %in% x) {
              cvar <- setdiff(x, cvar)
            } else {
              cvar <- c(trimws(strsplit(x = group_by_entry, split = ",", fixed = T)[[1]]), cvar)
            }
          }

          cvar <- cvar[cvar %in% colnames(temp_df)]

          RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$group_by_entry, paste0(cvar, collapse = ", "))


          unique_by_entry <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$unique_by_entry)



          if (unique_by_entry != "") {
            cvar2 <- trimws(strsplit(x = unique_by_entry, split = ",", fixed = T)[[1]])
            cvar2 <- cvar2[cvar2 %in% colnames(temp_df)]
            cvar2 <- unique(cvar2)
          } else {
            cvar2 <- c()
          }


          load_value_function_inner(session_name, temp_df, cvar, cvar2)
        })
        return(FALSE)
      }



      build_meta_data <- function(session_name, outer_env = totem) {
        event_mapping <- list(
          "Meta Table|Trigger Value Summary" = load_value_function,
          "Meta Table|Trigger Value Summary with Group By" = add_group_by_function
        )



        return(outer_env$u__df_tree(
          session_name = session_name,
          passed_box = outer_env[[session_name]]$data_view_list$slot1_box,
          rows_length = 1000,
          event_mapping = event_mapping,
          style_list = list(col1 = RGtk2::pangoFontDescriptionFromString("bold 10")),
          is_value_table = F, is_meta_table = T, is_data_code_table = F
        ))
      }





      build_full_data <- function(session_name, outer_env = totem) {
        event_mapping <- list(
          "Full Data Table|Trigger Value Summary" = load_value_function,
          "Full Data Table|Trigger Value Summary with Group By" = add_group_by_function
        )



        return(outer_env$u__df_tree(
          session_name = session_name,
          passed_box = outer_env[[session_name]]$data_view_list$bottom_tables_box,
          rows_length = 500,
          event_mapping = event_mapping,
          style_list = list(col1 = RGtk2::pangoFontDescriptionFromString("bold 10")),
          is_value_table = F, is_meta_table = F, is_data_code_table = F, is_full_data_table = T
        ))
      }



      #----------------------------------------

      # data view: group_by

      #----------------------------------------

      outer_env[[session_name]]$data_view_list$group_by_box <- RGtk2::gtkHBox()

      outer_env[[session_name]]$data_view_list$group_by_label <- RGtk2::gtkLabel("group_by: ")
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$group_by_box, outer_env[[session_name]]$data_view_list$group_by_label, F, F, padding = 2)


      outer_env[[session_name]]$data_view_list$group_by_entry <- RGtk2::gtkEntry()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$group_by_box, outer_env[[session_name]]$data_view_list$group_by_entry, T, T)





      u__button(
        box = outer_env[[session_name]]$data_view_list$group_by_box,
        start = T, padding = 2,
        stock_id = "gtk-close",
        tool_tip = "Clear",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$group_by_entry, "")
          return(FALSE)
        }, data = list(session_name, outer_env)
      )




      #----------------------------------------

      # data view: select

      #----------------------------------------

      outer_env[[session_name]]$data_view_list$select_box <- RGtk2::gtkHBox()

      outer_env[[session_name]]$data_view_list$select_label <- RGtk2::gtkLabel("select: ")
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$select_box, outer_env[[session_name]]$data_view_list$select_label, F, F, padding = 2)


      outer_env[[session_name]]$data_view_list$select_entry <- RGtk2::gtkEntry()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$select_box, outer_env[[session_name]]$data_view_list$select_entry, T, T)


      RGtk2::gSignalConnect(outer_env[[session_name]]$data_view_list$select_entry, "activate", function(menu, data) {
        session_name <- data[[1]]
        outer_env <- data[[2]]
        outer_env$show_load_window()
        outer_env$u__load_dataset_filter(session_name)
        outer_env$hide_load_window()
        return(T)
      }, data = list(session_name, outer_env))


      u__button(
        box = outer_env[[session_name]]$data_view_list$select_box,
        start = T, padding = 2,
        but_txt = "s",
        tool_tip = "starts_with",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
          if (st != "") {
            st <- paste0(st, ", starts_with()")
          } else {
            st <- "starts_with()"
          }
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, st)
          return(FALSE)
        }, data = list(session_name, outer_env)
      )


      u__button(
        box = outer_env[[session_name]]$data_view_list$select_box,
        start = T, padding = 2,
        but_txt = "c",
        tool_tip = "contains",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
          if (st != "") {
            st <- paste0(st, ", contains()")
          } else {
            st <- "contains()"
          }
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, st)
          return(FALSE)
        }, data = list(session_name, outer_env)
      )




      u__button(
        box = outer_env[[session_name]]$data_view_list$select_box,
        start = T, padding = 2,
        but_txt = "m",
        tool_tip = "matches",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          st <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$data_view_list$select_entry)
          if (st != "") {
            st <- paste0(st, ", matches()")
          } else {
            st <- "matches()"
          }
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, st)
          return(FALSE)
        }, data = list(session_name, outer_env)
      )


      u__button(
        box = outer_env[[session_name]]$data_view_list$select_box,
        start = T, padding = 2,
        stock_id = "gtk-close",
        tool_tip = "Clear",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          outer_env$show_load_window()
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$select_entry, "")
          outer_env$u__load_dataset_filter(session_name)
          outer_env$hide_load_window()
          return(FALSE)
        }, data = list(session_name, outer_env)
      )




      #----------------------------------------

      # data view: unique_by

      #----------------------------------------


      outer_env[[session_name]]$data_view_list$unique_by_box <- RGtk2::gtkHBox()

      outer_env[[session_name]]$data_view_list$unique_by_label <- RGtk2::gtkLabel("unique_by: ")
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$unique_by_box, outer_env[[session_name]]$data_view_list$unique_by_label, F, F, padding = 2)


      outer_env[[session_name]]$data_view_list$unique_by_entry <- RGtk2::gtkEntry()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$unique_by_box, outer_env[[session_name]]$data_view_list$unique_by_entry, T, T)





      u__button(
        box = outer_env[[session_name]]$data_view_list$unique_by_box,
        start = T, padding = 2,
        stock_id = "gtk-close",
        tool_tip = "Clear",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$unique_by_entry, "")
          return(FALSE)
        }, data = list(session_name, outer_env)
      )


      #----------------------------------------

      # data view: slot1

      #----------------------------------------


      outer_env[[session_name]]$data_view_list$file_source_cb <- RGtk2::gtkCheckButtonNewWithLabel("Source", show = FALSE)
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$file_source_bar, outer_env[[session_name]]$data_view_list$file_source_cb, F, F, padding = 1)


      #u__button(
      #  box = outer_env[[session_name]]$data_view_list$file_source_bar,
      #  start = T, padding = 2,
      #  stock_id = "gtk-open",
      #  tool_tip = "Open",
      #  call_back_fct = function(widget, event, data) {
      #    session_name <- data[[1]]
      #    outer_env <- data[[2]]

      #    utils::file.edit(totem$code_R)

      #    return(FALSE)
      #  }, data = list(session_name, outer_env)
      #)






      #u__button(
      #  box = outer_env[[session_name]]$data_view_list$file_source_bar,
      #  start = T, padding = 2,
      #  stock_id = "gtk-add",
      #  tool_tip = "Add session block to code.R",
      #  call_back_fct = function(widget, event, data) {
      #    session_name <- data[[1]]
      #    outer_env <- data[[2]]
      #    outer_env$u__code_r_add(session_name)
      #    utils::file.edit(totem$code_R)

      #    return(FALSE)
      #  }, data = list(session_name, outer_env)
      #)




      outer_env[[session_name]]$data_view_list$file_source_entry <- RGtk2::gtkEntry()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$file_source_bar, outer_env[[session_name]]$data_view_list$file_source_entry, T, T)
      RGtk2::gtkEntrySetText(outer_env[[session_name]]$data_view_list$file_source_entry, session_name)


      u__button(
        box = outer_env[[session_name]]$data_view_list$file_source_bar,
        start = T, padding = 2,
        but_txt = "grp",
        tool_tip = "Template group_by filter",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]

          outer_env$u__append_before_code(session_name, cmd = "df <- df %>% group_by(USUBJID) %>% filter(any(VAR == VAL))")

          return(FALSE)
        }, data = list(session_name, outer_env)
      )

      u__button(
        box = outer_env[[session_name]]$data_view_list$file_source_bar,
        start = T, padding = 2,
        but_txt = "new",
        tool_tip = "Template new column definition",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]

          outer_env$u__append_before_code(session_name, cmd = "df <- df %>% mutate(NEWCOL = ifelse(VAR == VAL, 1, 0))")

          return(FALSE)
        }, data = list(session_name, outer_env)
      )

      #u__button(
      #  box = outer_env[[session_name]]$data_view_list$file_source_bar,
      #  start = T, padding = 2,
      #  but_txt = "up",
      #  tool_tip = "Upper case columns",
      #  call_back_fct = function(widget, event, data) {
      #    session_name <- data[[1]]
      #    outer_env <- data[[2]]
      #
      #    outer_env$u__append_before_code(session_name, cmd = "colnames(df) <- toupper(colnames(df))")
      #
      #    return(FALSE)
      #  }, data = list(session_name, outer_env)
      #)

      u__button(
        box = outer_env[[session_name]]$data_view_list$file_source_bar,
        start = T, padding = 2,
        but_txt = "r__2",
        tool_tip = "Add fixed row number",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]

          outer_env$u__append_before_code(session_name, cmd = "df <- add_r__2(df, \"r__2\")")

          return(FALSE)
        }, data = list(session_name, outer_env)
      )

      u__button(
        box = outer_env[[session_name]]$data_view_list$file_source_bar,
        start = T, padding = 2,
        but_txt = "cc",
        tool_tip = "Add cross count",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]

          outer_env$u__append_before_code(session_name, cmd = "df$n__1 <- add_cross_counts(df, c('USUBJID', 'PARAM', 'AVISIT'))")
          
          return(FALSE)
        }, data = list(session_name, outer_env)
      )

      u__button(
        box = outer_env[[session_name]]$data_view_list$file_source_bar,
        start = T, padding = 2,
        but_txt = "time",
        tool_tip = "Print timeline to console",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          timeline <- data[[3]]
          #outer_env$timeline

          print("Printing timeline...")
          for (i in 1:length(timeline)) {
            print(paste0(i, ": ", timeline[i]))
          }
          print("Done")
          
          return(FALSE)
        }, data = list(session_name, outer_env, timeline)
      )







      u__button(
        box = outer_env[[session_name]]$data_view_list$code_tool_bar,
        start = T, padding = 2,
        stock_id = "gtk-delete",
        tool_tip = "Clear",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          u__text_area_clear(outer_env[[session_name]]$text_area_1)
          return(FALSE)
        }, data = list(session_name, outer_env)
      )
      
      u__button(
        box = outer_env[[session_name]]$data_view_list$code_tool_bar,
        start = T, padding = 2,
        stock_id = "gtk-close",
        tool_tip = "Clear and Run",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          outer_env$show_load_window()
          u__text_area_clear(outer_env[[session_name]]$text_area_1)
          outer_env$u__load_dataset_filter(session_name)
          outer_env$hide_load_window()
          return(FALSE)
        }, data = list(session_name, outer_env)
      )

      u__button(
        box = outer_env[[session_name]]$data_view_list$code_tool_bar,
        start = T, padding = 2,
        stock_id = "gtk-media-play",
        tool_tip = "Run code",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          outer_env$show_load_window()
          outer_env$u__load_dataset_filter(session_name)
          outer_env$hide_load_window()
          return(FALSE)
        }, data = list(session_name, outer_env)
      )



      u__button(
        box = outer_env[[session_name]]$data_view_list$code_tool_bar,
        start = T, padding = 2,
        stock_id = "gtk-copy",
        tool_tip = "Copy",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          x <- u__text_area_get_text(outer_env[[session_name]]$text_area_1)
          clipr::write_clip(x, allow_non_interactive = T)
          return(FALSE)
        }, data = list(session_name, outer_env)
      )


      u__button(
        box = outer_env[[session_name]]$data_view_list$code_tool_bar,
        start = T, padding = 2,
        stock_id = "gtk-paste",
        tool_tip = "Paste",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          x <- clipr::read_clip(allow_non_interactive = T)

          u__text_area_append_text(outer_env[[session_name]]$text_area_1, paste0(x, collapse = "\n"))

          return(FALSE)
        }, data = list(session_name, outer_env)
      )





      outer_env[[session_name]]$data_view_list$code_tool_bar_dim_label <- RGtk2::gtkLabel()
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$code_tool_bar, outer_env[[session_name]]$data_view_list$code_tool_bar_dim_label, F, F, padding = 1)

      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$code_tool_bar, outer_env[[session_name]]$data_view_list$select_box, T, T, padding = 1)

      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$code_tool_bar2, outer_env[[session_name]]$data_view_list$group_by_box, T, T, padding = 1)
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$data_view_list$code_tool_bar2, outer_env[[session_name]]$data_view_list$unique_by_box, T, T, padding = 1)



      #u__button(
      #  box = outer_env[[session_name]]$data_view_list$code_tool_bar,
      #  start = F, padding = 2,
      #  stock_id = "gtk-media-play",
      #  tool_tip = "Run code",
      #  call_back_fct = function(widget, event, data) {
      #    session_name <- data[[1]]
      #    outer_env <- data[[2]]
      #    outer_env$show_load_window()
      #    outer_env$u__load_dataset_filter(session_name)
      #    outer_env$hide_load_window()
      #    return(FALSE)
      #  }, data = list(session_name, outer_env)
      #)

      #u__button(
      #  box = outer_env[[session_name]]$data_view_list$code_tool_bar,
      #  start = F, padding = 2,
      #  stock_id = "gtk-close",
      #  tool_tip = "Clear",
      #  call_back_fct = function(widget, event, data) {
      #    session_name <- data[[1]]
      #    outer_env <- data[[2]]
      #    outer_env$show_load_window()
      #    u__text_area_clear(outer_env[[session_name]]$text_area_1)
      #    outer_env$u__load_dataset_filter(session_name)
      #    outer_env$hide_load_window()
      #    return(FALSE)
      #  }, data = list(session_name, outer_env)
      #)


      u__button(
        box = outer_env[[session_name]]$data_view_list$code_tool_bar,
        start = F, padding = 2,
        stock_id = "gtk-page-setup",
        tool_tip = "Previous code",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          outer_env[[session_name]]$show_past_code_window(session_name)
          return(FALSE)
        }, data = list(session_name, outer_env)
      )

      u__button(
        box = outer_env[[session_name]]$data_view_list$code_tool_bar,
        start = F, padding = 2,
        stock_id = "gtk-print-preview",
        tool_tip = "View Dataset",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          outer_env$u__df_view(outer_env[[session_name]]$data2, as.character(Sys.time()), height = 500, width = 500)
          return(FALSE)
        }, data = list(session_name, outer_env)
      )







      outer_env[[session_name]]$data_view_list$slot1_list <- list()

      outer_env[[session_name]]$data_view_list$slot1_list$meta_table <- build_meta_data(session_name)




      outer_env[[session_name]]$data_view_list$slot1_list$full_table <- build_full_data(session_name)



      #----------------------------------------

      # data view: slot2

      #----------------------------------------


      outer_env[[session_name]]$data_view_list$slot2_list <- list()



      build_value_data <- function(session_name, outer_env = totem) {
        return(outer_env$u__df_tree(
          session_name = session_name,
          passed_box = outer_env[[session_name]]$data_view_list$slot2_box,
          rows_length = 1000,
          event_mapping = NULL,
          style_list = list(value = RGtk2::pangoFontDescriptionFromString("bold 10")),
          is_value_table = T, is_meta_table = F, is_data_code_table = F
        ))
      }



      outer_env[[session_name]]$data_view_list$slot2_list$value_table <- build_value_data(session_name)




      #----------------------------------------

      # status bar

      #----------------------------------------






      outer_env[[session_name]]$status_bar <- list()

      outer_env[[session_name]]$status_bar$frame <- RGtk2::gtkFrame()
      outer_env[[session_name]]$status_bar$vbox <- RGtk2::gtkVBox()
      outer_env[[session_name]]$status_bar$box <- RGtk2::gtkHBox()
      outer_env[[session_name]]$status_bar$box_bucket <- RGtk2::gtkHBox()
      outer_env[[session_name]]$status_bar$box_bucket_showing <- F
      outer_env[[session_name]]$status_bar$simplicity_view <- totem$settings_list$simplicity
      outer_env[[session_name]]$status_bar$info_label <- RGtk2::gtkLabel("")
      outer_env[[session_name]]$status_bar$info_label_cell <- RGtk2::gtkLabel("")

      RGtk2::gtkContainerAdd(outer_env[[session_name]]$status_bar$frame, outer_env[[session_name]]$status_bar$vbox)

      RGtk2::gtkBoxPackStart(
        outer_env[[session_name]]$status_bar$vbox, outer_env[[session_name]]$status_bar$box,
        F, F
      )

      RGtk2::gtkBoxPackStart(
        outer_env[[session_name]]$status_bar$vbox, outer_env[[session_name]]$status_bar$box_bucket,
        F, F
      )
      RGtk2::gtkWidgetHide(outer_env[[session_name]]$status_bar$box_bucket)


      outer_env[[session_name]]$status_bar$box_bucket_entry <- RGtk2::gtkEntry()



      RGtk2::gtkEntrySetText(outer_env[[session_name]]$status_bar$box_bucket_entry, "")

      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$status_bar$box_bucket, RGtk2::gtkLabel("Bucket: "), F, F, padding = 2)

      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$status_bar$box_bucket, outer_env[[session_name]]$status_bar$box_bucket_entry, T, T)



      u__button(
        box = outer_env[[session_name]]$status_bar$box_bucket,
        start = F, padding = 5,
        stock_id = "gtk-clear",
        tool_tip = "Clear bucket",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$status_bar$box_bucket_entry, "")

          return(FALSE)
        },
        data = list(session_name, outer_env)
      )

      u__button(
        box = outer_env[[session_name]]$status_bar$box_bucket,
        start = F, padding = 5,
        stock_id = "gtk-goto-top",
        tool_tip = "Variable bucket hide",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          RGtk2::gtkWidgetHide(outer_env[[session_name]]$status_bar$box_bucket)
          outer_env[[session_name]]$status_bar$box_bucket_showing <- F
          RGtk2::gtkEntrySetText(outer_env[[session_name]]$status_bar$box_bucket_entry, "")

          return(FALSE)
        },
        data = list(session_name, outer_env)
      )

      #Activate simplicity view according to settings.rds
      if (outer_env[[session_name]]$status_bar$simplicity_view == T) {
        RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$top_code_box)
        RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$top_tables_box)
        outer_env[[session_name]]$status_bar$simplicity_view <- T
      }
      
      #JN: simplicity mode that will hide everything but Full Data Table     
      u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = F, padding = 5,
        stock_id = "gtk-fullscreen",
        tool_tip = "Toggle simplicity mode",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]

          #Hide top boxes if simplicity view is off, enable simplicity view
          if (outer_env[[session_name]]$status_bar$simplicity_view == F) {
            RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$top_code_box)
            RGtk2::gtkWidgetHide(outer_env[[session_name]]$data_view_list$top_tables_box)
            outer_env[[session_name]]$status_bar$simplicity_view <- T
          } 
          #Otherwise show top boxes, disable simplicity fiew
          else {
            RGtk2::gtkWidgetShow(outer_env[[session_name]]$data_view_list$top_code_box)
            RGtk2::gtkWidgetShow(outer_env[[session_name]]$data_view_list$top_tables_box)
            outer_env[[session_name]]$status_bar$simplicity_view <- F
          }

          return(FALSE)
        },
        data = list(session_name, outer_env)
      )










      RGtk2::gtkBoxPackEnd(
        outer_env[[session_name]]$main$main_box, outer_env[[session_name]]$status_bar$frame,
        F, F
      )



      u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = F, padding = 5,
        stock_id = "gtk-add",
        tool_tip = "Open new session.",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          outer_env$show_file_history_window()
          return(FALSE)
        },
        data = list(session_name, outer_env)
      )



      u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = F, padding = 5,
        stock_id = "gtk-preferences",
        tool_tip = "User Settings",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          outer_env$show_settings_window()

          return(FALSE)
        },
        data = list(session_name, outer_env)
      )


      u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = F, padding = 5,
        stock_id = "gtk-goto-bottom",
        tool_tip = "Variable bucket show",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          RGtk2::gtkWidgetShow(outer_env[[session_name]]$status_bar$box_bucket)
          outer_env[[session_name]]$status_bar$box_bucket_showing <- T


          return(FALSE)
        },
        data = list(session_name, outer_env)
      )





      # u__button(
      #   box = outer_env[[session_name]]$status_bar$box,
      #   start = F, padding = 5,
      #   stock_id = "gtk-media-play",
      #   tool_tip = "Run code",
      #   call_back_fct = function(widget, event, data) {
      #     session_name <- data[[1]]
      #     outer_env <- data[[2]]
      #     outer_env$show_load_window()
      #     outer_env$u__load_dataset_filter(session_name)
      #     outer_env$hide_load_window()
      #     return(FALSE)
      #   }, data = list(session_name, outer_env)
      # )


      # u__button(
      #   box = outer_env[[session_name]]$status_bar$box,
      #   start = F, padding = 5,
      #   stock_id = "gtk-close",
      #   tool_tip = "Clear",
      #   call_back_fct = function(widget, event, data) {
      #     session_name <- data[[1]]
      #     outer_env <- data[[2]]
      #     outer_env$show_load_window()
      #     u__text_area_clear(outer_env[[session_name]]$text_area_1)
      #     outer_env$u__load_dataset_filter(session_name)
      #     outer_env$hide_load_window()
      #     return(FALSE)
      #   }, data = list(session_name, outer_env)
      # )



      u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = T, padding = 3,
        stock_id = "gtk-refresh",
        tool_tip = "Reload dataset",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          refresh(session_name)
          return(FALSE)
        },
        data = list(session_name, outer_env)
      )


            u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = T, padding = 3,
        stock_id = "gtk-help",
        tool_tip = "Get help",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          utils::browseURL(url="https://wiki.internal.pentara.com/collection/jaw-YNNCF6rIE0/recent")
          return(FALSE)
        },
        data = list(session_name, outer_env)
      )


                  u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = T, padding = 3,
        stock_id = "gtk-dialog-question",
        tool_tip = "Suggest features",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          utils::browseURL(url="https://wiki.internal.pentara.com/doc/future-features-nm6Ur9fdWZ")
          return(FALSE)
        },
        data = list(session_name, outer_env)
      )






      outer_env[[session_name]]$export_name_entry <- RGtk2::gtkEntry()

      export_name <- make.names(gsub(paste0("\\.",outer_env[[session_name]]$passed_ext), "", outer_env[[session_name]]$sas_file_basename))


      RGtk2::gtkEntrySetText(outer_env[[session_name]]$export_name_entry, export_name)

      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$status_bar$box, outer_env[[session_name]]$export_name_entry, F, F)


      u__button(
        box = outer_env[[session_name]]$status_bar$box,
        start = T, padding = 3,
        stock_id = "gtk-harddisk",
        tool_tip = "Write to .GlobalEnv",
        call_back_fct = function(widget, event, data) {
          session_name <- data[[1]]
          outer_env <- data[[2]]
          assign_env <- data[[3]]
          export_name <- RGtk2::gtkEntryGetText(outer_env[[session_name]]$export_name_entry)

          assign(export_name, value = outer_env[[session_name]]$data2, envir = assign_env)
          message("Exported data")
          return(FALSE)
        },
        data = list(session_name, outer_env,assign_env)
      )



      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$status_bar$box, RGtk2::gtkLabel("Format by: "), F, F, padding = 2)




      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$status_bar$box, outer_env[[session_name]]$format_by_entry, F, F)



      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$status_bar$box, outer_env[[session_name]]$status_bar$info_label, F, F, padding = 2)
      RGtk2::gtkBoxPackStart(outer_env[[session_name]]$status_bar$box, outer_env[[session_name]]$status_bar$info_label_cell, F, F, padding = 2)




















      #----------------------------------------

      # tail

      #----------------------------------------






      refresh <- function(session_name, outer_env = totem) {
        outer_env$show_load_window()

        #Update version number here with substantial updates
        title <- paste0(
          gsub(paste0("\\.",outer_env[[session_name]]$passed_ext), "", outer_env[[session_name]]$sas_file_basename),
          " | ", outer_env[[session_name]]$sas_file_path, " | ", "Ver 1.0.5.4", " | ", as.character(Sys.time())
        )
        RGtk2::gtkWindowSetTitle(outer_env[[session_name]]$windows$main_window, title)


        outer_env$u__load_dataset(session_name)

        outer_env[[session_name]]$objects$current_view <- outer_env[[session_name]]$objects$next_view
        outer_env$hide_load_window()
      }
    },
    error = function(e) {


      #
      # gSourceRemove(totem$loop_function_obj)
      # totem$loop_function_obj <- F
    }
  )
  refresh(session_name)
  RGtk2::gtkWidgetShow(outer_env[[session_name]]$windows$main_window)
  outer_env$hide_load_window()
} # End of start function
