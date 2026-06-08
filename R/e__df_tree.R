#' e__df_tree
#'
#' @param session_name TODO
#' @param passed_box TODO
#' @param rows_length TODO
#' @param event_mapping TODO
#' @param style_list TODO
#' @param is_value_table TODO
#' @param is_meta_table TODO
#' @param is_data_code_table TODO
#' @param is_file_history_table TODO
#' @param is_full_data_table TODO
#' @param outer_env TODO
#'
#' @return TODO

e__df_tree <- function(session_name, passed_box, rows_length, event_mapping = NULL,
                       style_list = NULL,
                       is_value_table = F, is_meta_table = F, is_data_code_table = F, is_file_history_table = F, is_full_data_table = F, outer_env = totem) {
  inner_env <- new.env()

  box <- RGtk2::gtkVBox()

  RGtk2::gtkBoxPackStart(passed_box, box, F, F)


  color_header_1 <- "#00ffff"
  color_bg_1 <- "#ff00ff"
  color_bg_2 <- "#ffff00"
  color_sep_1 <- "#00ff00"


  if (is.null(event_mapping)) {
    event_mapping <- list()
  }
  if (is.null(style_list)) {
    style_list <- list()
  }



  #########################
  # Helpers & objects
  ##########################

  inner_env$table_cell_events <- e__with_env(e__table_cell_events)
  inner_env$tree_view_column_btn_press <- e__with_env(e__tree_view_column_btn_press)


  inner_env$new_tree_view_column <- e__with_env(e__new_tree_view_column)
  inner_env$add_column_label <- e__with_env(e__add_column_label)

  page_setup <- e__with_env(e__page_setup)
  filter_setup <- e__with_env(e__filter_setup)
  order_by_setup <- e__with_env(e__order_by_setup)
  select_setup <- e__with_env(e__select_setup)

  inner_env$table_obj_function_df2 <- e__with_env(e__table_obj_function_df2)
  inner_env$table_obj_function <- e__with_env(e__table_obj_function)


  inner_env$generic_filter_function <- e__with_env(e__generic_filter_function)
  inner_env$df_obj_function <- e__with_env(e__df_obj_function)





  #########################
  #
  # Menu
  #
  ##########################

  settings_config <- outer_env$settings_list$table_events


  possible_types <- c("General", "Copy")


  if (is_meta_table) {
    possible_types <- c(possible_types, "Meta Table")
  } else if (is_full_data_table) {
    possible_types <- c(possible_types, "Full Data Table")
  } else if (is_value_table) {
    possible_types <- c(possible_types, "Summary Table")
  } else if (is_data_code_table) {
    possible_types <- c(possible_types, "Past Code Table")
  } else if (is_file_history_table) {
    possible_types <- c(possible_types, "File History Table")
  }


  u__menubar_settings <- list()
  for (config_i in names(settings_config)) {
    if (config_i %in% possible_types) {
      u__menubar_settings[[config_i]] <- list()
      for (item_i in names(settings_config[[config_i]])) {
        u__menubar_settings[[config_i]][[item_i]] <- c("temp")
      }
    }
  }

  u__menubar <- list()
  u__menubar$items <- list()
  u__menubar[["base"]] <- RGtk2::gtkMenu()
  u__menubar$end_nodes <- c()

  # if (exists("pop_up") == F) {
  #   pop_up <- list()
  # }




  u__menubar_settings_map <- list()
  for (my_name in names(u__menubar_settings)) {
    u__menubar_settings_map[[my_name]] <- names(u__menubar_settings[[my_name]])
  }



  inner_env$menubar <- z__create_menu_from_list(
    u__menubar,
    "base", u__menubar_settings_map
  )


  all_menu_events <- e__with_env(e__all_menu_events)


  for (config_i in names(settings_config)) {
    if (config_i %in% possible_types) {

      for (item_i in names(settings_config[[config_i]])) {
        end_node <- paste0("base|",config_i,"|",item_i)
         RGtk2::gSignalConnect(inner_env$menubar$item[[end_node]], "activate", all_menu_events,
    data = list(config_i,item_i, outer_env, inner_env, session_name,event_mapping,NULL))
      }
    }
  }






  #########################
  #
  # Create Objects
  #
  ##########################

  top_box <- RGtk2::gtkHBox()
  top_box_right <- RGtk2::gtkVBox()
  header_table0 <- RGtk2::gtkTableNew(rows = 4, columns = 20, homogeneous = F)
  header_table <- RGtk2::gtkTableNew(rows = 4, columns = 20, homogeneous = F)
  RGtk2::gtkBoxPackStart(box, top_box, F, F, padding = 2)

  inner_env$hidden_table <- T

  # --- NEW: Refresh Button specifically for Past Code Window ---
  if (is_data_code_table) {
    u__button(
      box = top_box,
      start = T, padding = 1,
      stock_id = "gtk-refresh",
      tool_tip = "Sync and refresh past code from all sessions",
      call_back_fct = function(widget, event, data) {
        outer_env <- data[[1]]
        inner_env <- data[[2]]
        
        # 1. Pull the latest code history from the disk
        try({
          disk_settings <- readRDS(outer_env$local_settings_rds)
          if (is.list(disk_settings) && !is.null(disk_settings$previous_code)) {
            merged_code <- rbind(outer_env$settings_list$previous_code, disk_settings$previous_code)
            
            # 2. Sort descending by time to keep newest code at the top
            merged_code <- merged_code[order(merged_code$time, decreasing = TRUE), ]
            merged_code <- merged_code[!duplicated(merged_code[, -1]), ]
            
            # 3. Cap at 500 and save to the active environment
            outer_env$settings_list$previous_code <- head(merged_code, 500)
            
            # --- NEW: Save the synchronized state back to disk ---
            save_settings(outer_env)
          }
        }, silent = TRUE)
        
        # 4. Force the table UI to update with the new data
        if (!is.null(inner_env$df_obj)) {
            inner_env$df_obj$call_generate_full_df(outer_env$settings_list$previous_code)
        }
        
        return(FALSE)
      },
      data = list(outer_env, inner_env)
    )
  }

  u__button(
    box = top_box,
    start = T, padding = 1,
    stock_id = "gtk-leave-fullscreen",
    tool_tip = "Show Header",
    call_back_fct = function(widget, event, data) {
      if (inner_env$hidden_table) {
        RGtk2::gtkWidgetShow(header_table)
        inner_env$hidden_table <- F
      } else {
        RGtk2::gtkWidgetHide(header_table)
        inner_env$hidden_table <- T
      }

      return(T)
    },
    data = NULL
  )


  dim_label <- RGtk2::gtkLabel("")
  RGtk2::gtkBoxPackStart(top_box, dim_label, F, F, padding = 2)
  RGtk2::gtkBoxPackStart(top_box, top_box_right, T, T, padding = 2)
  RGtk2::gtkBoxPackStart(top_box_right, header_table0, T, T, padding = 2)
  RGtk2::gtkBoxPackStart(top_box_right, header_table, T, T, padding = 2)
  RGtk2::gtkWidgetHide(header_table)


  inner_env$page_obj <- page_setup(header_table0, 0)

  inner_env$filter_obj <- filter_setup(header_table, 1)
  inner_env$order_by_obj <- order_by_setup(header_table, 2)
  if (is_full_data_table) {
    inner_env$select_obj <- select_setup(header_table, 3)
  }
  inner_env$df_obj <- inner_env$df_obj_function(passed_box)


  #########################
  #
  # Tail
  #
  ##########################

  update <- function(df) {
    inner_env$df_obj$call_generate_full_df(df)
    return(T)
  }
  return(list(update = update))
}
