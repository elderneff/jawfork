
#' create_initial_list
#'
#' @param settings_dir TODO
#'
#' @return TODO

create_initial_list <- function(settings_dir=NULL) {
  jaw_e <- new.env()
  jaw_e$all_sessions <- c()

  if (is.null(settings_dir)==F) {

    jaw_e$using_temp_dir <- F
  } else {
    settings_dir <- tempdir()
    jaw_e$using_temp_dir <- T
    jaw_e$temp_path <- settings_dir
  }



  jaw_e$settings_dir_path <- file.path(settings_dir, "jaw")
  jaw_e$local_settings_rds <- file.path(jaw_e$settings_dir_path, "settings.rds")
  jaw_e$code_R <- file.path(jaw_e$settings_dir_path, "code.R")
  jaw_e$settings_list <- create_file_structure(jaw_e)
  return(jaw_e)
}


#' create_file_structure
#'
#' @param jaw_e TODO
#'
#' @return TODO

create_file_structure <- function(jaw_e) {
  if (dir.exists(jaw_e$settings_dir_path) == F) {
    dir.create(path = jaw_e$settings_dir_path, showWarnings = TRUE, recursive = T)
  }

  #Clear pinned comparison file on startup to prevent stale data
  pinned_path <- file.path(jaw_e$settings_dir_path, "pinned_comparison.rds")
  if (file.exists(pinned_path)) {
    unlink(pinned_path)
  }

  logger("", jaw_e, append = F)

  if (file.exists(jaw_e$local_settings_rds) == F) {
    saveRDS(list(), file = jaw_e$local_settings_rds)
  }

  #Write setting corruption status to global environment
  try_settings <- try(readRDS(file = jaw_e$local_settings_rds))
  assign("try_settings", try_settings, envir = .GlobalEnv)  
  #Reset settings if they cannot be read in
  if (class(try_settings) == "try-error") {
    saveRDS(list(), file = jaw_e$local_settings_rds)
  }  
  settings <- readRDS(file = jaw_e$local_settings_rds)

  if (file.exists(jaw_e$code_R) == F) {
    cat("", file = jaw_e$code_R)
  }



  return(check_settings(settings))
}

#' save_settings
#'
#' @param jaw_e TODO
#'
#' @return TODO

save_settings <- function(jaw_e) {
  if (jaw_e$using_temp_dir) {
    message(paste0("Removed: ", jaw_e$settings_dir_path))
    unlink(jaw_e$settings_dir_path, recursive = T)
  } else {
    saveRDS(jaw_e$settings_list, file = jaw_e$local_settings_rds)
  }
}




#' check_settings
#'
#' @param settings TODO
#'
#' @return TODO

check_settings <- function(settings) {

  all_items <- e__all_event_functions()


  settings$default_table_events <- list(
    "General" = list(

      #"View" = "right+alt",
      #"Add to filter" = "right+ctrl",
      #"Add to arrange" = "right+shift",
      "Open Context Menu" = "right+none"
    ),
    "Copy" = list(
      "Cell value" = "middle+none",
      "Column Name" = "-",
      "Column=Cell" = "-",
      "if then" = "-",
      "if then do" = "-",
      "Table full" = "-",
      "Table full to file" = "-",
      "Table filtered" = "-",
      "Column full" = "-",
      "Column filtered" = "-",
      "Column Wide" = "-",
      "Vector Column full" = "-",
      "Vector Column filtered" = "-",
      "Row" = "-"
    ),
    "Meta Table" = list(
      "Trigger Value Summary" = "left+none",
      "Trigger Value Summary with Group By" = "left+ctrl",
      "Trigger Value Summary with Unique By" = "left+alt"
    ),
    "Full Data Table" = list(
      "Trigger Value Summary" = "left+none",
      "Trigger Value Summary with Group By" = "left+ctrl",
      "Trigger Value Summary with Unique By" = "left+alt",
      "Add to Main Filter" = "right+ctrl",
      "Add to Main Filter Exclude" = "right+ctrl+shift",
      "Add to Main Filter (no combining)" = "-",
      "Get Summary" = "middle+ctrl"
    ),
    "Summary Table" = list(
      #"Open Flat View" = "left+alt",
      #"Open Inverted View" = "left+ctrl",
      "Add to Main Filter" = "right+ctrl",
      "Add to Main Filter Exclude" = "right+ctrl+shift",
      "Add to Main Filter (no combining)" = "-"
    ),
    "Past Code Table" = list(
      "Load Code" = "left+none"
    ),
    "File History Table" = list(
      "New Session" = "left+none"
    )
  )


  for (config_i in names(all_items)) {
    for (item_i in names(all_items[[config_i]])) {
      if((item_i %in%  names(settings$default_table_events[[config_i]]))==F){
        settings$default_table_events[[config_i]][[item_i]] <- "-"
      }
    }
    for (item_i in names(settings$default_table_events[[config_i]])) {
      if ((item_i %in% names(settings$table_events[[config_i]])) == F) {
        settings$table_events[[config_i]][[item_i]] <- settings$default_table_events[[config_i]][[item_i]]
      }
    }
    # Force the active settings list to inherit the exact order of the default settings list
    settings$table_events[[config_i]] <- settings$table_events[[config_i]][names(settings$default_table_events[[config_i]])]
  }


  if (("table_events" %in% names(settings)) == F) {
    settings$table_events <- settings$default_table_events
  } else {
    for (config_i in names(settings$default_table_events)) {
      if ((config_i %in% names(settings$table_events)) == F) {
        settings$table_events[[config_i]] <- list()
      }


      for (item_i in names(settings$default_table_events[[config_i]])) {
        if ((item_i %in% names(settings$table_events[[config_i]])) == F) {
          settings$table_events[[config_i]][[item_i]] <- settings$default_table_events[[config_i]][[item_i]]
        }
      }
    }
  }



  if (("previous_code" %in% names(settings)) == F) {
    settings$previous_code <- data.frame(
      "time" = character(),
      "dataset" = character(),
      "code" = character(),
      "full_path" = character(),
      stringsAsFactors = FALSE
    )
  } else {
    # HEAL EXISTING DATA: Sort descending by time to fix any corrupted orders from old Jaw versions
    if (nrow(settings$previous_code) > 0) {
      settings$previous_code <- settings$previous_code[order(settings$previous_code$time, decreasing = TRUE), ]
      
      # Apply the 500 cap here to instantly clean up bloated files on startup!
      settings$previous_code <- head(settings$previous_code, 500)
    }
  }


  if (("file_history" %in% names(settings)) == F) {
    # You can set your initial creation order here
    settings$file_history <- data.frame(
      "dataset" = character(), "latest" = logical(), 
      "loaded" = character(), "modified" = character(),
      "path" = character(),
      stringsAsFactors = FALSE
    )
  } else {
    # Dynamically migrate old column names for existing users
    cols <- colnames(settings$file_history)
    cols[cols == "mtime"] <- "modified"
    cols[cols == "load_time"] <- "loaded"
    cols[cols == "full_path"] <- "path"
    colnames(settings$file_history) <- cols
    
    # FORCE THE DESIRED COLUMN ORDER
    settings$file_history <- settings$file_history[, c("dataset", "latest", "loaded", "modified", "path")]
  }

  #Default maximize to T if there is no previous setting
  if (("maximize" %in% names(settings)) == F) {
    settings$maximize <- T
  }
  #Default simplicity to F if there is no previous setting
  if (("simplicity" %in% names(settings)) == F) {
    settings$simplicity <- F
  }
  #Default Ctrl+Shift to T if there is no previous setting
  if (("ctrlshift" %in% names(settings)) == F) {
    settings$ctrlshift <- T
  }
  #Default columns labels to T if there is no previous setting
  if (("columnlabels" %in% names(settings)) == F) {
    settings$columnlabels <- T
  }
  #Default column unique values to T if there is no previous setting
  if (("columnunique" %in% names(settings)) == F) {
    settings$columnunique <- T
  }
  #Default professional loading to F if there is no previous setting
  if (("professionalloading" %in% names(settings)) == F) {
    settings$professionalloading <- F
  }
  # Default dark_mode to F if there is no previous setting
  if (("dark_mode" %in% names(settings)) == F) {
    settings$dark_mode <- F
  }
  #Default show tooltips to T if there is no previous setting
  if (("show_tooltips" %in% names(settings)) == F) {
    settings$show_tooltips <- T
  }
  #Default show copy messages to T if there is no previous setting
  if (("copy_messages" %in% names(settings)) == F) {
    settings$copy_messages <- T
  }
  # Default code generation preferences to "Prompt" if there is no previous setting
  if (("code_case" %in% names(settings)) == F) {
    settings$code_case <- "Prompt"
  }
  if (("code_spacing" %in% names(settings)) == F) {
    settings$code_spacing <- "Prompt"
  }
  # Default custom code slots to list of name/code pairs
  if (("custom_code_slots" %in% names(settings)) == F) {
    settings$custom_code_slots <- list(
      list(name = "Slot 1", code = ""),
      list(name = "Slot 2", code = ""),
      list(name = "Slot 3", code = ""),
      list(name = "Slot 4", code = ""),
      list(name = "Slot 5", code = "")
    )
  }

  default_sizes <- list(window = c(864 + 50, 698), main_pane = 268, top_pane = 85 + 30, slot_pane = 417)


  if (("default_sizes" %in% names(settings)) == F) {
    settings$default_sizes <- default_sizes
  } else {
    for (config_i in names(default_sizes)) {
      if ((config_i %in% names(settings$default_sizes)) == F) {
        settings$default_sizes[[config_i]] <- default_sizes[[config_i]]
      }
    }
  }

  # Default update tracking date
  if (("last_update_check" %in% names(settings)) == F) {
    settings$last_update_check <- "1970-01-01"
  }

  return(settings)
}
