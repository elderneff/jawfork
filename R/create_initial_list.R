
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
  jaw_e$jaw_log_path <- file.path(jaw_e$settings_dir_path, "jaw_log.txt")
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



  #logger("", jaw_e, append = F)


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
      "Cell value" = "middle+none"
    ),
    "Meta Table" = list(
      "Trigger Value Summary" = "left+none",
      "Trigger Value Summary with Group By" = "left+ctrl"

    ),
    "Full Data Table" = list(
      "Trigger Value Summary" = "left+none",
      "Trigger Value Summary with Group By" = "left+ctrl",
      "Add to Main Filter" = "right+ctrl",
      "Add to Main Filter Exclude" = "right+ctrl+shift",
      "Get Summary" = "middle+ctrl"
    ),
    "Summary Table" = list(
      #"Open Flat View" = "left+alt",
      #"Open Inverted View" = "left+ctrl",
      "Add to Main Filter" = "right+ctrl",
      "Add to Main Filter Exclude" = "right+ctrl+shift"
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
  }


  if (("file_history" %in% names(settings)) == F) {
    settings$file_history <- data.frame(
      "latest" = logical(), "mtime" = character(),
      "load_time" = character(),
      "dataset" = character(),
      "full_path" = character(),
      stringsAsFactors = FALSE
    )
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



  return(settings)
}
