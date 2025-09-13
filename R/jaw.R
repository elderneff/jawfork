#' jaw
#'
#' @param settings_dir TODO
#'
#' @return TODO
#' @export

jaw <- function(settings_dir=NULL) {
  debug <- T

  color_bg_1 <- "#FFFFFF"
  color_bg_2 <- "#f9f9f9"
  totem <- create_initial_list(settings_dir)
  totem$while_loop_running <- F
  # Events
  totem$all_event_functions <- (e__with_env(e__all_event_functions))()

  # Actions
  totem$copy_if_then <- e__with_env(e__copy_if_then)
  totem$copy_if_then_do <- e__with_env(e__copy_if_then_do)
  totem$move_column <- e__with_env(e__move_column)

  totem$add_session <- e__with_env(e__add_session)
  totem$close_all_windows <- e__with_env(e__close_all_windows)

  totem$u__add_before_filter_full_data_bucket <- e__with_env(e__add_before_filter_full_data_bucket)
  totem$u__add_before_filter_full_data_column <- e__with_env(e__add_before_filter_full_data_column)
  totem$u__add_before_filter_full_data <- e__with_env(e__add_before_filter_full_data)
  totem$u__add_before_filter <- e__with_env(e__add_before_filter)
  totem$u__add_count_to_df_summary <- e__with_env(e__add_count_to_df_summary)
  totem$u__get_summary <- e__with_env(e__get_summary)
  totem$u__graph_summary <- e__with_env(e__graph_summary)
  totem$u__append_before_code <- e__with_env(e__append_before_code)
  totem$u__set_before_code <- e__with_env(e__set_before_code)



  totem$u__load_dataset <- e__with_env(e__load_dataset)
  totem$u__load_dataset_filter <- e__with_env(e__load_dataset_filter)
  totem$u__load_dataset_filter_inner_select <- e__with_env(e__load_dataset_filter_inner_select)
  totem$u__load_dataset_filter_inner <- e__with_env(e__load_dataset_filter_inner)


  totem$u__code_r_add <- e__with_env(e__code_r_add)
  totem$u__code_r_add_cmd <- e__with_env(e__code_r_add_cmd)

  # Objects

  totem$u__df_tree <- e__with_env(e__df_tree)
  totem$u__flat_view <- e__with_env(e__flat_view)
  totem$u__get_summary <- e__with_env(e__get_summary)
  totem$u__graph_summary <- e__with_env(e__graph_summary)
  totem$u__inverted_view <- e__with_env(e__inverted_view)
  totem$u__df_view <- e__with_env(e__df_view)


  # Create Objects

  e__with_env(e__create_settings)()
  e__with_env(e__file_history)()

  # Show and Hide

  totem$load_window <- RGtk2::gtkWindow(show = F)
  RGtk2::gtkWindowSetModal(totem$load_window, T)

  #Define a timeline and time to be referenced and edited by the text area
  timeline <- c()
  time <- 0
  
  #Pick a random loading gif for this session
  rand <- floor(runif(1, 1, 72))
  #Set rare Bob numbers for use elsewhere
  party_rand <- 1
  left_rand <- 19
  evil_rand <- 37
  blurry_rand <- 55
  #if (outer_env$settings_list$professionalloading) {
  #  RGtk2::gtkContainerAdd(totem$load_window, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "professional_loading.gif")))
  #}
  #else {
  RGtk2::gtkContainerAdd(totem$load_window_prof, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "professional_loading.gif")))
    if (rand == party_rand) {
      RGtk2::gtkContainerAdd(totem$load_window, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "party_loading.gif")))
    } else if (rand == left_rand) {
      RGtk2::gtkContainerAdd(totem$load_window, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "left_loading.gif")))
    } else if (rand == evil_rand) {
      RGtk2::gtkContainerAdd(totem$load_window, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "evil_loading.gif")))
    } else if (rand == blurry_rand) {
      RGtk2::gtkContainerAdd(totem$load_window, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "blurry_loading.gif")))
    } else {
      RGtk2::gtkContainerAdd(totem$load_window, RGtk2::gtkImageNewFromFile(file.path(system.file("images", package = "jaw"), "loading.gif")))
    }
  #}
  RGtk2::gtkWindowSetDecorated(totem$load_window, F)

  totem$hide_load_window <- function(outer_env = totem) {
    #RGtk2::gtkWidgetHide(outer_env$load_window)
    #if (outer_env$settings_list$professionalloading) {
    #  RGtk2::gtkWidgetHide(outer_env$load_window_prof)
    #} else {
      RGtk2::gtkWidgetHide(outer_env$load_window)
    #}
  }

  totem$show_load_window <- function(outer_env = totem) {
    #if (outer_env$settings_list$professionalloading) {
    #  RGtk2::gtkWidgetShow(outer_env$load_window_prof)
    #} else {
      RGtk2::gtkWidgetShow(outer_env$load_window)
    #}
  }

  totem$hide_settings_window <- function(outer_env = totem) {
    RGtk2::gtkWidgetHide(outer_env$settings_window$settings_window)
  }

  totem$show_settings_window <- function(outer_env = totem) {
    RGtk2::gtkWidgetShow(outer_env$settings_window$settings_window)
  }

  totem$hide_file_history_window <- function(outer_env = totem) {
    RGtk2::gtkWidgetHide(outer_env$file_history$file_history_window)
  }

  totem$show_file_history_window <- function(outer_env = totem) {
    RGtk2::gtkEntrySetText(outer_env$file_history$file_history_window_main_new_path_entry, "")
    outer_env$file_history$file_history_window_table$update(outer_env$settings_list$file_history)
    RGtk2::gtkWidgetShow(outer_env$file_history$file_history_window)
  }



  totem$start <- e__with_env(e__start)
  stop <- e__with_env(e__stop)
  block <- e__with_env(e__block)
  io_window_show <- function(outer_env = totem) {
    outer_env$show_file_history_window()
  }
  return(
    list(
      start = totem$start,
      stop = stop,
      block = block,
      io_window_show = io_window_show
    )
  )
}
