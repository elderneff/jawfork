#' e__create_settings
#'
#' @param outer_env TODO
#'
#' @return TODO

e__create_settings <- function(outer_env = totem) {
  outer_env$settings_window <- list()
  outer_env$settings_window$settings_window <- RGtk2::gtkWindow(show = F)


  outer_env$settings_window$settings_window_sw <- RGtk2::gtkScrolledWindow()

  RGtk2::gtkScrolledWindowSetPolicy(outer_env$settings_window$settings_window_sw, "automatic", "automatic")

  outer_env$settings_window$settings_window_main_box <- RGtk2::gtkVBox()

  RGtk2::gtkScrolledWindowAddWithViewport(outer_env$settings_window$settings_window_sw, outer_env$settings_window$settings_window_main_box)

  RGtk2::gtkContainerAdd(outer_env$settings_window$settings_window, outer_env$settings_window$settings_window_sw)


  RGtk2::gtkWindowSetTitle(outer_env$settings_window$settings_window, "Settings")

  RGtk2::gtkWidgetSetSizeRequest(outer_env$settings_window$settings_window, 600, 600)





  RGtk2::gSignalConnect(outer_env$settings_window$settings_window, "delete-event", f = function(window, event, data) {
    outer_env <- data
    outer_env$hide_settings_window()
    return(T)
  }, data = outer_env)


  settings_config <- outer_env$settings_list$table_events


  outer_env$settings_window$settings_config_objs <- list()

  for (config_i in names(settings_config)) {
    header_box <- RGtk2::gtkHBox()
    RGtk2::gtkBoxPackStart(header_box, RGtk2::gtkLabel(config_i), F, F, padding = 5)

    header_reset <- RGtk2::gtkButton("reset to default")
    RGtk2::gtkButtonSetFocusOnClick(header_reset, F)


    RGtk2::gSignalConnect(header_reset, "button-press-event", function(widget,
                                                                       event, data) {
      config_i <- data[[1]]
      outer_env <- data[[2]]
      for (item_i in names(settings_config[[config_i]])) {
        item_name <- paste0(config_i, "|", item_i)

        RGtk2::gtkLabelSetLabel(outer_env$settings_window$settings_config_objs[[item_name]]$label, outer_env$settings_list$default_table_events[[config_i]][[item_i]])
        outer_env$settings_window$settings_config_objs[[item_name]]$val <- outer_env$settings_list$default_table_events[[config_i]][[item_i]]
        outer_env$settings_window$settings_config_objs[[item_name]]$area <- config_i
        outer_env$settings_window$settings_config_objs[[item_name]]$item <- item_i
        outer_env$settings_list$table_events[[config_i]][[item_i]] <- outer_env$settings_list$default_table_events[[config_i]][[item_i]]
      }
      return(T)
    }, data = list(config_i, outer_env))

    RGtk2::gtkBoxPackEnd(header_box, header_reset, F, F, padding = 5)

    RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, header_box, F, F, padding = 4)

    RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, RGtk2::gtkLabel("Click to change settings."), F, F, padding = 4)

    outer_env$settings_window$settings_window_inner_table <- RGtk2::gtkTableNew(rows = length(names(settings_config[[config_i]])) * 2 + 1, columns = 4, homogeneous = F)


    RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, outer_env$settings_window$settings_window_inner_table, F, F, padding = 4)
    inner_table_i <- 0
    for (item_i in names(settings_config[[config_i]])) {
      item_name <- paste0(config_i, "|", item_i)


      evbl <- RGtk2::gtkEventBox()


      if (inner_table_i %% 2) {
        RGtk2::gtkWidgetModifyBg(object = evbl, state = "normal", color = "#DEE1D3")
      } else {
        RGtk2::gtkWidgetModifyBg(object = evbl, state = "normal", color = "#DEDEDE")
      }

      vb <- RGtk2::gtkVBox()
      RGtk2::gtkContainerAdd(evbl, vb)

      outer_env$settings_window$settings_config_objs[[item_name]]$label <- RGtk2::gtkLabel(settings_config[[config_i]][[item_i]])
      outer_env$settings_window$settings_config_objs[[item_name]]$val <- settings_config[[config_i]][[item_i]]
      outer_env$settings_window$settings_config_objs[[item_name]]$area <- config_i
      outer_env$settings_window$settings_config_objs[[item_name]]$item <- item_i
      RGtk2::gtkBoxPackStart(vb, outer_env$settings_window$settings_config_objs[[item_name]]$label, padding = 5)







      evb <- RGtk2::gtkEventBox()


      if (inner_table_i %% 2) {
        RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#F7FAEB")
      } else {
        RGtk2::gtkWidgetModifyBg(object = evb, state = "normal", color = "#f7f7f7")
      }

      hb <- RGtk2::gtkHBox()
      RGtk2::gtkContainerAdd(evb, hb)

      RGtk2::gtkBoxPackStart(hb, RGtk2::gtkLabel(item_i), padding = 0)




      sep <- RGtk2::gtkHSeparatorNew(show = T)
      RGtk2::gtkWidgetModifyBg(object = sep, state = "normal", color = "#f1f1f1")
      RGtk2::gtkTableAttach(outer_env$settings_window$settings_window_inner_table,
        child = sep, left.attach = 0, right.attach = 3,
        top.attach = inner_table_i * 2, bottom.attach = inner_table_i * 2 + 1,
        xoptions = 5, yoptions = 5, xpadding = 0,
        ypadding = 0
      )


      RGtk2::gtkTableAttach(outer_env$settings_window$settings_window_inner_table,
        child = evbl, left.attach = 0, right.attach = 1,
        top.attach = inner_table_i * 2 + 1, bottom.attach = inner_table_i * 2 + 2,
        xoptions = 5, yoptions = 5, xpadding = 0, ypadding = 0
      )

      RGtk2::gtkTableAttach(outer_env$settings_window$settings_window_inner_table,
        child = evb, left.attach = 1, right.attach = 3, top.attach = inner_table_i * 2 + 1,
        bottom.attach = inner_table_i * 2 + 2,
        xoptions = 5, yoptions = 5, xpadding = 0, ypadding = 0
      )







      for (evbx in c(evbl, evb)) {
        RGtk2::gSignalConnect(evbx, "button-press-event", function(widget, event, data) {
          config_ia <- data[[1]]
          item_ia <- data[[2]]
          item_name <- data[[3]]
          outer_env <- data[[4]]
          current_state <- z__event_state(event)
          
          # If the user triggers the exact same keybind currently assigned, clear it
          if (outer_env$settings_window$settings_config_objs[[item_name]]$val == current_state) {
            RGtk2::gtkLabelSetLabel(outer_env$settings_window$settings_config_objs[[item_name]]$label, "-")
            outer_env$settings_window$settings_config_objs[[item_name]]$val <- "-"
            outer_env$settings_list$table_events[[config_ia]][[item_ia]] <- "-"
          } else {
            # Otherwise, assign the new keybind
            RGtk2::gtkLabelSetLabel(outer_env$settings_window$settings_config_objs[[item_name]]$label, current_state)
            outer_env$settings_window$settings_config_objs[[item_name]]$val <- current_state
            outer_env$settings_list$table_events[[config_ia]][[item_ia]] <- current_state
          }

          return(T)
        }, data = list(config_i, item_i, item_name, outer_env))
      }



      cb <- RGtk2::gtkCheckButtonNewWithLabel("show", show = TRUE)
      RGtk2::gtkToggleButtonSetActive(cb, T)

      RGtk2::gtkTableAttach(outer_env$settings_window$settings_window_inner_table,
        child = cb, left.attach = 3, right.attach = 4, top.attach = inner_table_i * 2 + 1,
        bottom.attach = inner_table_i * 2 + 2,
        xoptions = 5, yoptions = 5, xpadding = 0, ypadding = 0
      )



      RGtk2::gSignalConnect(cb, "toggled", function(cb, data) {
        config_ia <- data[[1]]
        item_ia <- data[[2]]
        item_name <- data[[3]]
        outer_env <- data[[4]]
        current_state <- RGtk2::gtkToggleButtonGetActive(cb)


        print(current_state)

        # RGtk2::gtkLabelSetLabel(outer_env$settings_window$settings_config_objs[[item_name]]$label, current_state)
        # outer_env$settings_window$settings_config_objs[[item_name]]$val <- current_state
        # outer_env$settings_list$table_events[[config_ia]][[item_ia]] <- current_state

        return(T)
      }, data = list(config_i, item_i, item_name, outer_env))





      inner_table_i <- inner_table_i + 1
    }    
  }

  #################################################
  #       Add custom items after the loop         #
  # Things I don't want to be right click options #
  #################################################
  
  #Create header box, add heading and reset button
  header_box <- RGtk2::gtkHBox()
  RGtk2::gtkBoxPackStart(header_box, RGtk2::gtkLabel("Other"), F, F, padding = 5)  
  header_reset <- RGtk2::gtkButton("reset to default")
  RGtk2::gtkButtonSetFocusOnClick(header_reset, F)  
  RGtk2::gtkBoxPackEnd(header_box, header_reset, F, F, padding = 5)
  #Add header box to settings window
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, header_box, F, F, padding = 4)
  #Add click to change message to settings window
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, RGtk2::gtkLabel("Click to change settings."), F, F, padding = 4)
  
  
  #Add button for maximization setting
  max <- RGtk2::gtkCheckButtonNewWithLabel("Maximize on load", show = TRUE)
  RGtk2::gtkToggleButtonSetActive(max, outer_env$settings_list$maximize)
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, max, F, F, padding = 4)  
  
  #Add button for Ctrl+Shift setting
  ctsh <- RGtk2::gtkCheckButtonNewWithLabel("Ctrl+Shift runs code", show = TRUE)
  RGtk2::gtkToggleButtonSetActive(ctsh, outer_env$settings_list$ctrlshift)
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, ctsh, F, F, padding = 4) 
  
  #Add button for column label setting
  collabel <- RGtk2::gtkCheckButtonNewWithLabel("Display column label in column headers", show = TRUE)
  RGtk2::gtkToggleButtonSetActive(collabel, outer_env$settings_list$columnlabel)
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, collabel, F, F, padding = 4) 
  
  #Add button for column unique values setting
  colunique <- RGtk2::gtkCheckButtonNewWithLabel("Display number of unique values in column headers", show = TRUE)
  RGtk2::gtkToggleButtonSetActive(colunique, outer_env$settings_list$columnunique)
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, colunique, F, F, padding = 4) 
  
  #Add button for professional loading
  profloading <- RGtk2::gtkCheckButtonNewWithLabel("Show professional loading screen rather than Bob", show = TRUE)
  RGtk2::gtkToggleButtonSetActive(profloading, outer_env$settings_list$professionalloading)
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, profloading, F, F, padding = 4) 

  # Add combo box for Code Case
  case_box <- RGtk2::gtkHBox()
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, case_box, F, F, padding = 4)
  RGtk2::gtkBoxPackStart(case_box, RGtk2::gtkLabel("Generated Code Case: "), F, F, padding = 2)
  case_combo <- RGtk2::gtkComboBoxNewText()
  case_combo$show()
  for (choice in c("Prompt", "Lowercase", "Uppercase")) case_combo$appendText(choice)
  case_combo$setActive(which(c("Prompt", "Lowercase", "Uppercase") == outer_env$settings_list$code_case) - 1)
  RGtk2::gtkBoxPackStart(case_box, case_combo, F, F, padding = 2)
  
  RGtk2::gSignalConnect(case_combo, "changed", function(widget, data) {
    outer_env <- data
    outer_env$settings_list$code_case <- RGtk2::gtkComboBoxGetActiveText(widget)
  }, data = outer_env)

  # Add combo box for Code Spacing
  space_box <- RGtk2::gtkHBox()
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, space_box, F, F, padding = 4)
  RGtk2::gtkBoxPackStart(space_box, RGtk2::gtkLabel("Generated Code Spacing: "), F, F, padding = 2)
  space_combo <- RGtk2::gtkComboBoxNewText()
  space_combo$show()
  for (choice in c("Prompt", "Spaced (x = y)", "Compact (x=y)")) space_combo$appendText(choice)
  space_combo$setActive(which(c("Prompt", "Spaced (x = y)", "Compact (x=y)") == outer_env$settings_list$code_spacing) - 1)
  RGtk2::gtkBoxPackStart(space_box, space_combo, F, F, padding = 2)

  RGtk2::gSignalConnect(space_combo, "changed", function(widget, data) {
    outer_env <- data
    outer_env$settings_list$code_spacing <- RGtk2::gtkComboBoxGetActiveText(widget)
  }, data = outer_env)  
  
  #Define function to call when maximization button clicked
  RGtk2::gSignalConnect(max, "toggled", function(max) {
    current_state <- RGtk2::gtkToggleButtonGetActive(max)
    outer_env$settings_list$maximize <- current_state
    return(T)
  })
  
  #Define function to call when Ctrl+Shift button clicked
  RGtk2::gSignalConnect(ctsh, "toggled", function(ctsh) {
    current_state <- RGtk2::gtkToggleButtonGetActive(ctsh)
    outer_env$settings_list$ctrlshift <- current_state
    return(T)
  })
  
  #Define function to call when column label button clicked
  RGtk2::gSignalConnect(collabel, "toggled", function(collabel) {
    current_state <- RGtk2::gtkToggleButtonGetActive(collabel)
    outer_env$settings_list$columnlabel <- current_state
    return(T)
  })
  
  #Define function to call when column unique button clicked
  RGtk2::gSignalConnect(colunique, "toggled", function(colunique) {
    current_state <- RGtk2::gtkToggleButtonGetActive(colunique)
    outer_env$settings_list$columnunique <- current_state
    return(T)
  })
  
  #Define function to call when professional loading button clicked
  RGtk2::gSignalConnect(profloading, "toggled", function(colunique) {
    current_state <- RGtk2::gtkToggleButtonGetActive(profloading)
    outer_env$settings_list$professionalloading <- current_state
    return(T)
  })
  
  #Define function to call when reset button clicked
  RGtk2::gSignalConnect(header_reset, "button-press-event", function(widget, event, data) {
    cb <- data[[1]]
    case_combo <- data[[2]]
    space_combo <- data[[3]]
    outer_env <- data[[4]]
    
    RGtk2::gtkToggleButtonSetActive(cb, T)
    outer_env$settings_list$maximize <- T
    outer_env$settings_list$ctrlshift <- T
    outer_env$settings_list$columnlabel <- T
    outer_env$settings_list$columnunique <- T
    outer_env$settings_list$professionalloading <- F
    
    # Reset code preferences to Prompt
    outer_env$settings_list$code_case <- "Prompt"
    outer_env$settings_list$code_spacing <- "Prompt"
    case_combo$setActive(0)
    space_combo$setActive(0)
    
    return(T)
  }, data = list(cb, case_combo, space_combo, outer_env))



  #Display working directory for troubleshooting purposes
  header_box2 <- RGtk2::gtkHBox()
  RGtk2::gtkBoxPackStart(header_box2, RGtk2::gtkLabel(paste0("Work directory: ", getwd())), F, F, padding = 5)  
  #Add header box to settings window
  RGtk2::gtkBoxPackStart(outer_env$settings_window$settings_window_main_box, header_box2, F, F, padding = 4)
}
