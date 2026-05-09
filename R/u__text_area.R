u__add_text_area <- function(label, shift_function, session, outer_env) {
  temp_list <- list()
  temp_list$Frame <- RGtk2::gtkFrame()

  RGtk2::gtkFrameSetLabel(temp_list$Frame, label)

  temp_list$View <- RGtk2::gtkTextView()

    #Block Ctrl+Enter from inserting a newline
    #Track Ctrl+Shift cancellations
    RGtk2::gSignalConnect(temp_list$View, "key-press-event", 
        function(view, event, data) {
            session <- data[[1]]
            outer_env <- data[[2]]
            
            single_key <- event[["keyval"]]
            state_int <- as.integer(event[["state"]])
            ctrl <- bitwAnd(state_int, 4) > 0
            shift <- bitwAnd(state_int, 1) > 0
            #If no modifiers are currently being held down, reset the cancel flag
            if (!ctrl && !shift) {
                outer_env[[session]]$cancel_ctrl_shift <- FALSE
            }            
            #If the key pressed is NOT Ctrl (65507, 65508) and NOT Shift (65505, 65506) do not run code
            if (!(single_key %in% c("65505", "65506", "65507", "65508"))) {
                outer_env[[session]]$cancel_ctrl_shift <- TRUE
            }            
            # 65293 is standard Enter, 65458 is Numpad Enter
            if (ctrl && single_key %in% c("65293", "65458")) {
                return(TRUE) # TRUE kills the event, blocking the newline
            }            
            return(FALSE) # FALSE lets normal typing pass through to the buffer
        }, data = list(session, outer_env)
    )
  
    RGtk2::gSignalConnect(temp_list$View, "key-release-event", 
                function(view, event, data) {
                  session<- data[[1]]
                  shift_function <- data[[2]]
                  outer_env <- data[[3]]
                    ###################################
                    # Run code for select key strokes #
                    ###################################
                    key_state <- z__event_state_key(event)
                    single_key <- event[["keyval"]]
                    ctrl <- event[["state"]] == "4"
                  
                    run_triggered <- FALSE                    
                    #Look for Ctrl+Enter
                    if (ctrl & single_key %in% c("65293", "65458")) {
                        run_triggered <- TRUE
                    }                    
                    #Look for Ctrl+Shift, only if not cancelled by other keys
                    if (key_state == "shift+ctrl" & outer_env$settings_list$ctrlshift) {
                        if (!isTRUE(outer_env[[session]]$cancel_ctrl_shift)) {
                            run_triggered <- TRUE
                        }
                    }                    
                    #Run for either condition
                    if (run_triggered) {
                        shift_function(session)
                    }
                    ##########################################
                    # Refresh dataset for select key strokes #
                    ##########################################
                    if(ctrl & single_key %in% c("114")){
                      outer_env$show_load_window()
                      outer_env$u__load_dataset(session)
                      outer_env$hide_load_window()
                    }
                    ##########################################
                    # Store buffer whenever a key is pressed #
                    ##########################################
                    buffer <- RGtk2::gtkTextViewGetBuffer(view)
                    end_iter <- RGtk2::gtkTextBufferGetEndIter(buffer)
                    start_iter <- RGtk2::gtkTextBufferGetStartIter(buffer)
                    str <- RGtk2::gtkTextBufferGetText(buffer,
                      start_iter$iter, end_iter$iter,
                      include.hidden.chars = TRUE
                    )
                    #########################
                    # Undo / Redo Tracking  #
                    #########################
                    if (!(single_key %in% c("65507", "65505", "65513", "16777215", "65506", "65508", "65514", "65361", "65362", "65363", "65364", "65360", "65367", "65289"))
                         & !(single_key == "122" & ctrl) & !(single_key == "121" & ctrl) & !(single_key == "114" & ctrl)) {
                      
                      t <- outer_env[[session]]$time
                      #Only save to timeline if the string actually changed
                      if (str != outer_env[[session]]$timeline[t]) {                      
                        #Truncate alternate history branches if user types after an Undo
                        if (length(outer_env[[session]]$timeline) > t) {
                            outer_env[[session]]$timeline <- outer_env[[session]]$timeline[1:t]
                        }
                        
                        #Increment time and store the new state
                        t <- t + 1
                        outer_env[[session]]$timeline[t] <- str
                        outer_env[[session]]$time <- t
                      }
                    }

                    # Undo (Ctrl+Z)
                    if (single_key == "122" & ctrl & outer_env[[session]]$time > 1) {
                      t <- outer_env[[session]]$time - 1
                      outer_env[[session]]$time <- t
                      RGtk2::gtkTextBufferSetText(buffer, outer_env[[session]]$timeline[t])
                    }
                    
                    # Redo (Ctrl+Y)
                    if (single_key == "121" & ctrl & outer_env[[session]]$time < length(outer_env[[session]]$timeline)) {
                      t <- outer_env[[session]]$time + 1
                      outer_env[[session]]$time <- t
                      RGtk2::gtkTextBufferSetText(buffer, outer_env[[session]]$timeline[t])
                    }
                  
                    return(TRUE)
                },data=list(session,shift_function, outer_env))



  temp_list$Scroll <- RGtk2::gtkScrolledWindow()
  RGtk2::gtkScrolledWindowSetPolicy(temp_list$Scroll, "automatic", "automatic")

  RGtk2::gtkContainerAdd(temp_list$Scroll, temp_list$View)
  RGtk2::gtkContainerAdd(temp_list$Frame, temp_list$Scroll)

  RGtk2::gtkWidgetModifyFont(temp_list$View, RGtk2::pangoFontDescriptionFromString("Monospace"))

  temp_list$View["editable"] <- T
  temp_list$View["cursor-visible"] <- T
  temp_list$View["justification"] <- "left" # GtkJustification value
  temp_list$View["left-margin"] <- 10 # 0 is default

  Buffer <- RGtk2::gtkTextViewGetBuffer(temp_list$View)
  RGtk2::gtkTextBufferSetText(Buffer, "")
  return(temp_list)
}


u__text_area_get_text <- function(obj) {
  Buffer <- RGtk2::gtkTextViewGetBuffer(obj$View)
  end_iter <- RGtk2::gtkTextBufferGetEndIter(Buffer)
  start_iter <- RGtk2::gtkTextBufferGetStartIter(Buffer)
  cmd <- RGtk2::gtkTextBufferGetText(Buffer,
    start_iter$iter, end_iter$iter,
    include.hidden.chars = TRUE
  )

  return(cmd)
}



u__text_area_append_text <- function(obj, txt) {
  txt0 <- u__text_area_get_text(obj)
  if (txt0 == "") {
    u__text_area_set_text(obj, txt)
  } else {
    u__text_area_set_text(obj, paste0(txt0, "\n", txt))
  }

  return(T)
}

u__text_area_set_text <- function(obj, txt) {
  Buffer <- RGtk2::gtkTextViewGetBuffer(obj$View)
  RGtk2::gtkTextBufferSetText(Buffer, txt)

  return(T)
}

u__text_area_clear <- function(obj) {
  Buffer <- RGtk2::gtkTextViewGetBuffer(obj$View)
  RGtk2::gtkTextBufferSetText(Buffer, "")

  return(T)
}
