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
            
            single_key <- as.character(event[["keyval"]])
            raw_state_int <- as.numeric(event[["state"]])
            #Strip out Caps Lock (2) and Num Lock (16).
            clean_state_int <- bitwAnd(raw_state_int, bitwNot(bitwOr(2, 16)))
            state_str <- as.character(clean_state_int)      
            
            #Helper lists for states.
            is_ctrl_shift <- state_str == "5"
            is_ctrl <- state_str == "4"
          
            #If no modifiers are currently being held down, reset the cancel flag.
            if (single_key %in% c("65505", "65506", "65507", "65508")) {
                outer_env[[session]]$cancel_ctrl_shift <- FALSE
            }            
            #If the key pressed is NOT Ctrl or Shift do not run code.
            else {
                outer_env[[session]]$cancel_ctrl_shift <- TRUE
            }
            #Insert pipe for Ctrl+Shift+M or Ctrl+.
            if ((is_ctrl_shift && single_key %in% c("77", "109")) || (is_ctrl && single_key == "46")) {
                buffer <- RGtk2::gtkTextViewGetBuffer(view)
                RGtk2::gtkTextBufferInsertAtCursor(buffer, " %>% ")
                return(TRUE)
            }
            #65293 is standard Enter, 65458 is Numpad Enter.
            if (is_ctrl && single_key %in% c("65293", "65458")) {
                return(TRUE)
            }
            
            #Intercept GTK's native Ctrl+Backspace (65288) and Ctrl+Delete (65535) to enforce Windows standards.
            if (is_ctrl && single_key %in% c("65288", "65535")) {
                buffer <- RGtk2::gtkTextViewGetBuffer(view)
                
                mark <- RGtk2::gtkTextBufferGetInsert(buffer)
                iter <- RGtk2::gtkTextBufferGetIterAtMark(buffer, mark)$iter
                offset <- RGtk2::gtkTextIterGetOffset(iter)
                
                #Ctrl+Backspace logic.
                if (single_key == "65288" && offset > 0) {
                    start_iter <- RGtk2::gtkTextBufferGetStartIter(buffer)$iter
                    text_before <- RGtk2::gtkTextBufferGetText(buffer, start_iter, iter, TRUE)
                    
                    #Match the previous chunk (punctuation+spaces OR word+spaces OR just spaces).
                    match <- regexpr("([^a-zA-Z0-9_ \t\r\n]+[ \t]*|[a-zA-Z0-9_]+[ \t]*|[ \t\r\n]+)$", text_before)
                    
                    if (match != -1) {
                        chunk_len <- attr(match, "match.length")
                        del_start_iter <- RGtk2::gtkTextBufferGetIterAtOffset(buffer, offset - chunk_len)$iter
                        RGtk2::gtkTextBufferDelete(buffer, del_start_iter, iter)
                    } else {
                        del_start_iter <- RGtk2::gtkTextBufferGetIterAtOffset(buffer, offset - 1)$iter
                        RGtk2::gtkTextBufferDelete(buffer, del_start_iter, iter)
                    }
                } 
                #Ctrl+Delete logic.
                else if (single_key == "65535") {
                    end_iter_full <- RGtk2::gtkTextBufferGetEndIter(buffer)$iter
                    if (offset < RGtk2::gtkTextIterGetOffset(end_iter_full)) {
                        text_after <- RGtk2::gtkTextBufferGetText(buffer, iter, end_iter_full, TRUE)
                        
                        #Match the next chunk (spaces+word OR spaces+punctuation OR just spaces).
                        match <- regexpr("^([ \t]*[a-zA-Z0-9_]+|[ \t]*[^a-zA-Z0-9_ \t\r\n]+|[ \t\r\n]+)", text_after)
                        
                        if (match != -1) {
                            chunk_len <- attr(match, "match.length")
                            del_end_iter <- RGtk2::gtkTextBufferGetIterAtOffset(buffer, offset + chunk_len)$iter
                            RGtk2::gtkTextBufferDelete(buffer, iter, del_end_iter)
                        } else {
                            del_end_iter <- RGtk2::gtkTextBufferGetIterAtOffset(buffer, offset + 1)$iter
                            RGtk2::gtkTextBufferDelete(buffer, iter, del_end_iter)
                        }
                    }
                }
                
                #Fetch the new string immediately following the deletion.
                end_iter_new <- RGtk2::gtkTextBufferGetEndIter(buffer)$iter
                start_iter_new <- RGtk2::gtkTextBufferGetStartIter(buffer)$iter
                new_str <- RGtk2::gtkTextBufferGetText(buffer, start_iter_new, end_iter_new, TRUE)
                
                #Explicitly log to history with a state name that skips the grouping logic!
                outer_env$u__log_history(session, new_str, "ctrl_delete")
                
                #Block default GTK deletion.
                return(TRUE) 
            }
            
            return(FALSE)
        }, data = list(session, outer_env)
    )
  
    RGtk2::gSignalConnect(temp_list$View, "key-release-event", 
                function(view, event, data) {
                  session<- data[[1]]
                  shift_function <- data[[2]]
                  outer_env <- data[[3]]
                  
                  # --- FIX: Clean up the key state to ignore Caps/Num Lock keys
                  raw_state <- as.numeric(event[["state"]])
                  lock_masks <- bitwOr(2, 16) # Caps Lock = 2, Num Lock = 16
                  clean_state <- bitwAnd(raw_state, bitwNot(lock_masks))
                  
                  ###################################
                  # Run code for select key strokes #
                  ###################################
                  key_state <- z__event_state_key(event)
                  single_key <- event[["keyval"]]
                  
                  # Update the ctrl check to look at our sanitized clean_state instead of event[["state"]] == "4"
                  ctrl <- clean_state == 4
                  
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
                     & !(single_key %in% c("122", "90") & ctrl) & !(single_key %in% c("121", "89") & ctrl) & !(single_key == "114" & ctrl)) {
                    
                    # Categorize the keystroke
                    current_state <- "word"
                    if (single_key == "32") current_state <- "space"
                    else if (single_key %in% c("65293", "65458")) current_state <- "enter"
                    else if (single_key %in% c("65288", "65535")) current_state <- "delete"
              
                    # Send to universal tracker
                    outer_env$u__log_history(session, str, current_state)
                  }

                  # Undo (Ctrl+Z or Ctrl+Shift+Z / Caps Lock variants)
                  # 122 = 'z', 90 = 'Z'
                  if ((single_key == "122" | single_key == "90") & ctrl & outer_env[[session]]$time > 1) {
                    t <- outer_env[[session]]$time - 1
                    outer_env[[session]]$time <- t
                    outer_env[[session]]$last_edit_state <- "undo"
                    RGtk2::gtkTextBufferSetText(buffer, outer_env[[session]]$timeline[t])
                  }
                  
                  # Redo (Ctrl+Y or Caps Lock variants)
                  # 121 = 'y', 89 = 'Y'
                  if ((single_key == "121" | single_key == "89") & ctrl & outer_env[[session]]$time < length(outer_env[[session]]$timeline)) {
                    t <- outer_env[[session]]$time + 1
                    outer_env[[session]]$time <- t
                    outer_env[[session]]$last_edit_state <- "redo"
                    RGtk2::gtkTextBufferSetText(buffer, outer_env[[session]]$timeline[t])
                  }
                
                  return(TRUE)
                }, data=list(session, shift_function, outer_env))

  # Enable Horizontal Scrolling in the Text Area
  RGtk2::gSignalConnect(temp_list$View, "scroll-event", function(widget, event, data) {
      # 1. Safely determine if Shift is held
      shift_held <- bitwAnd(as.integer(event[["state"]]), 1) > 0
      direction <- event[["direction"]]
      
      # 2. If it's a normal vertical scroll without Shift, let GTK handle it natively!
      if (!shift_held && !(direction %in% c(RGtk2::GdkScrollDirection["left"], RGtk2::GdkScrollDirection["right"]))) {
          return(FALSE)
      }
      
      # 3. Fetch the adjustment from the parent Scrolled Window
      parent_sw <- RGtk2::gtkWidgetGetParent(widget)
      hadj <- RGtk2::gtkScrolledWindowGetHadjustment(parent_sw)
      
      # Text areas often have very small default steps (pixels vs columns), 
      # so you might actually want a LARGER multiplier here (e.g., 2.0 or 3.0) to speed it up
      speed_modifier <- 2
      step <- RGtk2::gtkAdjustmentGetStepIncrement(hadj)
      actual_step <- step * speed_modifier
      
      # 4. Moving Left: Native Left Scroll -OR- (Shift + Up Scroll)
      if (direction == RGtk2::GdkScrollDirection["left"] || 
         (direction == RGtk2::GdkScrollDirection["up"] && shift_held)) {
          
          new_val <- max(hadj$lower, hadj$value - actual_step)
          RGtk2::gtkAdjustmentSetValue(hadj, new_val)
          return(TRUE) 
          
      # 5. Moving Right: Native Right Scroll -OR- (Shift + Down Scroll)
      } else if (direction == RGtk2::GdkScrollDirection["right"] || 
                (direction == RGtk2::GdkScrollDirection["down"] && shift_held)) {
          
          new_val <- min(hadj$upper - hadj$pageSize, hadj$value + actual_step)
          RGtk2::gtkAdjustmentSetValue(hadj, new_val)
          return(TRUE) 
      }
      
      return(FALSE)
  })

  

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
