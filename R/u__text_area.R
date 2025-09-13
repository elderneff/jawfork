u__add_text_area <- function(label, shift_function, session, timeline, time, outer_env) {
  temp_list <- list()
  temp_list$Frame <- RGtk2::gtkFrame()

  RGtk2::gtkFrameSetLabel(temp_list$Frame, label)

  temp_list$View <- RGtk2::gtkTextView()

    RGtk2::gSignalConnect(temp_list$View, "key-release-event", 
                function(view, event, data) {
                  session<- data[[1]]
                  shift_function <- data[[2]]
                    ###################################
                    # Run code for select key strokes #
                    ###################################
                    key_state <- z__event_state_key(event)
                    single_key <- event[["keyval"]]
                    ctrl <- event[["state"]] == "4"
                    if((key_state=="shift+ctrl" & outer_env$settings_list$ctrlshift) | (ctrl & single_key %in% c("65293", "65458"))){
                      shift_function(session)
                    }
                    ##########################################
                    # Refresh dataset for select key strokes #
                    ##########################################
                    if(ctrl & single_key %in% c("114")){
                      print("Refresh command received")
                      refresh(session_name)
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
                    ###########################################################
                    # Delete the linebreak that gets inserted with Ctrl+Enter #
                    ###########################################################
                    if (ctrl & single_key =="65293") {
                      #Take last item from timeline if there is one
                      if (length(timeline) != 0) {
                        RGtk2::gtkTextBufferSetText(buffer, timeline[time - 1])
                      }
                      #If there is no timeline, set buffer to blank
                      else {
                        RGtk2::gtkTextBufferSetText(buffer, "")
                      }
                    }
                    #########################
                    #Do not add to timeline stack for the following keys:
                    #Left and right ctrl, shift, and alt keys; caps lock, arrow keys, home, end, and tab
                    ##############################
                    #Add buffer to timeline if not command key
                    if (!(single_key %in% c("65507", "65505", "65513", "16777215", "65506", "65508", "65514", "65361", "65362", "65363", "65364", "65360", "65367", "65289"))
                         & !(single_key == "122" & ctrl) & !(single_key == "121" & ctrl)) & !(single_key == "114" & ctrl)) {
                      print(paste0("Detected signal: ", str))
                      timeline[time] <<- str
                      time <<- time + 1
                    }
                    #Undo
                  
                    #TODO: combine entries that are the exact same
                    #TODO: print timeline and time with each key to see how it changes
                  
                    if (single_key == "122" & ctrl & time != 0) {
                      time <<- time - 1
                      RGtk2::gtkTextBufferSetText(buffer, timeline[time])
                      #print(timeline)
                    }
                    #Redo
                    if (single_key == "121" & ctrl & time != length(timeline)) {
                      time <<- time + 1
                      RGtk2::gtkTextBufferSetText(buffer, timeline[time])
                      #print(timeline)
                    }
                  
                    return(TRUE)
                },data=list(session,shift_function))



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
