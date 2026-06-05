z__event_state <- function(event) {
  click_btn <- event$button
  if (click_btn == 1) {
    btn <- "left"
  } else if (click_btn == 2) {
    btn <- "middle"
  } else if (click_btn == 3) {
    btn <- "right"
  } else {
    btn <- "other"
  }

  raw_click_state <- as.numeric(RGtk2::gdkEventGetState(event)[2])
  
  # --- FIX: Define the lock masks to ignore (Caps Lock = 2, Num Lock = 16)
  lock_masks <- bitwOr(2, 16)
  
  # Strip ONLY those two specific bits out, preserving all mouse button bits!
  click_state <- bitwAnd(raw_click_state, bitwNot(lock_masks))

  # Strip out mouse button bits temporarily JUST for the modifier text logic
  # Left Button = 256, Middle = 512, Right = 1024
  mouse_mask <- bitwOr(256, bitwOr(512, 1024))
  modifier_only_state <- bitwAnd(click_state, bitwNot(mouse_mask))

  if (modifier_only_state == 0) {
    state <- "none"
  } else if (modifier_only_state == 1) {
    state <- "shift"
  } else if (modifier_only_state == 4) {
    state <- "ctrl"
  } else if (modifier_only_state == 5) {
    state <- "ctrl+shift"
  } else if (modifier_only_state == 8) {
    state <- "alt"
  } else if (modifier_only_state == 9) {
    state <- "alt+shift"
  } else if (modifier_only_state == 12) {
    state <- "ctrl+alt"
  } else if (modifier_only_state == 13) {
    state <- "ctrl+alt+shift"
  } else {
    state <- "none"
  }

  return(paste0(btn, "+", state))
}



z__event_state_key <- function(event) {
  raw_state <- as.numeric(event[["state"]])
  
  # Define the lock masks to ignore (Caps Lock = 2, Num Lock = 16)
  lock_masks <- bitwOr(2, 16)
  
  # Strip ONLY those two bits out of the keyboard modifier check
  modifier_state <- bitwAnd(raw_state, bitwNot(lock_masks))

  keyval <- as.character(event[["keyval"]])
  
  # Match against your standard state integer bounds
  # 5 = ctrl+shift 
  # Only trigger if BOTH keys were held down (modifier_state == 5)
  # AND the key being released is either Shift (65505/65506) or Ctrl (65507/65508)
  if (modifier_state == 5 && keyval %in% c("65505", "65506", "65507", "65508")) {
    return("shift+ctrl")
  }

  return("")
}
