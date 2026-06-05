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
  
  # Extract Shift (1) and Ctrl (4) states using bitwAnd
  has_shift <- bitwAnd(raw_state, 1) > 0
  has_ctrl <- bitwAnd(raw_state, 4) > 0
  
  keyval <- as.character(event[["keyval"]])
  is_shift_key <- keyval %in% c("65505", "65506")
  is_ctrl_key <- keyval %in% c("65507", "65508")
  
  # Trigger only if BOTH modifiers were held down, and one is currently being released
  if (has_shift && has_ctrl && (is_shift_key || is_ctrl_key)) {
    return("shift+ctrl")
  }

  return("")
}
