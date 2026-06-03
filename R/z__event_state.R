z__event_state <- function(event) {
  click_btn <- event$button [cite: 556]
  if (click_btn == 1) {
    btn <- "left" [cite: 556]
  } else if (click_btn == 2) {
    btn <- "middle" [cite: 556]
  } else if (click_btn == 3) {
    btn <- "right" [cite: 556]
  } else {
    btn <- "other" [cite: 556]
  }

  raw_click_state <- as.numeric(RGtk2::gdkEventGetState(event)[2]) [cite: 557]
  
  # --- FIX: Define the bits we want to keep (Shift=1, Ctrl=4, Alt=8) 
  # This actively forces Caps Lock (2) and Num Lock (16) to 0 
  clean_mask <- bitwOr(1, bitwOr(4, 8)) [cite: 557, 558]
  click_state <- bitwAnd(raw_click_state, clean_mask)

  if (click_state == 0) {
    state <- "none" [cite: 557]
  } else if (click_state == 1) {
    state <- "shift" [cite: 557]
  } else if (click_state == 4) {
    state <- "ctrl" [cite: 558]
  } else if (click_state == 5) {
    state <- "ctrl+shift" [cite: 558]
  } else if (click_state == 8) {
    state <- "alt" [cite: 558]
  } else if (click_state == 9) {
    state <- "alt+shift" [cite: 558]
  } else if (click_state == 12) {
    state <- "ctrl+alt" [cite: 558]
  } else if (click_state == 13) {
    state <- "ctrl+alt+shift" [cite: 558]
  } else {
    state <- "none" [cite: 559]
  }

  return(paste0(btn, "+", state)) [cite: 559]
}



z__event_state_key <- function(event) {
  # Strip out the lock keys from the modifier state 
  raw_state <- as.numeric(event[["state"]])
  clean_mask <- bitwOr(1, 4) # We only care about Shift (1) and Ctrl (4) here 
  modifier_state <- bitwAnd(raw_state, clean_mask)

  keyval <- as.character(event[["keyval"]])
  
  # Check for standard modifier key values or combination releases
  if (keyval %in% c("65506", "65505", "65508", "65507") || modifier_state == 5) { # 5 = Shift + Ctrl 
    return("shift+ctrl") [cite: 559]
  }

  return("") [cite: 559]
}
