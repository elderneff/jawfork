



#' e__select_setup
#'
#' @param header_table TODO
#' @param row_1 TODO
#' @param obj_env TODO
#'
#' @return TODO

e__select_setup <- function(header_table, row_1,obj_env=inner_env) {
  select_box <- RGtk2::gtkHBox()


  box_label <- RGtk2::gtkLabel("select: ")

  RGtk2::gtkTableAttach(header_table,
    child = box_label, left.attach = 0, right.attach = 1,
    top.attach = row_1, bottom.attach = row_1 + 1, xoptions = 5,
    yoptions = 5, xpadding = 0, ypadding = 0
  )





  select_entry <- RGtk2::gtkEntry()
  RGtk2::gtkEntrySetText(select_entry, "")

  RGtk2::gtkTableAttach(header_table,
    child = select_entry, left.attach = 1, right.attach = 19,
    top.attach = row_1, bottom.attach = row_1 + 1, xoptions = 5,
    yoptions = 5, xpadding = 0, ypadding = 0
  )






  clear_box <- RGtk2::gtkHBox()

  RGtk2::gtkTableAttach(header_table,
    child = clear_box, left.attach = 19, right.attach = 20,
    top.attach = row_1, bottom.attach = row_1 + 1, xoptions = 5,
    yoptions = 5, xpadding = 0, ypadding = 0
  )

  u__button(
    box = clear_box,
    start = T, padding = 1,
    stock_id = "gtk-close",
    tool_tip = "Clear",
    call_back_fct = function(widget, event, data) {
      clean()
      return(FALSE)
    },
    data = NULL
  )





  clean_inner <- function() {
    RGtk2::gtkEntrySetText(select_entry, "")
  }



  clean <- function(obj_env=inner_env) {
    RGtk2::gtkEntrySetText(select_entry, "")
    obj_env$df_obj$update_filter()
  }

  get <- function() {
    return(RGtk2::gtkEntryGetText(select_entry))
  }

  hide <- function() {
    RGtk2::gtkWidgetHide(select_box)
  }

  show <- function() {
    RGtk2::gtkWidgetShow(select_box)
  }



  RGtk2::gSignalConnect(select_entry, "activate", function(menu, data) {
    obj_env <- data
    obj_env$df_obj$update_filter()
    return(T)
  }, data = obj_env)



  add <- function(txt) {
    x <- RGtk2::gtkEntryGetText(select_entry)
    if (x == "") {
      RGtk2::gtkEntrySetText(select_entry, txt)
    } else {
      x <- trimws(strsplit(x, ",", T)[[1]])
      RGtk2::gtkEntrySetText(select_entry, paste0(c(x, txt), collapse = ", "))
    }

    # df_obj$update_filter()
  }



  return(list(clean = clean, clean_inner = clean_inner, get = get, hide = hide, show = show, add = add))
}
