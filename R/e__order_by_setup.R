



#' e__order_by_setup
#'
#' @param header_table TODO
#' @param row_1 TODO
#' @param obj_env TODO
#'
#' @return TODO

e__order_by_setup <- function(header_table, row_1,obj_env=inner_env) {
  order_by_box <- RGtk2::gtkHBox()


  box_label <- RGtk2::gtkLabel("arrange: ")

  RGtk2::gtkTableAttach(header_table,
    child = box_label, left.attach = 0, right.attach = 1,
    top.attach = row_1, bottom.attach = row_1 + 1, xoptions = 5,
    yoptions = 5, xpadding = 0, ypadding = 0
  )





  order_by_entry <- RGtk2::gtkEntry()
  RGtk2::gtkEntrySetText(order_by_entry, "")

  RGtk2::gtkTableAttach(header_table,
    child = order_by_entry, left.attach = 1, right.attach = 19,
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
    RGtk2::gtkEntrySetText(order_by_entry, "")
  }



  clean <- function(obj_env=inner_env) {
    RGtk2::gtkEntrySetText(order_by_entry, "")
    obj_env$df_obj$update_filter()
  }

  get <- function() {
    return(RGtk2::gtkEntryGetText(order_by_entry))
  }

  hide <- function() {
    RGtk2::gtkWidgetHide(order_by_box)
  }

  show <- function() {
    RGtk2::gtkWidgetShow(order_by_box)
  }



  RGtk2::gSignalConnect(order_by_entry, "activate", function(menu, data) {
    obj_env <- data
    obj_env$df_obj$update_filter()
    return(T)
  }, data = obj_env)



  add <- function(column,obj_env=inner_env) {
    #Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", column)) { 
      clean_column <- paste0("`", column, "`") 
    } else {
      clean_column <- column
    }
    
    desc_column <- paste0("desc(", clean_column, ")")
    x <- RGtk2::gtkEntryGetText(order_by_entry)
    if (x == "") {
      RGtk2::gtkEntrySetText(order_by_entry, paste0(clean_column))
    } else {
      x <- trimws(strsplit(x, ",", T)[[1]])
      if (clean_column %in% x) {
        x[which(x %in% clean_column)] <- desc_column
        RGtk2::gtkEntrySetText(order_by_entry, paste0(x, collapse = ", "))
      } else if (desc_column %in% x) {
        RGtk2::gtkEntrySetText(order_by_entry, paste0(setdiff(x, desc_column), collapse = ", "))
      } else {
        RGtk2::gtkEntrySetText(order_by_entry, paste0(c(x, clean_column), collapse = ", "))
      }
    }

    obj_env$df_obj$update_filter()
  }



  return(list(clean = clean, clean_inner = clean_inner, get = get, hide = hide, show = show, add = add))
}
