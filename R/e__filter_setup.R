



#' e__filter_setup
#'
#' @param header_table TODO
#' @param row_1 TODO
#' @param obj_env TODO
#'
#' @return TODO

e__filter_setup <- function(header_table, row_1,obj_env=inner_env) {
  filter_box <- RGtk2::gtkHBox()




  box_label <- RGtk2::gtkLabel("filter: ")

  RGtk2::gtkTableAttach(header_table,
    child = box_label, left.attach = 0, right.attach = 1,
    top.attach = row_1, bottom.attach = row_1 + 1, xoptions = 5,
    yoptions = 5, xpadding = 0, ypadding = 0
  )





  filter_entry <- RGtk2::gtkEntry()
  RGtk2::gtkEntrySetText(filter_entry, "")

  RGtk2::gtkTableAttach(header_table,
    child = filter_entry, left.attach = 1, right.attach = 19,
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
    RGtk2::gtkEntrySetText(filter_entry, "")
  }



  clean <- function(obj_env=inner_env) {
    RGtk2::gtkEntrySetText(filter_entry, "")
    obj_env$df_obj$update_filter()
  }

  get <- function() {
    return(RGtk2::gtkEntryGetText(filter_entry))
  }

  hide <- function() {
    RGtk2::gtkWidgetHide(filter_box)
  }

  show <- function() {
    RGtk2::gtkWidgetShow(filter_box)
  }



  RGtk2::gSignalConnect(filter_entry, "activate", function(menu, data) {
    obj_env<- data
    obj_env$df_obj$update_filter()
    return(T)
  }, data = obj_env)



  add <- function(column, value,obj_env=inner_env) {
    x <- RGtk2::gtkEntryGetText(filter_entry)

    df <- obj_env$df_obj$full_data()

    #Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
      clean_column <- paste0("`", column, "`") 
    } else {
      clean_column <- column
    }
    
    if (is.numeric(df[[column]])) {
      if (x == "") {
        RGtk2::gtkEntrySetText(filter_entry, paste0(clean_column, " %in% c(", value, ")"))
      } else {
        RGtk2::gtkEntrySetText(filter_entry, paste0(x, " & ", clean_column, " %in% c(", value, ")"))
      }
    } else {
      if (x == "") {
        RGtk2::gtkEntrySetText(filter_entry, paste0(clean_column, " %in% c(\"", value, "\")"))
      } else {
        RGtk2::gtkEntrySetText(filter_entry, paste0(x, " & ", clean_column, " %in% c(\"", value, "\")"))
      }
    }


    obj_env$df_obj$update_filter()
  }




  add_grepl <- function(column, value,obj_env=inner_env) {
    x <- RGtk2::gtkEntryGetText(filter_entry)

    df <- obj_env$df_obj$full_data()

    #Sandwich column name with backticks if it has special characters
    if (!grepl("^[a-zA-Z][a-zA-Z0-9]*$", x)) { 
      clean_column <- paste0("`", column, "`") 
    } else {
      clean_column <- column
    }

    if (is.numeric(df[[column]])) {
      new_filter_string <- paste0("grepl(\"", value, "\",as.character(", clean_column, "))")
    } else {
      new_filter_string <- paste0("grepl(\"", value, "\", ", clean_column, ")")
    }

    if (x == "") {
      RGtk2::gtkEntrySetText(filter_entry, new_filter_string)
    } else {
      RGtk2::gtkEntrySetText(filter_entry, paste0(x, " & ", new_filter_string))
    }


    obj_env$df_obj$update_filter()
  }





  set_dim <- function(txt) {
    RGtk2::gtkLabelSetLabel(dim_label, paste0(txt))
  }

  return(list(clean = clean, clean_inner = clean_inner, get = get, hide = hide, show = show, add = add, add_grepl = add_grepl, set_dim = set_dim))
}
