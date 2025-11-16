#' e__generic_filter_function
#'
#' @param e TODO
#' @param df TODO
#' @param u__order_by TODO
#' @param u__filter TODO
#' @param u__select TODO
#' @param table_obj TODO
#' @param outer_env TODO
#'
#' @return TODO

e__generic_filter_function <- function(e, df, u__order_by, u__filter, u__select, table_obj,outer_env=totem) {



  if (nrow(df) == 0) {
    return(df)
  }



  e$df <- df



  cmd <- paste0("df <- df %>% filter(", u__filter, ")")

  tryCatch(
    {
      eval(parse(text = cmd), envir = e)
      df <- e$df
    },
    error = function(e) {
      dialog <- RGtk2::gtkMessageDialog(
        parent = outer_env[[session_name]]$windows$main_window,
        flags = "destroy-with-parent",
        type = "question",
        buttons = "ok-cancel",
        "filter Error: Should entry by cleared?"
      )
      dialog["secondary-text"] <- cmd


      ###################################################
      ### code chunk number 62: Pre-defined-dialogs.Rnw:49-58
      ###################################################
      response <- RGtk2::gtkDialogRun(dialog)
      if (response == RGtk2::GtkResponseType["cancel"] ||
        response == RGtk2::GtkResponseType["close"] ||
        response == RGtk2::GtkResponseType["delete-event"]) {
        ## pass
      } else if (response == RGtk2::GtkResponseType["ok"]) {
        table_obj$clear_filters()
      }
      RGtk2::gtkWidgetDestroy(dialog)
    }
  )


  cmd <- paste0("df <- df  %>% arrange(", u__order_by, ")")

  tryCatch(
    {
      eval(parse(text = cmd), envir = e)
      df <- e$df
    },
    error = function(e) {
      dialog <- RGtk2::gtkMessageDialog(
        parent = outer_env[[session_name]]$windows$main_window,
        flags = "destroy-with-parent",
        type = "question",
        buttons = "ok-cancel",
        "arrange Error: Should entry by cleared?"
      )
      dialog["secondary-text"] <- cmd


      ###################################################
      ### code chunk number 62: Pre-defined-dialogs.Rnw:49-58
      ###################################################
      response <- RGtk2::gtkDialogRun(dialog)
      if (response == RGtk2::GtkResponseType["cancel"] ||
        response == RGtk2::GtkResponseType["close"] ||
        response == RGtk2::GtkResponseType["delete-event"]) {
        ## pass
      } else if (response == RGtk2::GtkResponseType["ok"]) {
        table_obj$clear_arrange()
      }
      RGtk2::gtkWidgetDestroy(dialog)
    }
  )

  if (is_full_data_table) {
    max_size <- 400


    r__ <- df[, "r__", drop = F]
    df <- df[, -1, drop = F]
    e$df <- df

    if (ncol(df) > max_size & u__select == "") {
      table_obj$add_select(paste0("1:", max_size))
      u__select <- paste0("1:", max_size)
    }

    if (u__select != "") {
      cmd <- paste0("df <- df  %>% select(", u__select, ")")

      tryCatch(
        {
          eval(parse(text = cmd), envir = e)
          df <- e$df
        },
        error = function(e) {
          dialog <- RGtk2::gtkMessageDialog(
            parent = outer_env[[session_name]]$windows$main_window,
            flags = "destroy-with-parent",
            type = "question",
            buttons = "ok-cancel",
            "select Error: Should entry by cleared?"
          )
          dialog["secondary-text"] <- cmd


          ###################################################
          ### code chunk number 62: Pre-defined-dialogs.Rnw:49-58
          ###################################################
          response <- RGtk2::gtkDialogRun(dialog)
          if (response == RGtk2::GtkResponseType["cancel"] ||
            response == RGtk2::GtkResponseType["close"] ||
            response == RGtk2::GtkResponseType["delete-event"]) {
            if (ncol(df) > max_size) {
              df <- df[, 1:max_size]
            }
            ## pass
          } else if (response == RGtk2::GtkResponseType["ok"]) {
            table_obj$clear_select()



            if (ncol(df) > max_size) {
              table_obj$add_select(paste0("1:", max_size))
              df <- df[, 1:max_size]
            }
          }
          RGtk2::gtkWidgetDestroy(dialog)
        }
      )
    }
    df <- cbind(r__, df)
  }
  e$df <- NULL

  return(as.matrix(df))
}
