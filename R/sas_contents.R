
#' Sas Contents v0.0.1
#'
#' @param sas_path Path to a .sas7bdat file.
#'
#' @return TODO data.frame with the following columns:
#' variable - names of the variables in the dataset.
#' length - the storage length of the variable.
#' type - what kind of variable.
#' label - the variable label.
#' n - the row count.
#'
#' Note the function will always return a data.frame. If it fails, then the row count will be zero.
#'
#' @export
sas_contents <- function(sas_path) {
  output_df <- data.frame(
    "variable" = character(),
    "length" = integer(),
    "type" = character(),
    "label" = character(),
    "n" = integer(),
    stringsAsFactors = FALSE
  )


  if (file.exists(sas_path) == F) {
    message("File does not exist.")
    return(output_df)
  }
  if (grepl("\\.sas7bdat$", sas_path) == F) {
    message("Only works on files ending in .sas7bdat.")
    return(output_df)
  }

  BUGREPORT <- "please report bugs to maintainer"
  connect_success <- F


  tryCatch(
    expr = {
      con <- file(sas_path, "rb")
      connect_success <- T

      # Magic number
      MAGIC <- as.raw(c(
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        0x0, 0x0, 0x0, 0x0, 0xc2, 0xea, 0x81, 0x60,
        0xb3, 0x14, 0x11, 0xcf, 0xbd, 0x92, 0x8, 0x0,
        0x9, 0xc7, 0x31, 0x8c, 0x18, 0x1f, 0x10, 0x11
      ))

      check_magic_number <- function(data) {
        identical(data[1:length(MAGIC)], MAGIC)
      }

      header <- readBin(con, "raw", 288, 1)

      if (length(header) < 288) {
        stop(message = "header too short (not a sas7bdat file?)")
      }
      if (!check_magic_number(header)) {
        stop(message = paste0("magic number mismatch", BUGREPORT))
      }


      read_bin <- function(buf, off, len, type, ...) {
        readBin(buf[(off + 1):(off + len)], type, 1, len, ...)
      }
      read_raw <- function(buf, off, len, ...) {
        readBin(buf[(off + 1):(off + len)], "raw", len, 1, ...)
      }

      read_str <- function(buf, off, len, ...) {
        read_bin(buf, off, len, "character", ...)
      }

      read_flo <- function(buf, off, len, ...) {
        read_bin(buf, off, len, "double", ...)
      }


      read_int <- function(buf, off, len, ...) {
        read_bin(buf, off, len, "integer", ...)
      }
      align1 <- read_raw(header, 32, 1)
      if (identical(align1, as.raw(0x33))) {
        align1 <- 4
      } else {
        align1 <- 0
      }


      if (align1 == 4) {
        u64 <- TRUE
      } else {
        u64 <- FALSE
      }


      align2 <- read_raw(header, 35, 1)
      if (identical(align2, as.raw(0x33))) {
        align2 <- 4
      } else {
        align2 <- 0
      }



      endian <- read_raw(header, 37, 1)
      if (identical(endian, as.raw(0x01))) {
        endian <- "little"
      } else {
        endian <- "big"
        stop(message = "big endian files are not supported")
      }



      winunix <- read_str(header, 39, 1)
      if (identical(winunix, "1")) {
        winunix <- "unix"
      } else if (identical(winunix, "2")) {
        winunix <- "windows"
      } else {
        winunix <- "unknown"
      }


      datecreated <- read_flo(header, 164 + align1, 8)
      datecreated <- datecreated + as.POSIXct("1960/01/01", format = "%Y/%m/%d")
      datemodified <- read_flo(header, 172 + align1, 8)
      datemodified <- datemodified + as.POSIXct("1960/01/01", format = "%Y/%m/%d")



      header_length <- read_int(header, 196 + align2, 4)
      header <- c(header, readBin(con, "raw", header_length - 288, 1))
      if (length(header) < header_length) {
        stop(message = "header too short (not a sas7bdat file?)")
      }



      page_size <- read_int(header, 200 + align2, 4)
      if (page_size < 0) {
        stop(message = paste("page size is negative", BUGREPORT))
      }
      page_count <- read_int(header, 204 + align2, 4)
      if (page_count < 1) {
        stop(message = paste("page count is not positive", BUGREPORT))
      }


      SAS_release <- read_str(header, 216 + align1 + align2, 8)

      OS_version <- read_str(header, 240 + align1 + align2, 16)
      OS_maker <- read_str(header, 256 + align1 + align2, 16)
      OS_name <- read_str(header, 272 + align1 + align2, 16)


      # Subheader 'signatures'
      SUBH_ROWSIZE <- as.raw(c(0xF7, 0xF7, 0xF7, 0xF7))
      SUBH_COLSIZE <- as.raw(c(0xF6, 0xF6, 0xF6, 0xF6))
      SUBH_COLTEXT <- as.raw(c(0xFD, 0xFF, 0xFF, 0xFF))
      SUBH_COLATTR <- as.raw(c(0xFC, 0xFF, 0xFF, 0xFF))
      SUBH_COLNAME <- as.raw(c(0xFF, 0xFF, 0xFF, 0xFF))
      SUBH_COLLABS <- as.raw(c(0xFE, 0xFB, 0xFF, 0xFF))
      SUBH_COLLIST <- as.raw(c(0xFE, 0xFF, 0xFF, 0xFF))
      SUBH_SUBHCNT <- as.raw(c(0x00, 0xFC, 0xFF, 0xFF))

      # Page types
      PAGE_META <- 0
      PAGE_DATA <- 256 # 1<<8
      PAGE_MIX <- c(512, 640) # 1<<9,1<<9|1<<7
      PAGE_AMD <- 1024 # 1<<10
      PAGE_METC <- 16384 # 1<<14 (compressed data)
      PAGE_COMP <- -28672 # ~(1<<14|1<<13|1<<12)
      PAGE_MIX_DATA <- c(PAGE_MIX, PAGE_DATA)
      PAGE_META_MIX_AMD <- c(PAGE_META, PAGE_MIX, PAGE_AMD)
      PAGE_ANY <- c(PAGE_META_MIX_AMD, PAGE_DATA, PAGE_METC, PAGE_COMP)

      page_type_strng <- function(type) {
        if (type %in% PAGE_META) {
          return("meta")
        }
        if (type %in% PAGE_DATA) {
          return("data")
        }
        if (type %in% PAGE_MIX) {
          return("mix")
        }
        if (type %in% PAGE_AMD) {
          return("amd")
        }
        return("unknown")
      }

      pages <- vector("list", page_count)
      for (page_num in 1:page_count) {
        pages[[page_num]] <- list()
        pages[[page_num]]$page <- page_num
        pages[[page_num]]$data <- readBin(con, "raw", page_size, 1)
        pages[[page_num]]$type <- read_int(pages[[page_num]]$data, if (u64) 32 else 16, 2)
        pages[[page_num]]$type_strng <- page_type_strng(pages[[page_num]]$type)
        pages[[page_num]]$blck_count <- read_int(pages[[page_num]]$data, if (u64) 34 else 18, 2)
        pages[[page_num]]$subh_count <- read_int(pages[[page_num]]$data, if (u64) 36 else 20, 2)
      }

      read_subheaders <- function(page, u64) {
        subhs <- list()
        subh_total <- 0
        if (!(page$type %in% PAGE_META_MIX_AMD)) {
          return(subhs)
        }
        # page offset of subheader pointers
        oshp <- if (u64) 40 else 24
        # length of subheader pointers
        lshp <- if (u64) 24 else 12
        # length of first two subheader fields
        lshf <- if (u64) 8 else 4
        for (i in 1:page$subh_count) {
          subh_total <- subh_total + 1
          base <- oshp + (i - 1) * lshp
          subhs[[subh_total]] <- list()
          subhs[[subh_total]]$page <- page$page
          subhs[[subh_total]]$offset <- read_int(page$data, base, lshf)
          subhs[[subh_total]]$length <- read_int(page$data, base + lshf, lshf)
          if (subhs[[subh_total]]$length > 0) {
            subhs[[subh_total]]$raw <- read_raw(
              page$data,
              subhs[[subh_total]]$offset, subhs[[subh_total]]$length
            )
            subhs[[subh_total]]$signature <- read_raw(subhs[[subh_total]]$raw, 0, 4)
          }
        }
        return(subhs)
      }

      subhs <- list()
      for (page in pages) {
        subhs <- c(subhs, read_subheaders(page, u64))
      }


      get_subhs <- function(subhs, signature) {
        keep <- sapply(subhs, function(subh) {
          identical(subh$signature, signature)
        })
        subhs[keep]
      }


      row_count <- NA
      try({
        # Parse row size subheader
        row_size <- get_subhs(subhs, SUBH_ROWSIZE)
        if (length(row_size) != 1) {
          message(paste(
            "found", length(row_size),
            "row size subheaders where 1 expected", BUGREPORT
          ))
        }
        row_size <- row_size[[1]]

        row_count <- read_int(
          row_size$raw,
          if (u64) 48 else 24,
          if (u64) 8 else 4
        )
      })




      col_attr <- get_subhs(subhs, SUBH_COLATTR)


      read_column_attributes <- function(col_attr, u64) {
        info <- list()
        info_ct <- 0
        lcav <- if (u64) 16 else 12
        for (subh in col_attr) {
          cmax <- (subh$length - if (u64) 28 else 20) / lcav
          for (i in 1:cmax) {
            info_ct <- info_ct + 1
            info[[info_ct]] <- list()
            base <- lcav + (i - 1) * lcav
            info[[info_ct]]$offset <- read_int(
              subh$raw, base,
              if (u64) 8 else 4
            )
            info[[info_ct]]$length <- read_int(
              subh$raw,
              base + if (u64) 8 else 4,
              4
            )
            info[[info_ct]]$type <- read_int(
              subh$raw,
              base + if (u64) 14 else 10,
              1
            )
            info[[info_ct]]$type <- ifelse(info[[info_ct]]$type == 1,
              "numeric", "character"
            )
          }
        }
        return(info)
      }




      col_attr <- read_column_attributes(col_attr, u64)


      if (length(col_attr) == 0) {
        stop(message = "No column information.")
      } else {
        col_attr <- dplyr::bind_rows(col_attr)[, -1]
      }


      read_column_names <- function(col_name, col_text, u64) {
        names <- list()
        name_count <- 0
        offp <- if (u64) 8 else 4
        for (subh in col_name) {
          cmax <- (subh$length - if (u64) 28 else 20) / 8
          for (i in 1:cmax) {
            name_count <- name_count + 1
            names[[name_count]] <- list()
            base <- (if (u64) 16 else 12) + (i - 1) * 8
            hdr <- read_int(subh$raw, base, 2)
            off <- read_int(subh$raw, base + 2, 2)
            len <- read_int(subh$raw, base + 4, 2)
            names[[name_count]]$name <- read_str(
              col_text[[hdr + 1]]$raw,
              off + offp, len
            )
          }
        }
        return(names)
      }


      col_name <- get_subhs(subhs, SUBH_COLNAME)
      col_text <- get_subhs(subhs, SUBH_COLTEXT)
      col_name <- read_column_names(col_name, col_text, u64)
      col_name <- dplyr::bind_rows(col_name)
      colnames(col_name) <- "variable"


      read_column_labels_formats <- function(col_labs, col_text, u64) {
        if (length(col_labs) < 1) {
          return(NULL)
        }
        offp <- if (u64) 8 else 4
        labs <- list()
        for (i in 1:length(col_labs)) {
          labs[[i]] <- list()
          base <- if (u64) 46 else 34
          hdr <- read_int(col_labs[[i]]$raw, base, 2)
          off <- read_int(col_labs[[i]]$raw, base + 2, 2)
          len <- read_int(col_labs[[i]]$raw, base + 4, 2)
          if (len > 0) {
            labs[[i]]$format <- read_str(
              col_text[[hdr + 1]]$raw,
              off + offp, len
            )
          }
          labs[[i]]$fhdr <- hdr
          labs[[i]]$foff <- off
          labs[[i]]$flen <- len
          base <- if (u64) 52 else 40
          hdr <- read_int(col_labs[[i]]$raw, base, 2)
          off <- read_int(col_labs[[i]]$raw, base + 2, 2)
          len <- read_int(col_labs[[i]]$raw, base + 4, 2)
          if (len > 0) {
            labs[[i]]$label <- read_str(
              col_text[[hdr + 1]]$raw,
              off + offp, len
            )
          }
          labs[[i]]$lhdr <- hdr
          labs[[i]]$loff <- off
          labs[[i]]$llen <- len
        }
        return(labs)
      }


      col_label <- data.frame("label" = rep(NA, nrow(col_attr)))
      try({
        col_labs <- get_subhs(subhs, SUBH_COLLABS)

        col_labs <- read_column_labels_formats(col_labs, col_text, u64)

        if (is.null(col_labs)) {
          col_label <- data.frame("label" = rep(NA, nrow(col_attr)))
        } else {
          col_label <- dplyr::bind_rows(col_labs)
          if ("label" %in% colnames(col_label)) {
            col_label <- col_label[, c("label"), drop = F]
          } else {
            col_label <- data.frame("label" = rep(NA, nrow(col_attr)))
          }
        }
      })

      output_df <- cbind(col_name, col_attr, col_label)


      output_df$n <- row_count
    }, error = function(e) {
      message(e)
    },
    finally = {
      if (connect_success) {
        close(con)
      }
    }
  )
  return(output_df)
}
