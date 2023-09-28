


token_row <- function(.data, token, find_min = FALSE) {
  eot <- dplyr::filter(.data, stringr::str_detect(text, token)) |>
    head(1)
}

concat_rows <- function(.data, translation_tables = FALSE) {
  col_to_check <- ifelse(translation_tables, 1, 2)
  if (any(!nzchar(.data[[col_to_check]]))) {
    # all but export conform to this condition
    wrapped <- !nzchar(.data[[col_to_check]])

    to_concat <- UU::rle_df(wrapped) |>
      dplyr::filter(values) |>
      slider::slide(~.x$start:.x$end)


    if (!translation_tables) {
      to_fill <- purrr::map_chr(to_concat, ~{
        paste0(.data[min(.x) - 1, "Notes"]," ", unlist(.data[.x,]), collapse = " ")
      })
      .data[purrr::map_dbl(to_concat, min), "Notes"] <- to_fill
      .data <- .data[!wrapped, ]
    } else {

      if (1 %in% to_concat[[1]]) {
        # If there are long titles spanning multiple row
        names(.data) <- purrr::imap_chr(.data[to_concat[[1]],], ~{
          trimws(paste0(.y, " ", paste0(unlist(.x), collapse = " ")))
        })
        .data <- .data[- to_concat[[1]], ]
        # regenerate wrapped/to_concat since .data rows changed
        wrapped <- !nzchar(.data[[col_to_check]])
        to_concat <- UU::rle_df(wrapped) |>
          dplyr::filter(values) |>
          slider::slide(~.x$start:.x$end)
      }

      # Create the concatenated fill data
      to_fill <- purrr::map(to_concat, ~{
        r <- .x
        out <- rlang::set_names(vector(length = ncol(.data)), names(.data))
        for (nm in names(.data)) {
          out[nm] = trimws(paste0(.data[min(.x) - 1, nm]," ",paste0(unlist(.data[r, nm]), collapse = " ")))
        }
        out
      })
      # map over the row to fill and fill the data
      fill_rows <- purrr::map_int(to_concat, 1) - 1
      for (i in seq_along(to_fill)) {
        .data[fill_rows[i],] <- as.list(to_fill[[i]])
      }
      # remove the wrapped rows
      .data <- .data[!wrapped,]
    }

  }
  .data
}

fill_missed_exportid <- function(.data) {
  if (!"ExportID" %in% .data$Name) {
    .data <- tibble::add_row(.data, Name = "ExportID", Type = "S32")

  }
  .data
}

fix_footnotes <- function(.data) {
  fixes <- c(
    "SSN",
    "Geocode",
    "LastPermanentZIP"
  )

  for (i in seq_along(fixes)) {
    regex <- paste0("^",fixes[i],"\\d$")
    to_fix <- stringr::str_detect(.data$Name, regex)
    if (any(to_fix))
      .data[to_fix, "Name"] <- fixes[i]
  }
  .data
}

#' @title 2024 HUD Specifications PDF
#' @export

hud_spec_2024 <- "https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf"


#' @title 2022 HUD Specifications PDF
#' @export

hud_spec_2022 <- "https://hudhdx.info/Resources/Vendors/HMIS_CSV_Specifications_FY2022_v1.2_clean.pdf"

#' @title 2020 HUD Specifications PDF
#' @export

hud_spec_2020 <- "https://www.hudhdx.info/Resources/Vendors/HMIS%20CSV%20Specifications%20FY2020%20v1.8.pdf"

#' @title Create a pdf data list from the HUD 2024 HMIS Specs
#' @inherit pdftools::pdf_data
#' @export
hud_pdf_data <- function(pdf = hud_spec_2024, font_info = TRUE, opw = "", upw = "") {
  pdftools::pdf_data(pdf, font_info = TRUE)
}


.hud_types <- c("S32", "S50", "S250", "D", "I", "T")
.hud_tbl_cols <- c("DE.", "Name", "Type", "List", "Null", "Notes")

#' @title Extract HUD Export Item Specifications
#' @description Extract the specifications for each of the HUD Export Items from the HUD Specifications PDF
#' @param path \code{(character/list/tibble)} Either a character vector with path to the HUD CSV Spec PDF or output from `hud_pdf_data`
#' @return \code{(list)} of tibbles with specifications from tables in the PDF
#' @export

hud_export_specs <- function(path = hud_spec_2024) {
  # browser()
  pdf_data <- suppressWarnings(hud_pdf_data(path))
  hud_specs_tbl <- dplyr::bind_rows(pdf_data, .id = "Page")

  numbering_starts_on_pg <- 0
  # numbering_starts_on_pg <- which(purrr::map_lgl(pdf_data[-1], ~{
  #   !is.na(as.numeric(dplyr::slice_max(.x, y)$text))
  # }))[1]

  # table of contents data
  toc <- hud_specs_tbl[which(stringr::str_detect(hud_specs_tbl$text,"\\.{10}")),]$Page |>
    unique() |>
    as.numeric()
  toc_data <- dplyr::filter(hud_specs_tbl, Page %in% toc)
  export_nms <- dplyr::filter(toc_data, stringr::str_detect(text, "\\.csv"))

  hud_export_pgs <- {slider::slide_dbl(export_nms, ~{
      dplyr::filter(toc_data, y == .x$y, Page == .x$Page) |>
        dplyr::pull(text) |>
        as.integer() |>
        na.omit()
    }) + numbering_starts_on_pg} |>
      rlang::set_names(stringr::str_extract(export_nms$text, "[\\w\\*]+\\.csv"))
  hud_export_pgs <- hud_export_pgs[stringr::str_detect(names(hud_export_pgs), "\\*", negate = TRUE)]


  hud_specs_tbl <- dplyr::mutate(hud_specs_tbl, Page = as.numeric(Page))
  purrr::imap(hud_export_pgs, ~{
    message(.y)
    browser()
    pg <- .x
    ni <- hud_export_pgs[which(names(hud_export_pgs) %in% .y) + 1]
    if (!UU::is_legit(ni) || .y == "YouthEducationStatus.csv")
      ni = .x+2
    pgs <- .x:ni
    .data <- hud_specs_tbl |>
      dplyr::filter(Page %in% pgs)
    title_ln <- min(stringr::str_which(.data$text, .y))
    .data <- .data[title_ln:nrow(.data),]
    begin_token <- purrr::when(.y,                                                        grepl("Export", .) ~ "Name",
                               ~ "DE\\#")
    two_tables <- sum(hud_export_pgs %in% .x) > 1
    # First row of table
    fr <- token_row(.data, begin_token)
    min_x <- min(fr$x) - fr$width
    min_y <- min(fr$y)
    max_x <- max(.data$x)
    end_token <- purrr::when(.y,
                         grepl("Export", .) ~ "ImplementationID",
                             ~ "ExportID"
    )
    # Last row of table
    lr <- token_row(.data, end_token)

    row_height <- (lr$height + 2)
    max_y <- lr$y + row_height
    pgs <- fr$Page:lr$Page
    if (length(pgs) > 1) {
      dims <- purrr::imap(pgs, ~{
        if (.y == 1)
          c(min_y, min_x, max(pdf_data[[.x]]$y) + row_height, max_x)
        else if (.y == length(pgs))
          c(min(pdf_data[[.x]]$y), min_x, max_y, max_x)
        else {
          "guess"
        }

      })
    } else {
      dims <- list(c(min_y, min_x, max_y, max_x))
    }

    .args <- list(path,
                  pages = pgs,
                  area = dims,
                  output = "data.frame")
    guess <- purrr::map_lgl(dims, ~all(.x == "guess"))
    if (any(guess)) {
      tbls <- purrr::map2(dims, pgs, ~{
        .args$pages <- .y
        if (all(.x == "guess"))
          do.call(tabulizer::extract_tables, .args[!names(.args) %in% "area"])[[1]]
        else {
          .args$area <- list(.x)
          do.call(tabulizer::extract_tables, .args)[[1]]
        }
      })
    } else {
      tbls <- do.call(tabulizer::extract_tables, .args)
    }


    out <- try(dplyr::bind_rows(purrr::map(
      tbls,
      ~ dplyr::mutate(.x, across(where( ~ !is.character(
        .x
      )), as.character))
    ))
    )



    # Fix parse errors ----
    # Fri Oct 08 17:07:40 2021
    # Fix extra cols
    out <- purrr::keep(out, ~!all(is.na(.x))) |>
      purrr::map_dfc(~{
        .x[is.na(.x)] <- ""
        .x
      })
  })

}

hud_export_clean <- function(hud_tbl) {

  #browser(expr = .y == "Exit.csv")
  if (ncol(hud_tbl) > 6) {
    i <- which(purrr::map_lgl(hud_tbl[,7,drop = TRUE], ~!is.na(.x) & nzchar(.x)))
    hud_tbl[i, "Notes"] <- hud_tbl[i, 7]
    hud_tbl <- hud_tbl[names(hud_tbl) %in% .hud_tbl_cols]
  }


  # handle Double line table entries
  col_shifts <- purrr::map_lgl(hud_tbl$Name, ~.x %in% c(.hud_types, ""))
  if (any(col_shifts)) {
    # Handle cases where Name is wrapped and columns shifted
    dbl_line <- purrr::map_lgl(hud_tbl$Name, ~.x %in% .hud_types)
    for (i in which(dbl_line)) {
      hud_tbl[i - 1, ] <- c(hud_tbl[i - 1, 1], paste0(hud_tbl[i - 1, -1], hud_tbl[i,-ncol(hud_tbl)]))
    }
    hud_tbl <- hud_tbl[!dbl_line,]
    # Handle cases with DE# line-wrap
    dbl_de <- stringr::str_detect(hud_tbl[[1]], "^\\&")
    for (i in which(dbl_de)) {
      r <- paste0(hud_tbl[i - 1, ], hud_tbl[i, ])
      if (!is.data.frame(r))
        r <- tibble::tibble_row(!!!rlang::set_names(r, .hud_tbl_cols))
      hud_tbl[i - 1, ] <- r
    }
    hud_tbl <- hud_tbl[!dbl_de,]
    # Handle Cases where Notes is wrapped

    note_wraps <- !nzchar(hud_tbl[[1]])
    if (UU::is_legit(note_wraps) & any(note_wraps) & FALSE) {
      # Notes may wrap multiple lines
      note_ind <- which(note_wraps)
      note_runs <- UU::rle_df(diff(note_ind)) |>
        dplyr::filter(values == 1) |>
        slider::slide(~{
          run <- .x$start:.x$end
          unique(c(run, run + 1))
        })
      to_rm <- c()
      for (i in note_runs) {
        .ind <- note_ind[i]
        .col <- purrr::map_lgl(hud_tbl, ~all(nzchar(.x[.ind])))
        hud_tbl[.ind[1] - 1, "Notes"] <- paste(hud_tbl[.ind[1] - 1, "Notes"], hud_tbl[.ind, .col], collapse = " ")
        to_rm <- c(to_rm, .ind)
      }

      note_ind <- note_ind[!note_ind %in% to_rm]
      for (i in note_ind) {
        hud_tbl[i - 1, "Notes"] <- paste(hud_tbl[i - 1, "Notes"], hud_tbl$DE.[i])
      }
      hud_tbl <- hud_tbl[!note_wraps,]
    }
  }


  # cn <- purrr::when(.y == "Export", isTRUE(.) ~ 1, ~ 2)
  # browser(expr = out[nrow(out), cn] != end_token)
  hud_tbl |>
    fill_missed_exportid() |>
    fix_footnotes() |>
    concat_rows()
}

hash <- tibble::tribble(~ typ, ~ hud, ~ fun, ~ chr,
                  "integer", "I", readr::parse_integer, "i",
                  "numeric", "I", readr::parse_number, "n",
                  "character", "S", readr::parse_character, "c",
                  "logical", "S", readr::parse_logical, "l",
                  "factor", "I", readr::parse_factor, "f",
                  "Date", "D", readr::parse_date, "D",
                  "POSIXct", "T", readr::parse_datetime, "T"
)

#' @title Converts input to a specified type output
#' @description Given various inputs, provide a col_type specification in the format indicated by `outtype`
#' @param x \code{(vector/function)} One of:
#' \itemize{
#'   \item{column}{ \code{(any)}}
#'   \item{a type specification from HUD}{ \code{(character)}}
#'   \item{a readr `parse_*` function (See \link[readr]{parse_logical})}{ \code{(function)}}
#'   \item{a readr type specification (See \link[readr]{cols})}{ \code{(character)}}
#' }
#' @param outtype \code{(character)} One of:
#' \itemize{
#'   \item{\code{"chr"}}{ Returns the class as a readr abbreviation (See \link[readr]{cols})}
#'   \item{\code{"hud"}}{ \code{(character)} a type specification from HUD}
#'   \item{\code{"fun"}}{a readr `parse_*` function (See \link[readr]{parse_logical})}{ \code{(function)}}
#'   \item{\code{"type"}}{ \code{(character)} The R data class}
#' }
#' @return See outtype
#' @export

col_types <- function(x, outtype = c("chr", "hud", "fun", "typ")[1]) {

  intype <- purrr::when(x,
              length(.) == 1 && all(stringr::str_detect(., stringr::regex(paste0("^", hash$hud, "$", collapse = "|"), ignore_case = FALSE))) ~ "hud",
              is.function(.) ~ "fun",
              length(.) == 1 && all(stringr::str_detect(., stringr::regex(paste0("^", hash$chr, "$", collapse = "|"), ignore_case = FALSE))) ~ "chr",
              ~ "typ")

  type <- switch(intype,
         typ = hash$typ[hash$typ %in% class(x)],
         hud = hash$typ[stringr::str_which(hash$hud, x)[1]],
         fun = hash$typ[purrr::map_lgl(hash$fun, identical, y = x)],
         chr = hash$typ[hash$chr %in% x])

  out <- hash[[outtype]][hash$typ %in% type]
  if (outtype == "fun")
    out <- out[[1]]
  out
}


#' @title Translate HUD Export Specification Tables to useful R equivalents
#' @description Given various inputs, provide a col_type specification in the format indicated by `outtype`
#' @param hud_spec \code{(data.frame)} HUD Specification table. Output from `hud_export_specs`.
#' @inheritParams col_types
#' @examples
#' purrr::map(hud_export_specs(hud_pdf_data()), hud_spec_r_type)
#' @export

hud_spec_r_type <- function(hud_spec, outtype = c("chr", "hud", "fun", "typ")[1]) {
  fn <- purrr::when(outtype,
              . == "fun" ~ purrr::map,
              ~ purrr::map_chr)
  ind <- nzchar(hud_spec$Name)
  setNames(fn(hud_spec$Type[ind], col_types, outtype = outtype), hud_spec$Name[ind])
}

# 202o types extracted
# specs <- hud_export_specs(hud_spec_2022)
# specs_clean <- purrr::map(specs, hud_export_clean)
# .col_types <- purrr::map(specs_clean, hud_spec_r_type) |>
#   {\(x) {rlang::set_names(x, stringr::str_remove(names(x), "\\.csv$"))}}()
# .hud_export <- source("../clarity.looker/R/hud_export_2022.R")$value
# hud_export <- purrr::imap(.col_types, ~{
#   # Integrity check
#   if (names(.x)[1] != paste(ifelse(.y == "Client", "Personal", .y),"ID") && names(.x)[length(.x)] != ifelse(.y == "Export", "HashStatus", "ExportID"))
#     stop(.y, " is missing col_types.")
#   if (is.list(.hud_export[[.y]]))
#     purrr::list_modify(.hud_export[[.y]], col_types = .x)
#   else
#     .x
# })
# dput(hud_export, file = "../clarity.looker/R/hud_export_2022.R")

# .dir <- file.path("inst", "export_text_translations", "2022")
# .to_dir <- file.path("..", "HMIS", .dir)
# dir.create(.to_dir, recursive = TRUE)
# list.files(full.names = TRUE, .dir) |>
#   rlang::set_names() |>
#   purrr::map_chr(~file.path("..", "HMIS", .dir, basename(.x))) |>
#   purrr::imap_lgl(~file.copy(.y, .x, overwrite = TRUE, recursive = TRUE))
