


token_row <- function(.data, token, find_min = FALSE) {
  fn <- purrr::when(find_min,
              isTRUE(.) ~ dplyr::slice_min,
              ~ dplyr::slice_max)
  eot <- dplyr::filter(.data, stringr::str_detect(text, token)) %>%

    fn(order_by = y, n = 1) %>%
    head(1)
}

concat_rows <- function(.data) {
  if (any(!nzchar(.data[[2]]))) {
    # all but export conform to this condition
    wrapped <- !nzchar(.data[[2]])

    to_concat <- as.numeric(rownames(.data[wrapped,])) - 1
    .data[to_concat, "Notes"] <- paste0(.data[to_concat, "Notes"], " ", .data[wrapped,1])
    .data <- .data[!wrapped, ]
  }
  .data
}

fill_missed_exportid <- function(.data) {
  if (!"ExportID" %in% .data$Name) {
    .data <- tibble::add_row(.data, Name = "ExportID", Type = "S32")
    .data[nrow(.data), is.na(.data[nrow(.data),])] <- ""
  }
  .data
}


#' @inherit pdftools::pdf_data
#' @export
hud_pdf_data <- function(pdf = "https://hudhdx.info/Resources/Vendors/HMIS_CSV_Specifications_FY2022_v1.0.pdf", font_info = TRUE, opw = "", upw = "") {
  pdftools::pdf_data(pdf, font_info = TRUE)
}

#' @title Extract HUD Export Item Specifications
#' @description Extract the specifications for each of the HUD Export Items from the HUD Specifications PDF
#' @param hud_pdf_data \code{tibble} output from `hud_pdf_data`
#' @return \code({list}) of tibbles with specifications from tables in the PDF

hud_export_specs <- function(hud_pdf_data) {
  hud_specs_tbl <- dplyr::bind_rows(hud_pdf_data, .id = "Page")
  numbering_starts_on_pg <- which(purrr::map_lgl(hud_pdf_data[-1], ~{
    !is.na(as.numeric(dplyr::slice_max(.x, y)$text))
  }))[1]
  toc <- hud_specs_tbl[which(stringr::str_detect(hud_specs_tbl$text,"\\.{20}")),]$Page %>%
    unique %>%
    as.numeric
  toc_data <- dplyr::filter(hud_specs_tbl, Page %in% toc)
  hud_export_pgs <- purrr::imap_int(hud.export::.hud_export, ~{
    toc_ln <- dplyr::filter(toc_data, stringr::str_detect(text, paste0(.y, ".csv?")))
    if (nrow(toc_ln) != 1)
      stop(.y, " Page Number not detected properly")
    nm_y <- toc_ln$y
    pg <- toc_ln$Page
    pg <- dplyr::filter(toc_data, y == nm_y, Page == pg) %>%
      dplyr::pull(text) %>%
      as.integer %>%
      na.omit

  }) + numbering_starts_on_pg



  hud_items_specs <- purrr::imap(hud_export_pgs, ~{
    pg <- .x
    .data <- hud_pdf_data[[pg]]
    title_ln <- min(stringr::str_which(.data$text, paste0(.y, ".csv?")))
    .data <- .data[title_ln:nrow(.data),]
    begin_token <- purrr::when(.y,                                                        .  == "Export" ~ "Name",
                               ~ "DE\\#")
    two_tables <- (hud_export_pgs %in% .x) > 1
    tbl_ln <- token_row(.data, begin_token, find_min = TRUE)
    if (nrow(tbl_ln) == 0) {
      # If the table starts on a page following the title page
      pg <- pg + 1
      .data <- hud_pdf_data[[pg]]
      tbl_ln <- token_row(.data, begin_token, find_min = TRUE)
    }

    end_token <- purrr::when(.y,
                             . == "Export" ~ "HashStatus",
                             ~ "ExportID"
    )
    min_x <- min(tbl_ln$x) - tbl_ln$width
    min_y <- min(tbl_ln$y)
    max_x <- max(.data$x)
    lr <- token_row(.data, end_token, find_min = two_tables)
    o_pg <- pg
    while(nrow(lr) != 1) {
      pg <- pg + 1
      .data <- hud_pdf_data[[pg]]
      lr <- token_row(.data, end_token, find_min = two_tables)
    }
    row_height <- (lr$height + 2)
    max_y <- lr$y + row_height
    pgs <- get0("o_pg", inherits = F, ifnotfound = pg):pg
    if (length(pgs) > 1) {
      dims <- purrr::imap(pgs, ~{
        if (.y == 1)
          c(min_y, min_x, max(hud_pdf_data[[.x]]$y) + row_height, max_x)
        else if (.y == length(pgs))
          c(min(hud_pdf_data[[.x]]$y), min_x, max_y, max_x)
        else {
          "guess"
        }

      })
    } else {
      dims <- list(c(min_y, min_x, max_y, max_x))
    }
    .args <- list(hud_specs,
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

    cn <- purrr::when(.y == "Export", isTRUE(.) ~ 1, ~ 2)
    # browser(expr = inherits(out, "try-error"))
    # browser(expr = out[nrow(out), cn] != end_token)
    fill_missed_exportid(out)
  })

  # Cleanup ----
  # Thu Jul 22 17:21:16 2021
  hud_items_specs <- purrr::map(hud_items_specs, concat_rows)

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
#' @inheritSection col_types description
#' @param hud_spec \code{(data.frame)} HUD Specification table. Output from `hud_export_specs`.
#' @inheritParams col_types
#' @examples
#' purrr::map(hud_export_specs(hud_pdf_data()), hud_spec_r_type)
#' @export
hud_spec_r_type <- function(hud_spec, outtype = c("chr", "hud", "fun", "typ")[1]) {
  fn <- purrr::when(outtype,
              . == "fun" ~ purrr::map,
              ~ purrr::map_chr)
  setNames(fn(hud_spec$Type, col_types, outtype = outtype), hud_spec$Name)
}
