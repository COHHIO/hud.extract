hud_pdf_table <- function(hud_pdf_data) {
  dplyr::bind_rows(hud_pdf_data, .id = "pg") |>
    dplyr::mutate(pg = as.numeric(pg))
}

#' @title Set the dimensions of the main content area for the PDF
#' @description Use \link[tabulizer]{locate_areas} to set the x & y minimums and maximums for the main content area excluding header and footer regions for future use.
#' @param file \code{(character)} Path to file
#' @param pg \code{(numeric)} Page number to use. Pages with a table filling the entire content will provide the most accurate boundaries of the main content area.
#' @return \code{(named list)} with X & Y minimums and maximums
#' @export
hud_dimensions <- function(file = hud_spec_pdf, pg = 20) {
  setNames(as.list(tabulizer::locate_areas(file, pages = pg)[[1]]),
           c("y_min", "x_min", "y_max", "x_max"))
}

#' @title Create hash table functions that transform code specification integers to human-legible characters
#' @description This function parses the appendices of the HUD Spec PDF where Value => Text correspondence tables are represented and transforms these tables into functions that allow coercion between Value/Text
#' @param \code{(list)} output from `hud_pdf_data`
#' @param dims \code{(list)} output from `hud_dimensions`
#' @export

hud_value_tables <- function(hud_pdf_data, dims, .write = TRUE, verify = interactive(), overwrite = FALSE, path = file.path("data", "public", "export_text_translations")) {

  tbl <- hud_pdf_table(hud_pdf_data)
  app_begin <- dplyr::filter(tbl, stringr::str_detect(text, "Appendix|^B$") & stringr::str_detect(font_name, "Light$")) |>
    {\(x) {UU::smode(x$pg)}}()
  app_end <- dplyr::filter(tbl, stringr::str_detect(text, "Appendix|^C") & stringr::str_detect(font_name, "Light$")) |>
    {\(x) {max(x$pg) - 1}}()
  appendix <- dplyr::filter(tbl, pg %in% app_begin:app_end)
  title_size <- dplyr::filter(appendix, stringr::str_detect(font_name, "Light$")) |>
    dplyr::group_by(y, font_name) |>
    dplyr::summarise(font_size = UU::smode(font_size), .groups = "keep") |>
    {\(x) {UU::smode(x$font_size)}}()
  titles <- dplyr::filter(appendix, font_size == title_size) |>
    dplyr::group_by(pg, y) |>
    dplyr::summarise(titles = paste0(text, collapse = " "), .groups = "keep") |>
    dplyr::filter(stringr::str_detect(titles, "^Notes$", negate = TRUE))


  value_tables <- slider::slide(titles, .before = 1L, .complete = TRUE, ~{
    rows <- .x
    fp <- file.path(path, paste0(UU::make_names(rows$titles[1]), ".feather"))
    if (file.exists(fp) && !overwrite)
      return(feather::read_feather(fp))

    pgs <- do.call(seq, as.list(.x$pg))
    if (length(pgs) > 1) {
      area <- purrr::imap(pgs, ~{
        if (.y == 1)
          c(y_min = rows$y[1], x_min = dims$x_min, y_max = dims$y_max, x_max = dims$x_max)
        else if (.y == length(pgs))
          c(y_min = dims$y_min, x_min = dims$x_min, y_max = rows$y[2], x_max = dims$x_max)
        else {
          do.call(c,dims)
        }
      })
    } else {
      area <- list(c(y_min = rows$y[1], x_min = dims$x_min, y_max = rows$y[2], x_max = dims$x_max))
    }

    table <- tibble::as_tibble(do.call(rbind, tabulizer::extract_tables(file = hud_spec_pdf,
                              pages = pgs,
                              area = area)), .name_repair = "minimal")
    if (table[1,1] != "Value")
      table <- table[-1,]

    table <- setNames(table, table[1,])
    # Filter the rows with values equivalent to the table names
    filter_expr <- try({paste0(purrr::map_chr(names(table), ~{
      paste0("`",.x, "` != '", .x,"'")
    }), collapse = " & ") |>
      rlang::parse_expr()})

    out <- rlang::eval_bare(rlang::expr(dplyr::filter(table, !!filter_expr)))
    # Fix character aberrations
    out <- purrr::map_dfc(out, ~{
      stringr::str_replace_all(.x, "â€“", "-") |>
        stringr::str_replace_all("â€™", "'")
    })
    # Coerce Value to numeric
    out <- dplyr::mutate(out, Value = as.numeric(Value))
    if (.write) {
      if (!dir.exists(path))
        UU::mkpath(path)
      print(out, n = nrow(out))
      if (verify) {
        can_write <- utils::askYesNo("Is the above ready to write to feather?")
      }
      if (!verify || isTRUE(get0("can_write")))
        feather::write_feather(out, fp)
    }
  })

  value_tables <- setNames(value_tables, titles$titles)
  return(value_tables)
}

# Tables that didn't parse correctly

c("3.12.1 Living situation Option List", "V7.P DependentUnder6", "C3.B CurrentEdStatus")



#' @title Translate Numeric/Character from a Hash Table with columns Value/Text
#'
#' @param x \code{(character/numeric)} vector to translate
#' @param hash \code{(data.frame)} with Value/Text numeric/character corresponding to the value to translate
#'
#' @return \code{(numeric/character)} Whichever class is opposite the input vector
#' @export

hud_translate <- function(x, hash) {
  UseMethod("hud_translate")
}
#' @title S3 Method for hud_translate
#' @export
hud_translate.numeric <- function(x, hash) {
  out <- rep(NA_character_, length(x))
  na <- is.na(x)
  out[!na] <- purrr::map_chr(x[!na], ~hash[["Text"]][.x == hash[["Value"]]])
  out
}
#' @title S3 Method for hud_translate
#' @export
hud_translate.character <- function(x, hash) {
  out <- rep(NA_real_, length(x))
  na <- is.na(x)
  out[!na] <- purrr::map_dbl(x[!na], ~hash[["Value"]][.x == hash[["Text"]]])
  out
}

#' @title Translate HUD Coding of Data Elements
#' @description Translate values from HUD Data elements between values or text
#' @param .x \code{(character/numeric)} The values to translate
#' @return \code{(character/numeric)} equivalent, depending on the input
#' @export
hud_translations <- list.files(full.names = TRUE, file.path("inst", "export_translations")) |>
  {\(x) {rlang::set_names(x, stringr::str_remove(basename(x), "\\.feather"))}}() |>
  purrr::map(~
               rlang::new_function(args = rlang::pairlist2(.x = , table = FALSE), body = rlang::expr({
                 hash <- feather::read_feather(system.file("export_translations", !!basename(.x), package = "hud.extract", mustWork = TRUE))
                 if (table) {
                   out <- hash
                 } else {
                   out <- hud_translate(.x, hash)
                 }
                 out
               })
               )
  ) |>
  {\(x) {rlang::list2(
    !!!x
  )}}()
