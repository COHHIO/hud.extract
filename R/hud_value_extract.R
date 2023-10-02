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
hud_dimensions <- function(file = hud_spec_2024, pg = 35) {
  setNames(as.list(tabulizer::locate_areas(file, pages = pg)[[1]]),
           c("y_min", "x_min", "y_max", "x_max"))
}

#' @title Create hash table functions that transform code specification integers to human-legible characters
#' @description This function parses the appendices of the HUD Spec PDF where Value => Text correspondence tables are represented and transforms these tables into functions that allow coercion between Value/Text
#' @param \code{(list)} output from `hud_pdf_data`
#' @param dims \code{(list)} output from `hud_dimensions`. *Required*
#' @export

hud_value_tables <- function(pdf = hud_spec_2024, dims = hud_dimensions(hud_spec_2024), .write = TRUE, verify = interactive(), overwrite = TRUE, path = file.path("inst", "export_text_translations", "2024")) {
  tbl <- hud_pdf_table(hud_pdf_data(pdf))
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
    if (rows$titles[1] != "3.12.1 Living Situation Option List" ) {
      raw_table <- do.call(rbind, tabulizer::extract_tables(file = pdf,
                                                            pages = pgs, area = area))

      title_row <- which(stringr::str_detect(raw_table[,1], "^Value"))
    } else {
      raw_table <- do.call(rbind, tabulizer::extract_areas(file = pdf,
                                                            pages = pgs))
      # Create a new title row
      title_row_values <- matrix(c("Value", "Text", "Prior Living Situation (3.917)",
                         "Current Living Situation (4.12)",
                         "Destination (3.12)"), nrow = 1)

      # Append new title row
      raw_table <- rbind(title_row_values, raw_table)

      # Remove old title rows (2 to 4) from the original matrix
      raw_table <- raw_table[-c(2:4), ]

      title_row <- which(stringr::str_detect(raw_table[,1], "^Value"))
    }


    if (UU::is_legit(title_row)) {
      row_rm <- purrr::when(title_row,
                  . > 1 ~ 1:title_row,
                  ~ title_row)

      table <- setNames(tibble::as_tibble(raw_table), raw_table[title_row,])[-row_rm,]
    } else {
      table <- setNames(tibble::as_tibble(raw_table), c("Value", "Text"))
    }

    table <- concat_rows(table, translation_tables = TRUE)

    # Filter the rows with values equivalent to the table names
    filter_expr <- try({paste0(purrr::map_chr(names(table), ~{
      paste0("`",.x, "` != '", .x,"'")
    }), collapse = " & ") |>
      rlang::parse_expr()})

    out <- rlang::eval_bare(rlang::expr(dplyr::filter(table, !!filter_expr)))
    # Fix character aberrations
    out <- purrr::map_dfc(out, ~{
      stringr::str_replace_all(.x, "â€“", "-") |>
        stringr::str_replace_all("â€™", "'") |>
        stringr::str_replace_all("\\\r", " ")
    })
    # Coerce Value to numeric
    if (!stringr::str_detect(.x$titles[1], "VAMCStationNumber$"))
      out <- dplyr::mutate(out, Value = as.numeric(Value))

    if (.write) {
      if (!dir.exists(path))
        UU::mkpath(path)
      print(out, n = nrow(out))
      if (verify) {
        choices <- c("Yes" = TRUE, "Skip" = FALSE, "Browser" = NA)
        can_write <- utils::menu(title = paste0("Can this table be written for ", .x$titles[1],"?"), choices = names(choices))
        choice <- choices[can_write]

      }
      choice <- get0("choice")
      browser(expr = is.na(choice))
      if (!verify || isTRUE(choice)) {
        feather::write_feather(out, fp)
      }
    }
    out
  })

  value_tables <- setNames(value_tables, titles$titles)
  return(value_tables)
}

# Tables that didn't parse correctly

# tibble::tribble(~Value, ~Text, 0, "Pursuing a high school diploma or GED",
#                 1, "Pursuing Associate’s Degree",
#                 2, "Pursuing Bachelor’s Degree",
#                 3, "Pursuing Graduate Degree",
#                 4, "Pursuing other post-secondary credential",
#                 8, "Client doesn't know",
#                 9, "Client refused",
#                 99, "Data not collected") |> feather::write_feather(file.path("inst", "export_text_translations", "2022", "C3.B CurrentEdStatus.feather"))





