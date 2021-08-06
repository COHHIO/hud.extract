hud_pdf_table <- function(hud_pdf_data) {
  dplyr::bind_rows(hud_pdf_data, .id = "pg") |>
    dplyr::mutate(pg = as.numeric(pg))
}

hud_value_tables <- function(hud_pdf_data) {
  tbl <- hud_pdf_table(hud_pdf_data)
  app_begin <- dplyr::filter(tbl, stringr::str_detect(text, "Appendix|^B$") & stringr::str_detect(font_name, "Light$")) %>%
    {hud.export:::.mode(.$pg)}
  app_end <- dplyr::filter(tbl, stringr::str_detect(text, "Appendix|^C") & stringr::str_detect(font_name, "Light$")) %>%
    {hud.export:::.mode(.$pg) - 1}
  appendix <- dplyr::filter(tbl, pg %in% app_begin:app_end)
  title_size <- dplyr::filter(appendix, stringr::str_detect(font_name, "Light$")) %>%
    dplyr::group_by(y, font_name) %>%
    dplyr::summarise(font_size = hud.export:::.mode(font_size), .groups = "keep") %>%
    {hud.export:::.mode(.$font_size)}
  titles <- dplyr::filter(appendix, font_size == title_size) %>%
    group_by(pg, y) %>%
    dplyr::summarise(titles = paste0(text, collapse = " "), .groups = "keep") %>%
    dplyr::filter(stringr::str_detect(titles, "^Notes$", negate = TRUE))


}

