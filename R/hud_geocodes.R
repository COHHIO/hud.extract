#' @title Extract HUD Geocodes
#' @description Extract HUD Geocodes from latest specification pdf in inst folder
#' @param file \code{(character)} the file path or URL. If none is specified, the *inst* directory is searched for the most recent file.
#' @return \code{(tbl_df)} of geocodes
#' @export


hud_geocodes <- function(file) {
  if (missing(file))
    file <- list.files("inst", "geocode", ignore.case = TRUE, full.names = TRUE) |>
  {\(x) {
    subset(x, as.numeric(stringr::str_extract(x, "\\d{4}")) |> {\(x) x == max(x, na.rm = TRUE)}())
  }}()

  tables <- tabulizer::extract_tables(file, pages = 1:{pdftools::pdf_length(file)})
  tables <- purrr::map_dfr(tables, ~{
    setNames(tibble::as_tibble(.x[3:nrow(.x),]), c("GeographicCode", "State", "Name", "FY_PPRN")) |>
      dplyr::mutate(
        Type = ifelse(stringr::str_detect(Name, "County$"), "County", "City"),
        Name = stringr::str_remove(Name, "\\sCounty"),
        FY_PPRN = as.numeric(stringr::str_remove_all(FY_PPRN, "\\$|\\,")))
  })
  tables
}


