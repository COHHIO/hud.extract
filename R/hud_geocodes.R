ohio_city2county <- function(file = "https://www.omlohio.org/DocumentCenter/View/117/Municipalities-in-Ohio-PDF",
                             path = file.path("data", "ohio_city2county.feather"), overwrite = FALSE) {

  if (file == "https://www.omlohio.org/DocumentCenter/View/117/Municipalities-in-Ohio-PDF" && file.exists(path) && !overwrite) {
    city_table <- feather::read_feather(path)
  } else {
    city_table <- tabulizer::extract_tables(file, pages = 1:{pdftools::pdf_length(file)})
    city_table <- rlang::exec(rbind, city_table[[1]][3:nrow(city_table[[1]]),], !!!city_table[2:length(city_table)])|>
      tibble::as_tibble() |>
      setNames(c("City", "Muni", "County", "Census")) |>
      tibble::add_row(City = "Hamilton City", County = "Hamilton") |>
      tibble::add_row(City = "Cleveland Heights", County = "Summit")
    feather::write_feather(city_table, path)
  }
  return(city_table)
}


#' @title Extract HUD Geocodes
#' @description Extract HUD Geocodes from latest specification pdf in inst folder
#' @details Extracting tables from every page of a PDF is an expensive process, therefore this function saves a  \link[feather]{feather} file for rapid retrieval. The file will be written to `path` with the same name as the pdf passed to `file`.
#' @param file \code{(character)} the file path or URL. If none is specified, the *inst* directory is searched for the most recent file.
#' @param path \code{(character)} The path to save the feather file for the extracted data.
#' @return \code{(tbl_df)} of geocodes. A message indicating where the feather file has been saved for quick retrieval.
#' @export


hud_geocodes <- function(file = "https://www.hud.gov/sites/dfiles/CPD/documents/FY-2021-GeoCodes-with-PPRN_Final.pdf", path = "data", overwrite = FALSE) {
  fp <- file.path(path, stringr::str_replace(basename(file), stringr::regex("pdf$", ignore_case = TRUE), "feather"))
  if (file.exists(fp) && !overwrite)
    tables <- feather::read_feather(fp)
  else {
    tables <- tabulizer::extract_tables(file, pages = 1:{pdftools::pdf_length(file)})


    tables <- purrr::map_dfr(tables, ~{

      start_row <- which(head(apply(.x, 2, stringr::str_detect, pattern = "\\d{5}"), 6))[1]
      out <- setNames(tibble::as_tibble(.x, .name_repair = "unique")[start_row:nrow(.x),], c("GeographicCode", "State", "Name", "FY_PPRN")[1:ncol(.x)]) |>
        dplyr::mutate(
          Type = dplyr::if_else(stringr::str_detect(Name, "County$"), "County", "City"),
          County = dplyr::if_else(Type == "County", stringr::str_remove(Name, "\\sCounty"), NA_character_)
          )

      out
    })
    feather::write_feather(tables, fp)
    cli::cli_alert_success(paste0("Tables saved to ",fp))
  }
  return(tables)
}

ohio_geocodes <- function(geocodes) {
  city_table <- ohio_city2county()
  ohio_tables <- geocodes |>
    dplyr::filter(State == "OH")
  ohio_tables[is.na(ohio_tables$County), "County"] <- purrr::map_chr(ohio_tables$Name[is.na(ohio_tables$County)], ~{
    city_table$County[grepl(paste0("^", .x, "$"), city_table$City, fixed = FALSE)]
    })
  ohio_tables
}

ohio_geocodes_full <- function(files = historical_geocode_pdfs) {
  tables <- purrr::map(files, hud_geocodes)
  # get the common cols
  common_cols <- table(do.call(c, purrr::map(tables, names))) |>
    {\(x) {x[x == UU::smode(x)]}}() |>
    names()
  tables <- purrr::map(tables, ~.x[names(.x) %in% common_cols])
  purrr::map_dfr(tables, ~{
    ohio_geocodes(.x)
  }) |>
  dplyr::distinct(GeographicCode, .keep_all = TRUE)
}

#' @title URLs of legacy geocode PDFs from HUD
#' @description URLs of legacy geocode PDFs from HUD for use with `hud_geocodes`
#' @export

historical_geocode_pdfs <-
c(FY2019 = "https://files.hudexchange.info/resources/documents/fy-2019-CoC-Geographic-Codes.pdf",
  FY2020 = "https://www.hud.gov/sites/dfiles/CPD/documents/FY2020-Geographic-Codes-with-PPRN_Final.pdf",
  FY2021 = "https://www.hud.gov/sites/dfiles/CPD/documents/FY-2021-GeoCodes-with-PPRN_Final.pdf")
