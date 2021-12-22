#' @title Open stored HUD PDFs
#'
#' @param type \code{(character)} One of: \code{`r paste0(eval(rlang::fn_fmls(open_pdfs)$type[[2]]), collapse = ", ")`}
#' @param year \code{(character/numeric)} to open. Use `"latest"` to return the most recent
#'
#' @return \code{(character)} file path invisibly
#' @export
#'
#' @examples
#' open_pdfs()
open_pdfs = function(type = c("csv_specs", "data_dictionarys", "data_standards", "acronyms")[1], year = "latest") {

  types <- eval(rlang::fn_fmls()$type[[2]])
  if (!type %in% types)
    rlang::abort(paste0("`type` must be one of: ", paste0(types, collapse = ", ")))

  fp <- file.path(ifelse(stringr::str_detect(getwd(), "hud.extract$"), file.path(getwd(), "inst"), system.file(package = "hud.extract")), type)


  fs <- list.files(fp)
  opts <- c(versions = "(?<=v)[\\d]+\\.\\d+",
    years = "\\d{4}") |>
    purrr::map(~as.numeric(stringr::str_extract(basename(fs), .x)))
  if (year == "latest")
    y <- which(opts$years == max(opts$years))
  else
    y <- which(as.numeric(year) == opts$years)
  if (UU::is_legit(y)) {
    fs <- fs[y]
    opts <- purrr::map(opts, `[`, y)
  }
  # Return the latest version
  if (UU::is_legit(opts$versions))
    f <- fs[which.max(opts$versions)]

  f <- file.path(fp, f)
  shell.exec(f)
  cli::cli_alert_success(paste0("Opening ", f))
  invisible(f)
}

#' @title Extract a previously downloaded HUD Export archive
#'
#' @param browser_dl_folder \code{(character)} path to the browser's download folder or the file to extract
#' @param extract_path \code{(character)} path to the folder where the archive is to be extracted
#' @param delete_archive \code{(logical)} Delete the archive after extracting?
#' @param moment \code{(POSIXct/Date)} The time point which the archive creation time should be greater than to ensure it's recent.
#' @param wait \code{(Duration)} to wait for the file to appear in the download directory. Relevant when using browser automation.
#' @return \code{(logical)} as to whether the extraction was successful
#' @export

hud_export_extract <- function(browser_dl_folder = "~/../Downloads", extract_path = "data", delete_archive = TRUE, moment = Sys.Date(), wait = lubridate::minutes(2)) {
  downloads <- path.expand(browser_dl_folder)
  if (!file.exists(downloads)) {
    dls <- list.files(downloads, full.names = TRUE, pattern = "^hudx")
    dl_times <- do.call(c, purrr::map(dls, ~file.info(.x)$mtime))
    if (!UU::is_legit(dl_times))
      cli::cli_alert(paste0("No HUD Export found in ", path.expand(downloads), " waiting ", wait))
    wait = lubridate::now() + wait
    .recent <- dl_times > moment
    while (!any(.recent) && Sys.time() < wait) {
      Sys.sleep(5)
      dls <- list.files(downloads, full.names = TRUE, pattern = "^hudx")
      dl_times <- do.call(c, purrr::map(dls, ~file.info(.x)$mtime))
      .recent <- dl_times > moment
    }
  } else {
    f <- downloads
  }

  if (any(get0(".recent", inherits = FALSE)))
    f <- dls[.recent]




  if (UU::is_legit(f))
    archive::archive_extract(f, path.expand(extract_path))
  } else
    cli::cli_alert("No HUD Export found in ", path.expand(downloads), " with creation time greater than ", moment)

  if (delete_archive)
    file.remove(f)
}
