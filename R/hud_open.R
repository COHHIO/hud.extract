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
