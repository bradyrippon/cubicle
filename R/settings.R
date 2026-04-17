
#' Show cubicle settings
#'
#' @export
cubicle_settings <- function() {

  config <- .read_config()

  ## find permanent/session settings ----------
  out <- list(
    session_root     = getOption("cubicle.root"),
    saved_root       = config$root,
    session_template = getOption("cubicle.template"),
    saved_template   = config$template
  )

  print(out)
  invisible(out)

} # --- cubicle_settings
