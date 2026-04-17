

## helper ----------
.default_template_path <- function() {
  system.file("template", package = "cubicle")
}



#' Get the active cubicle template path
#'
#' @export
get_template <- function() {

  ## check sessions settings ----------
  opt <- getOption("cubicle.template")
  if (!is.null(opt) && fs::dir_exists(opt)) {
    return(opt)
  }

  ## check permanent settings ----------
  config <- .read_config()
  if (!is.null(config$template) && fs::dir_exists(config$template)) {
    return(config$template)
  }

  ## find default settings ----------
  if (!nzchar(.default_template_path())) {
    stop("Default package template could not be found.", call. = FALSE)
  }

  .default_template_path()

} # --- get_template


#' Show the current cubicle template structure
#'
#' @export
show_template <- function() {

  cli::cli_h1("{.cyan Cubicle Template}")
  fs::dir_tree(
    path = get_template(),
    regexp = "\\.cubicle-temp$",
    invert = TRUE
  )
  invisible(NULL)

} # --- show_template


#' Set a custom cubicle template
#'
#' @param path Path to a folder containing a template structure
#' @param save Logical. If `TRUE`, save this setting across R sessions.
#' @export
set_template <- function(path, save = TRUE) {

  if (!fs::dir_exists(path)) {
    stop("Template directory does not exist: ", path, call. = FALSE)
  }

  ## find where new template exists ----------
  path <- fs::path_norm(path)

  ## save permanent setting (if necessary) ----------
  if (isTRUE(save)) {
    config <- .read_config()
    config$template <- path
    .write_config(config)
  }

  ## save session setting ----------
  options(cubicle.template = path)
  invisible(path)

} # --- set_template


#' Reset to the default cubicle template
#'
#' @param save Logical. If `TRUE`, revert to package default.
#' @export
reset_template <- function(save = TRUE) {

  if (isTRUE(save)) {
    config <- .read_config()
    config$template <- NULL
    .write_config(config)
  }

  options(cubicle.template = NULL)
  invisible(NULL)

} # --- reset_template
