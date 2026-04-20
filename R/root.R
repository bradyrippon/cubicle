
#' Get the saved cubicle root
#'
#' @export
get_root <- function() {

  # look for session setting
  opt <- getOption("cubicle.root")
  if (!is.null(opt) && nzchar(opt)) {
    return(opt)
  }

  # look for permanent setting
  config <- .read_config()
  config$root

} # --- get_root()



#' Save new cubicle root as default
#'
#' @param path Path to the default parent directory for projects
#' @param save Logical. If `TRUE`, save this setting across R sessions.
#' @export
set_root <- function(path, save = TRUE) {

  if (!fs::dir_exists(path)) {
    cli::cli_abort("Directory does not exist: {.path {path}}")
  }

  # find where new root exists
  path <- fs::path_norm(path)

  # save permanent setting (if necessary)
  if (isTRUE(save)) {
    config <- .read_config()
    config$root <- path
    .write_config(config)
  }

  # save session setting
  options(cubicle.root = path)
  invisible(path)

} # --- set_root()



#' Reset the cubicle root
#'
#' @param save Logical. If `TRUE`, revert to package default.
#' @export
reset_root <- function(save = TRUE) {

  if (isTRUE(save)) {
    config <- .read_config()
    config$root <- NULL
    .write_config(config)
  }

  options(cubicle.root = NULL)
  invisible(NULL)

} # --- reset_root()
