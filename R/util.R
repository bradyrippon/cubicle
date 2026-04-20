
# -- rlang operator --
`%||%` <- function(x, y) {if (is.null(x)) y else x}



# -- remove whitespace and special characters --
.clean_string <- function(x) {

  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x

} # --- .clean_string()



# -- normalize paths --
.normalize_path <- function(path) {

  fs::path_norm(fs::path_abs(path))

} # --- .normalize_path()



# -- combine path/root directory --
.decide_path <- function(path, root = NULL) {

  if (fs::is_absolute_path(path)) {
    return(fs::path_norm(path))
  }

  chosen_root <- root %||% get_root()

  # handle mismatch of path/root
  if (is.null(chosen_root) || !nzchar(chosen_root)) {
    cli::cli_abort(c(
      "Relative path supplied but no root directory was found.",
      "i" = "Use {.code root = ...} or run {.fn set_root} first."
    ))
  }

  fs::path_norm(fs::path(chosen_root, path))

} # --- .decide_path()



# -- input validation: TRUE/FALSE flag --
.validate_flag <- function(x, arg) {

  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be `TRUE` or `FALSE`.")
  }
  invisible(x)

} # --- .validate_flag()


# -- input validation: character string (required) --
.validate_string_required <- function(x, arg) {

  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    cli::cli_abort("{.arg {arg}} must be a non-empty character string.")
  }
  invisible(x)

} # --- .validate_string_required()



# -- input validation: character string (optional) --
.validate_string_optional <- function(x, arg) {

  if (is.null(x)) {
    return(invisible(x))
  }

  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    cli::cli_abort("{.arg {arg}} must be `NULL` or a non-empty character string.")
  }
  invisible(x)

} # --- .validate_string_optional()



# -- get list of registered projects --
.get_registered_projects <- function() {

  .read_config()$projects

} # --- .get_registered_projects()



# -- detect whether RStudio API is available --
.is_rstudio_available <- function() {

  requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()

} # --- .is_rstudio_available()



# -- get active RStudio project safely --
.get_active_rstudio_project <- function() {

  if (!.is_rstudio_available()) {
    return(NULL)
  }

  tryCatch(
    rstudioapi::getActiveProject(),
    error = function(e) NULL
  )

} # --- .get_active_rstudio_project()



# -- open project in RStudio --
.open_rstudio_project <- function(path) {

  if (!.is_rstudio_available()) {
    cli::cli_abort("You can only open a project from within RStudio.")
  }

  rstudioapi::openProject(path)
  invisible(path)

} # --- .open_rstudio_project()
