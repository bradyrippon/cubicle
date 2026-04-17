
## rlang operator ----------
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


## combine path and root directory ----------
.decide_path <- function(path, root = NULL) {

  if (fs::is_absolute_path(path)) {
    return(fs::path_norm(path))
  }

  chosen_root <- root %||% get_root()

  ## handle mismatch of path/root ----------
  if (is.null(chosen_root) || !nzchar(chosen_root)) {
    stop(
      "Relative path supplied but no root directory was found.",
      "Use `root = ...` or run `set_root()` first.",
      call. = FALSE
    )
  }

  fs::path_norm(fs::path(chosen_root, path))

} # --- .decide_path


## remove whitespace and special characters ----------
.clean_string <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
} # --- .clean_string
