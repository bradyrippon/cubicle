
#' Open a registered project
#'
#' @param project Project name/path to registered project.
#'
#' @export
open_project <- function(project) {

  path <- .find_project_path(project)
  rproj_file <- .find_rproj(path)

  if (!.is_rstudio_available()) {
    cli::cli_abort("You can only open a project from within RStudio.")
  }

  if (!fs::file_exists(rproj_file) ||
      !identical(tolower(fs::path_ext(rproj_file)), "rproj")) {
    cli::cli_abort("No `.Rproj` file was found in the registered project directory.")
  }

  .open_rstudio_project(rproj_file)
  invisible(rproj_file)

} # --- open_project()
