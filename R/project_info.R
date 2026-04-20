
#' Show information for a registered project
#'
#' @param project Project name/path to registered project.
#'
#' @export
project_info <- function(project) {

  projects <- .get_registered_projects()
  idx      <- .find_project_index(project)
  proj     <- projects[[idx]]

  cli::cli_h1("Project Info")
  cli::cli_text("{.strong Name:} {proj$name}")
  cli::cli_text("{.strong Path:} {.path {proj$path}}")
  cli::cli_text("{.strong Timestamp:} {proj$timestamp}")
  cli::cli_text("{.strong Group:} {proj$group %||% 'None'}")

  tags <- .list_tags(proj$path)
  if (length(tags) > 0) {
    cli::cli_text("{.strong Tags:} {paste(tags, collapse = ', ')}")
  } else {
    cli::cli_text("{.strong Tags:} None")
  }

  invisible(proj)

} # --- project_info()
