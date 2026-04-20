
#' Edit metadata for a registered project
#'
#' @param project Project name/path to registered project.
#' @param group (Optional) New group value.
#' @param timestamp (Optional) New time stamp value.
#'
#' @export
edit_project <- function(project, group = NULL, timestamp = NULL) {

  projects <- .get_registered_projects()
  idx      <- .find_project_index(project)

  if (!is.null(group)) {
    projects[[idx]]$group <- group
  }

  if (!is.null(timestamp)) {
    projects[[idx]]$timestamp <- as.character(timestamp)
  }

  .save_registered_projects(projects)

  cli::cli_alert_success("Updated project metadata.")
  invisible(projects[[idx]])

} # --- edit_project()
