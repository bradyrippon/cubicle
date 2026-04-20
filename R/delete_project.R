
#' Remove a project from the cubicle registry
#'
#' @param project Project name/path to registered project.
#'
#' @export
delete_project <- function(project) {

  projects <- .get_registered_projects()
  idx      <- .find_project_index(project)
  removed  <- projects[[idx]]
  projects <- projects[-idx]

  .save_registered_projects(projects)

  cli::cli_alert_success("Removed project {.val {removed$name}} from registry.")
  invisible(removed)

} # --- delete_project()
