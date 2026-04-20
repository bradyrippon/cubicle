
#' Register an existing project in cubicle
#'
#' @param path Path to a project directory or `.Rproj` file. If `NULL`,
#'   the active/open project is used.
#' @param group (Optional) Group label to save with the project.
#' @param timestamp (Optional) Date of project registration. Defaults to
#'   `Sys.Date()`.
#'
#' @export
register_project <- function(path = NULL, group = NULL, timestamp = NULL) {

  if (is.null(path)) {

    project_root <- .active_project_root()

    if (is.null(project_root)) {
      cli::cli_abort(
        "Could not determine {.arg path}/active R project."
      )
    }

    project_path <- project_root
  } else {

      rproj_path <- .find_rproj(path)

      if (
        fs::file_exists(rproj_path) &&
        identical(tolower(fs::path_ext(rproj_path)), "rproj")
      ) {
        project_path <- fs::path_dir(rproj_path)
      } else {
        project_path <- rproj_path
      }

  }

  project_path <- .normalize_path(project_path)
  name         <- fs::path_file(project_path)

  .register_project(
    name      = name,
    path      = project_path,
    timestamp = timestamp,
    group     = group
  )

  cli::cli_alert_success("Registered project {.val {name}}")
  invisible(project_path)

} # --- register_project()





# Utilities --------------------------------------------------

# -- save project internally --
.register_project <- function(name, path, timestamp = NULL, group = NULL) {

  config <- .read_config()

  entry <- list(
    name      = name,
    path      = .normalize_path(path),
    timestamp = as.character(timestamp %||% Sys.Date()),
    group     = group %||% NULL
  )

  # check if project already exists
  existing_paths <- vapply(
    config$projects,
    function(x) x$path %||% NA_character_,
    character(1)
  )

  idx <- match(entry$path, existing_paths)

  if (is.na(idx)) {
    config$projects[[length(config$projects) + 1]] <- entry
  } else {
    config$projects[[idx]] <- entry
  }

  .write_config(config)
  invisible(entry)

} # --- .register_project()



# -- save registry of projects --
.save_registered_projects <- function(projects) {

  config <- .read_config()
  config$projects <- projects
  .write_config(config)
  invisible(projects)

} # --- .save_registered_projects()



# -- find path components --
.path_components <- function(x) {

  x <- .normalize_path(x)
  parts <- strsplit(x, "[/\\\\]+")[[1]]
  parts[nzchar(parts)]

} # --- .path_components()



# -- find registry index --
.find_project_index <- function(project) {

  projects <- .get_registered_projects()

  if (length(projects) == 0) {
    cli::cli_abort("No registered projects were found.")
  }

  paths <- vapply(projects, function(x) x$path, character(1))
  names <- vapply(projects, function(x) x$name, character(1))

  # direct path match
  if (fs::dir_exists(project) || fs::file_exists(project)) {

    idx <- which(paths == .normalize_path(project))
    if (length(idx) == 0) {
      cli::cli_abort("That path is not currently registered in cubicle.")
    }

    return(idx)
  }

  # name match
  idx <- which(names == project)

  if (length(idx) == 0) {
    cli::cli_abort("No project found with name {.val {project}}.")
  }

  if (length(idx) > 1) {
    cli::cli_abort(c(
      "Multiple registered projects are named {.val {project}}.",
      "i" = "Use the full project path instead."
    ))
  }

  idx

} # --- .find_project_index()



# -- find project path --
.find_project_path <- function(project) {

  projects <- .read_config()$projects
  idx <- .find_project_index(project)
  projects[[idx]]$path

} # --- .find_project_path()



# -- find R project --
.find_rproj <- function(path) {

  # direct path to R project is supplied
  if (fs::file_exists(path) && identical(tolower(fs::path_ext(path)), "rproj")) {
    return(.normalize_path(path))
  }

  # directory supplied, look for R project inside
  if (fs::dir_exists(path)) {
    rprojs <- fs::dir_ls(path, type = "file", regexp = "\\.Rproj$")
    if (length(rprojs) >= 1) {
      return(.normalize_path(rprojs[[1]]))
    }
    return(.normalize_path(path))
  }

  cli::cli_abort("Could not locate a valid project at {.path {path}}.")

} # --- .find_rproj()
