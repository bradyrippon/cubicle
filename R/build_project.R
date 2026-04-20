
#' Build a new project from the cubicle template
#'
#' @param name Name of the project folder.
#' @param path Path where the project folder should be created.
#' @param root (Optional) Root directory for relative paths.
#' @param group (Optional) Group label to save with the project when
#'   `register = TRUE`.
#' @param register Logical. If `TRUE` (default), save the project in the
#'   internal cubicle project registry.
#' @param load_project Logical. If `TRUE`, open the new project in RStudio,
#'   if available.
#' @param use_name Logical. If `TRUE`, rename the `.Rproj` file by `name`.
#'   If `FALSE` (default), keep the template `.Rproj` file name.
#' @param append (Optional) String to append to notes and project files.
#'
#' @export
build_project <- function(
    name,
    path,
    root         = NULL,
    group        = NULL,
    register     = TRUE,
    load_project = FALSE,
    use_name     = FALSE,
    append       = NULL
) {

  # validate inputs
  .validate_string_required(name, "name")
  .validate_string_required(path, "path")
  .validate_string_optional(root, "root")
  .validate_string_optional(group, "group")
  .validate_flag(register, "register")
  .validate_flag(load_project, "load_project")
  .validate_flag(use_name, "use_name")
  .validate_string_optional(append, "append")

  parent_path <- .decide_path(path, root = root)
  target      <- fs::path(parent_path, name)

  if (fs::dir_exists(target) || fs::file_exists(target)) {
    cli::cli_abort("Target project already exists: {.path {target}}")
  }

  # put template files in specified path
  fs::dir_create(target, recurse = TRUE)

  contents <- fs::dir_ls(get_template(), all = TRUE)
  contents <- contents[!basename(contents) %in% c(".Rproj.user")]

  ## collect folders/files separately
  dir_contents  <- contents[fs::is_dir(contents)]
  file_contents <- contents[!fs::is_dir(contents)]

  if (length(dir_contents) > 0) {
    for (x in dir_contents) {
      fs::dir_copy(x, fs::path(target, basename(x)))
    }
  }

  if (length(file_contents) > 0) {
    fs::file_copy(file_contents, target)
  }

  # remove placeholder files used to preserve empty directories
  # -- NOTE: placeholder files (.cubicle-temp) are stored in inst/template
  # -- so empty directories are preserved in package template
  placeholder_files <- fs::dir_ls(
    path = target,
    all = TRUE,
    recurse = TRUE,
    regexp = "\\.cubicle-temp$"
  )

  if (length(placeholder_files) > 0) {
    fs::file_delete(placeholder_files)
  }

  # edit notes and project files
  old_notes <- fs::path(target, "notes.docx")
  old_proj  <- fs::path(target, "proj.Rproj")

  ## rules for validation
  has_append <- !is.null(append) && nzchar(append)
  name_proj  <- isTRUE(use_name)

  ## notes filename
  if (has_append) {
    append <- .clean_string(append)
  }
  notes_name <- "notes.docx"

  ## project filename
  proj_base <- if (name_proj) {
    .clean_string(name)
  } else {
    "proj"
  }
  proj_name <- paste0(proj_base, ".Rproj")

  ## append names
  if (has_append) {
    notes_name <- paste0("notes-", append, ".docx")
    proj_name  <- paste0(proj_base, "-", append, ".Rproj")
  }

  ## replace old files with new names
  if (fs::file_exists(old_notes)) {
    fs::file_move(old_notes, fs::path(target, notes_name))
  }

  if (fs::file_exists(old_proj)) {
    fs::file_move(old_proj, fs::path(target, proj_name))
  }

  # save project to project registry
  if (isTRUE(register)) {
    .register_project(
      name      = fs::path_file(target),
      path      = target,
      timestamp = Sys.Date(),
      group     = group
    )
  }

  # print success alert
  cli::cli_alert_success("Project created at {.path {target}}")

  if (isTRUE(register)) {
    cli::cli_alert_info("Project registered in cubicle project registry.")
  }

  # load project in RStudio
  if (isTRUE(load_project)) {
    proj_file <- fs::dir_ls(target, regexp = "\\.Rproj$", type = "file")

    if (length(proj_file) == 1 && .is_rstudio_available()) {
      .open_rstudio_project(proj_file)
    } else {
      cli::cli_alert_info("You can only open your project in RStudio.")
    }
  }

  invisible(target)

} # --- build_project()
