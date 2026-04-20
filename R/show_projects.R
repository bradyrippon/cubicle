
#' Show registered cubicle projects
#'
#' @param levels Number of directory levels to print.
#' @param arrange Character vector of fields to sort by. Allowed values
#'   are `"timestamp"`, `"label"`, and `"group"`.
#' @param group (Optional) Group used to filter displayed projects.
#' @param timestamp (Optional) Date used to filter displayed projects.
#' @param tags Logical. If `TRUE`, print tags under each project.
#'
#' @export
show_projects <- function(
    levels    = 1,
    arrange   = "timestamp",
    group     = NULL,
    timestamp = NULL,
    tags      = FALSE
) {

  projects <- .get_registered_projects()
  projects <- .filter_projects(projects, group = group, timestamp = timestamp)
  projects <- .order_projects(projects, arrange = arrange, levels = levels)

  if (length(projects) == 0) {
    cli::cli_alert_info("No registered projects found.")
    return(invisible(projects))
  }

  cli::cli_h1("Registered Projects")

  for (proj in projects) {
    parts <- .find_parts(proj$path, levels = levels)

    if (nzchar(parts$prefix)) {
      cli::cli_text("{.cyan {parts$prefix}}{.green {parts$name}}")
    } else {
      cli::cli_text("{.green {parts$name}}")
    }

    group_text <- proj$group %||% "None"
    cli::cli_text("  {.strong Group:} {group_text}")
    cli::cli_text("  {.strong Timestamp:} {proj$timestamp}")

    if (isTRUE(tags)) {
      tag_names <- .list_tags(proj$path)

      if (length(tag_names) > 0) {
        for (tg in tag_names) {
          cli::cli_text("  {.yellow {tg}}")
        }
      }
    }
  }

  invisible(projects)

} # --- show_projects()





# Utilities --------------------------------------------------


# -- filter projects --
.filter_projects <- function(projects, group = NULL, timestamp = NULL) {

  out <- projects

  if (!is.null(group)) {
    keep <- vapply(out, function(x) identical(x$group %||% NULL, group), logical(1))
    out <- out[keep]
  }

  if (!is.null(timestamp)) {
    timestamp <- as.character(timestamp)
    keep <- vapply(out, function(x) identical(x$timestamp, timestamp), logical(1))
    out <- out[keep]
  }

  out

} # --- .filter_projects()



# -- order projects --
.order_projects <- function(projects, arrange = "timestamp", levels = 1) {

  if (length(projects) <= 1) {
    return(projects)
  }

  valid <- c("timestamp", "label", "group")
  if (!all(arrange %in% valid)) {
    cli::cli_abort(
      "All values of {.arg arrange} must be one of: {.val {paste(valid, collapse = ', ')}}."
    )
  }

  ordering_df <- data.frame(
    idx = seq_along(projects),
    stringsAsFactors = FALSE
  )

  ordering_df$timestamp <- vapply(projects, function(x) x$timestamp %||% "", character(1))
  ordering_df$group     <- vapply(projects, function(x) x$group %||% "", character(1))
  ordering_df$label     <- vapply(
    projects,
    function(x) .find_path_levels(x$path, levels = levels),
    character(1)
  )

  ord <- do.call(order, ordering_df[arrange])
  projects[ordering_df$idx[ord]]

} # --- .order_projects()



# -- find multiple levels of project path --
.find_path_levels <- function(path, levels = 1) {

  if (!is.numeric(levels) || length(levels) != 1 || is.na(levels) ||
      levels < 1 || levels %% 1 != 0) {
    cli::cli_abort("{.arg levels} must be a positive integer.")
  }

  comps <- .path_components(path)
  keep  <- utils::tail(comps, levels)
  paste(keep, collapse = "/")

} # --- .find_path_levels()



# -- split label into prefix/name --
.find_parts <- function(path, levels = 1) {

  label <- .find_path_levels(path, levels = levels)
  comps <- strsplit(label, "/", fixed = TRUE)[[1]]

  if (length(comps) == 1) {
    return(list(prefix = "", name = comps))
  }

  list(
    prefix = paste0(paste(utils::head(comps, -1), collapse = "/"), "/"),
    name   = utils::tail(comps, 1)
  )

} # --- .find_parts()



# -- list available tags --
.list_tags <- function(path) {

  tag_dir <- fs::path(path, "tags")
  if (!fs::dir_exists(tag_dir)) {
    return(character(0))
  }

  tag_files <- fs::dir_ls(
    tag_dir,
    type = "file",
    regexp = "\\.txt$"
  )

  if (length(tag_files) == 0) {
    return(character(0))
  }

  fs::path_ext_remove(fs::path_file(tag_files))

} # --- .list_tags()
