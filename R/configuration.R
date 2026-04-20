
#' Show current cubicle configuration
#'
#' @export
show_config <- function() {

  config <- .read_config()

  # find permanent/session settings
  out <- list(
    session_root     = getOption("cubicle.root"),
    config_root      = config$root,
    session_template = getOption("cubicle.template"),
    config_template  = config$template
  )

  print(out)
  invisible(out)

} # --- show_config()





# Utilities --------------------------------------------------

# -- find location for user directory --
.user_config_dir <- function() {

  rappdirs::user_config_dir("cubicle", "cubicle")

} # --- .user_config_dir()



# -- configure location for saving user info --
.config_path <- function() {

  # create test path (internal use)
  test_path <- Sys.getenv("CUBICLE_CONFIG_PATH", unset = NA_character_)
  if (!is.na(test_path) && nzchar(test_path)) {
    return(test_path)
  }

  # find appropriate system location for configuration
  config_dir <- .user_config_dir()

  # create configuration directory
  fs::dir_create(config_dir)
  fs::path(config_dir, "config.rds")

} # --- .config_path()



# -- read user info --
.read_config <- function() {

  path    <- .config_path()
  default <- list(
    root     = NULL,
    template = NULL,
    projects = list()
  )

  # create configuration list if path exists
  if (!fs::file_exists(path)) {
    return(default)
  }

  # overwrite default with available configured fields
  config <- readRDS(path)
  default[names(config)] <- config

  default

} # --- .read_config()



# -- save user info --
.write_config <- function(config) {

  saveRDS(config, .config_path())
  invisible(config)

} # --- .write_config()
