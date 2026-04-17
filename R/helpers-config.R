
## configuration for user directory ----------
.user_config_dir <- function() {
  rappdirs::user_config_dir("cubicle", "cubicle")
}



## configure location for saving user info ----------
.config_path <- function() {

  ## find appropriate system location for settings ----------
  config_dir <- .user_config_dir()

  ## create settings directory ----------
  fs::dir_create(config_dir)
  fs::path(config_dir, "settings.rds")

} # --- .config_path


## print user info ----------
.read_config <- function() {

  path <- .config_path()

  ## create configuration list if path exists ----------
  if (!fs::file_exists(path)) {
    return(list(
      root = NULL,
      template = NULL
    ))
  }

  readRDS(path)

} # --- .read_config


## save user info ----------
.write_config <- function(config) {

  path <- .config_path()
  saveRDS(config, path)
  invisible(config)

} # --- .write_config

