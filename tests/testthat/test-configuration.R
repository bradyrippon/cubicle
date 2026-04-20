
test_that(".user_config_dir returns a character path", {
  out <- .user_config_dir()

  expect_type(out, "character")
  expect_length(out, 1)
  expect_true(nzchar(out))
})

test_that(".config_path returns config.rds path and creates directory", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(CUBICLE_CONFIG_PATH = NA_character_)

  with_mocked_bindings(
    {
      out <- .config_path()

      expect_true(fs::dir_exists(fs::path_dir(out)))
      expect_equal(fs::path_file(out), "config.rds")
    },
    .user_config_dir = function() tmp
  )
})

test_that(".config_path uses CUBICLE_CONFIG_PATH when supplied", {
  tmp <- withr::local_tempdir()
  custom_path <- fs::path(tmp, "custom-config.rds")

  withr::local_envvar(CUBICLE_CONFIG_PATH = custom_path)

  out <- .config_path()

  expect_equal(fs::path_norm(out), fs::path_norm(custom_path))
})

test_that(".read_config returns default config when file does not exist", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(c(CUBICLE_CONFIG_PATH = ""))

  with_mocked_bindings(
    {
      out <- .read_config()

      expect_equal(
        out,
        list(
          root = NULL,
          template = NULL,
          projects = list()
        )
      )
    },
    .user_config_dir = function() tmp
  )
})

test_that(".read_config merges incomplete config with defaults: root only", {
  tmp <- withr::local_tempdir()

  with_mocked_bindings(
    {
      saveRDS(list(root = "C:/projects"), .config_path())
      out <- .read_config()

      expect_equal(out$root, "C:/projects")
      expect_null(out$template)
      expect_equal(out$projects, list())
    },
    .user_config_dir = function() tmp
  )
})

test_that(".read_config merges incomplete config with defaults: template only", {
  tmp <- withr::local_tempdir()

  with_mocked_bindings(
    {
      saveRDS(list(template = "C:/template"), .config_path())
      out <- .read_config()

      expect_null(out$root)
      expect_equal(out$template, "C:/template")
      expect_equal(out$projects, list())
    },
    .user_config_dir = function() tmp
  )
})

test_that(".read_config merges incomplete config with defaults: projects only", {
  tmp <- withr::local_tempdir()
  projects <- list(list(name = "proj-a", path = "a"))

  with_mocked_bindings(
    {
      saveRDS(list(projects = projects), .config_path())
      out <- .read_config()

      expect_null(out$root)
      expect_null(out$template)
      expect_equal(out$projects, projects)
    },
    .user_config_dir = function() tmp
  )
})

test_that(".write_config writes readable config to disk", {
  tmp <- withr::local_tempdir()
  config <- list(
    root = "C:/projects",
    template = "C:/templates/cubicle",
    projects = list()
  )

  with_mocked_bindings(
    {
      .write_config(config)
      out <- .read_config()

      expect_equal(out, config)
    },
    .user_config_dir = function() tmp
  )
})

test_that(".write_config creates config.rds file", {
  tmp <- withr::local_tempdir()
  config <- list(
    root = "C:/projects",
    template = NULL,
    projects = list()
  )

  with_mocked_bindings(
    {
      path <- .config_path()
      expect_invisible(.write_config(config))
      expect_true(fs::file_exists(path))
    },
    .user_config_dir = function() tmp
  )
})

test_that("show_config returns session/config values invisibly", {
  fake_config <- list(
    root = "C:/saved/root",
    template = "C:/saved/template",
    projects = list()
  )

  withr::local_options(
    list(
      cubicle.root = "C:/session/root",
      cubicle.template = "C:/session/template"
    )
  )

  out <- with_mocked_bindings(
    show_config(),
    .read_config = function() fake_config
  )

  expect_type(out, "list")
  expect_named(
    out,
    c("session_root", "config_root", "session_template", "config_template")
  )
  expect_equal(out$session_root, "C:/session/root")
  expect_equal(out$config_root, "C:/saved/root")
  expect_equal(out$session_template, "C:/session/template")
  expect_equal(out$config_template, "C:/saved/template")
})

test_that("show_config handles NULL values", {
  fake_config <- list(
    root = NULL,
    template = NULL,
    projects = list()
  )

  withr::local_options(
    list(
      cubicle.root = NULL,
      cubicle.template = NULL
    )
  )

  out <- with_mocked_bindings(
    show_config(),
    .read_config = function() fake_config
  )

  expect_null(out$session_root)
  expect_null(out$config_root)
  expect_null(out$session_template)
  expect_null(out$config_template)
})

test_that("show_config returns invisibly", {
  fake_config <- list(
    root = NULL,
    template = NULL,
    projects = list()
  )

  expect_invisible(
    with_mocked_bindings(
      show_config(),
      .read_config = function() fake_config
    )
  )
})
