
test_that("register_project registers directory path", {
  tmp <- withr::local_tempdir()
  config_file <- fs::path(tmp, "config.rds")
  withr::local_envvar(CUBICLE_CONFIG_PATH = config_file)

  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)

  out <- register_project(path = proj)

  projects <- .get_registered_projects()

  expect_equal(out, fs::path_norm(proj))
  expect_length(projects, 1)
  expect_equal(projects[[1]]$name, "proj-a")
  expect_equal(projects[[1]]$path, fs::path_norm(proj))
})

test_that("register_project registers from .Rproj file path", {
  tmp <- withr::local_tempdir()
  config_file <- fs::path(tmp, "config.rds")
  withr::local_envvar(CUBICLE_CONFIG_PATH = config_file)

  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)
  rproj <- fs::path(proj, "proj-a.Rproj")
  fs::file_create(rproj)

  out <- register_project(path = rproj)

  projects <- .get_registered_projects()

  expect_equal(out, fs::path_norm(proj))
  expect_length(projects, 1)
  expect_equal(projects[[1]]$name, "proj-a")
  expect_equal(projects[[1]]$path, fs::path_norm(proj))
})

test_that("register_project uses active project root when path = NULL", {
  tmp <- withr::local_tempdir()
  config_file <- fs::path(tmp, "config.rds")
  withr::local_envvar(CUBICLE_CONFIG_PATH = config_file)

  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)

  with_mocked_bindings(
    {
      out <- register_project(path = NULL)

      projects <- .get_registered_projects()
      expect_equal(out, fs::path_norm(proj))
      expect_length(projects, 1)
      expect_equal(projects[[1]]$name, "proj-a")
    },
    .active_project_root = function() proj
  )
})

test_that("register_project errors when path = NULL and no active project found", {
  with_mocked_bindings(
    {
      expect_error(
        register_project(path = NULL),
        "Could not determine `path`/active R project"
      )
    },
    .active_project_root = function() NULL
  )
})

test_that("register_project passes through group and timestamp", {
  tmp <- withr::local_tempdir()
  captured <- NULL

  with_mocked_bindings(
    {
      out <- register_project(
        path = tmp,
        group = "analysis",
        timestamp = as.Date("2026-01-01")
      )

      expect_equal(out, fs::path_norm(tmp))
    },
    .register_project = function(name, path, timestamp = NULL, group = NULL) {
      captured <<- list(
        name = name,
        path = path,
        timestamp = timestamp,
        group = group
      )
      invisible(NULL)
    }
  )

  expect_equal(captured$name, fs::path_file(fs::path_norm(tmp)))
  expect_equal(captured$path, fs::path_norm(tmp))
  expect_equal(captured$timestamp, as.Date("2026-01-01"))
  expect_equal(captured$group, "analysis")
})

test_that(".register_project adds new project to empty registry", {
  tmp <- withr::local_tempdir()
  config_file <- fs::path(tmp, "config.rds")
  withr::local_envvar(CUBICLE_CONFIG_PATH = config_file)

  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)

  out <- .register_project(
    name = "proj-a",
    path = proj,
    timestamp = as.Date("2026-01-01"),
    group = "analysis"
  )

  projects <- .get_registered_projects()

  expect_equal(out$name, "proj-a")
  expect_equal(out$path, fs::path_norm(proj))
  expect_equal(out$timestamp, "2026-01-01")
  expect_equal(out$group, "analysis")
  expect_length(projects, 1)
})

test_that(".register_project appends distinct projects", {
  tmp <- withr::local_tempdir()
  config_file <- fs::path(tmp, "config.rds")
  withr::local_envvar(CUBICLE_CONFIG_PATH = config_file)

  proj1 <- fs::path(tmp, "proj-a")
  proj2 <- fs::path(tmp, "proj-b")
  fs::dir_create(proj1)
  fs::dir_create(proj2)

  .register_project(name = "proj-a", path = proj1)
  .register_project(name = "proj-b", path = proj2)

  projects <- .get_registered_projects()

  expect_length(projects, 2)
  expect_equal(projects[[1]]$name, "proj-a")
  expect_equal(projects[[2]]$name, "proj-b")
})

test_that(".register_project overwrites existing entry for same path", {
  tmp <- withr::local_tempdir()
  config_file <- fs::path(tmp, "config.rds")
  withr::local_envvar(CUBICLE_CONFIG_PATH = config_file)

  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)

  .register_project(name = "proj-a", path = proj, group = "old")
  .register_project(name = "proj-a-new", path = proj, group = "new")

  projects <- .get_registered_projects()

  expect_length(projects, 1)
  expect_equal(projects[[1]]$name, "proj-a-new")
  expect_equal(projects[[1]]$group, "new")
})

test_that(".register_project defaults timestamp and group", {
  tmp <- withr::local_tempdir()
  config_file <- fs::path(tmp, "config.rds")
  withr::local_envvar(CUBICLE_CONFIG_PATH = config_file)

  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)

  out <- .register_project(name = "proj-a", path = proj)

  expect_equal(out$timestamp, as.character(Sys.Date()))
  expect_null(out$group)
})

test_that(".save_registered_projects replaces projects and preserves root/template", {
  check <- NULL

  with_mocked_bindings(
    {
      projects <- list(list(name = "proj-a", path = "a"))
      out <- .save_registered_projects(projects)

      expect_equal(out, projects)
    },
    .read_config = function() {
      list(
        root = "C:/root",
        template = "C:/template",
        projects = list(list(name = "old", path = "old"))
      )
    },
    .write_config = function(config) {
      check <<- config
      invisible(config)
    }
  )

  expect_equal(check$root, "C:/root")
  expect_equal(check$template, "C:/template")
  expect_equal(check$projects, list(list(name = "proj-a", path = "a")))
})

test_that(".path_components splits path into non-empty components", {
  x <- fs::path("projects", "analysis", "proj-a")
  out <- .path_components(x)

  expect_true(length(out) >= 3)
  expect_equal(utils::tail(out, 3), c("projects", "analysis", "proj-a"))
})

test_that(".find_project_index matches by unique name", {
  projects <- list(
    list(name = "proj-a", path = "a"),
    list(name = "proj-b", path = "b")
  )

  out <- with_mocked_bindings(
    .find_project_index("proj-b"),
    .get_registered_projects = function() projects
  )

  expect_equal(out, 2)
})

test_that(".find_project_index matches by full path", {
  tmp <- withr::local_tempdir()
  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)

  projects <- list(
    list(name = "proj-a", path = fs::path_norm(proj))
  )

  out <- with_mocked_bindings(
    .find_project_index(proj),
    .get_registered_projects = function() projects
  )

  expect_equal(out, 1)
})

test_that(".find_project_index errors when no registered projects exist", {
  with_mocked_bindings(
    {
      expect_error(
        .find_project_index("proj-a"),
        "No registered projects were found"
      )
    },
    .get_registered_projects = function() list()
  )
})

test_that(".find_project_index errors when path exists but is not registered", {
  tmp <- withr::local_tempdir()
  proj <- fs::path(tmp, "proj-a")
  fs::dir_create(proj)

  with_mocked_bindings(
    {
      expect_error(
        .find_project_index(proj),
        "That path is not currently registered in cubicle"
      )
    },
    .get_registered_projects = function() list(list(name = "other", path = fs::path(tmp, "other")))
  )
})

test_that(".find_project_index errors when no project found by name", {
  with_mocked_bindings(
    {
      expect_error(
        .find_project_index("proj-a"),
        "No project found with name"
      )
    },
    .get_registered_projects = function() list(list(name = "proj-b", path = "b"))
  )
})

test_that(".find_project_index errors when duplicate names exist", {
  projects <- list(
    list(name = "proj", path = "a"),
    list(name = "proj", path = "b")
  )

  with_mocked_bindings(
    {
      expect_error(
        .find_project_index("proj"),
        "Multiple registered projects are named"
      )
    },
    .get_registered_projects = function() projects
  )
})

test_that(".find_project_path returns registered path", {
  projects <- list(
    list(name = "proj-a", path = "a"),
    list(name = "proj-b", path = "b")
  )

  out <- with_mocked_bindings(
    .find_project_path("proj-b"),
    .read_config = function() list(root = NULL, template = NULL, projects = projects),
    .find_project_index = function(project) 2
  )

  expect_equal(out, "b")
})

test_that(".find_rproj returns direct .Rproj path when supplied", {
  tmp <- withr::local_tempdir()
  proj_file <- fs::path(tmp, "proj-a.Rproj")
  fs::file_create(proj_file)

  out <- .find_rproj(proj_file)

  expect_equal(out, fs::path_norm(proj_file))
})

test_that(".find_rproj returns first .Rproj file in directory", {
  tmp <- withr::local_tempdir()
  proj_file <- fs::path(tmp, "proj-a.Rproj")
  fs::file_create(proj_file)

  out <- .find_rproj(tmp)

  expect_equal(out, fs::path_norm(proj_file))
})

test_that(".find_rproj returns directory when no .Rproj file found inside", {
  tmp <- withr::local_tempdir()

  out <- .find_rproj(tmp)

  expect_equal(out, fs::path_norm(tmp))
})

test_that(".find_rproj errors when supplied path does not exist", {
  tmp <- withr::local_tempdir()
  path <- fs::path(tmp, "does-not-exist")

  expect_error(
    .find_rproj(path),
    "Could not locate a valid project"
  )
})
