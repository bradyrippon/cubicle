
make_temp_template_build_project <- function(path) {
  fs::dir_create(path)

  dirs <- c(
    "code",
    "code/admin",
    "code/admin/functions",
    "data",
    "data/raw",
    "documents",
    "documents/prints",
    "documents/prints/published",
    "documents/records",
    "documents/records/published",
    "documents/zips",
    "figures",
    "notes",
    "reports",
    "tags"
  )

  for (x in fs::path(path, dirs)) {
    fs::dir_create(x)
  }

  writeLines("ignore", fs::path(path, ".gitignore"))
  writeLines("Version: 1.0", fs::path(path, "proj.Rproj"))
  writeLines("# libs", fs::path(path, "code", "admin", "libs.R"))

  placeholder_dirs <- c(
    "code/admin/functions",
    "data/raw",
    "documents/prints/published",
    "documents/records/published",
    "documents/zips",
    "figures",
    "notes",
    "reports",
    "tags"
  )

  for (x in fs::path(path, placeholder_dirs, ".cubicle-temp")) {
    writeLines("", x)
  }

  rproj_user_dir <- fs::path(path, ".Rproj.user")
  fs::dir_create(rproj_user_dir)
  writeLines("temp", fs::path(rproj_user_dir, "should_not_copy.txt"))
}

local_mock_template_build_project <- function(env = parent.frame()) {
  template_dir <- withr::local_tempdir(.local_envir = env)
  make_temp_template_build_project(template_dir)
  template_dir
}

test_that("build_project validates required arguments", {
  tmp <- withr::local_tempdir()

  expect_error(build_project(name = NULL, path = tmp), "`name` must be a non-empty character string")
  expect_error(build_project(name = "", path = tmp), "`name` must be a non-empty character string")
  expect_error(build_project(name = NA_character_, path = tmp), "`name` must be a non-empty character string")
  expect_error(build_project(name = 1, path = tmp), "`name` must be a non-empty character string")

  expect_error(build_project(name = "x", path = NULL), "`path` must be a non-empty character string")
  expect_error(build_project(name = "x", path = ""), "`path` must be a non-empty character string")
  expect_error(build_project(name = "x", path = NA_character_), "`path` must be a non-empty character string")
  expect_error(build_project(name = "x", path = 1), "`path` must be a non-empty character string")
})

test_that("build_project validates optional arguments", {
  tmp <- withr::local_tempdir()

  expect_error(
    build_project(name = "x", path = tmp, root = 1),
    "`root` must be `NULL` or a non-empty character string"
  )

  expect_error(
    build_project(name = "x", path = tmp, group = 1),
    "`group` must be `NULL` or a non-empty character string"
  )

  expect_error(
    build_project(name = "x", path = tmp, register = NA),
    "`register` must be `TRUE` or `FALSE`"
  )

  expect_error(
    build_project(name = "x", path = tmp, load_project = "TRUE"),
    "`load_project` must be `TRUE` or `FALSE`"
  )

  expect_error(
    build_project(name = "x", path = tmp, use_name = 1),
    "`use_name` must be `TRUE` or `FALSE`"
  )

  expect_error(
    build_project(name = "x", path = tmp, append = ""),
    "`append` must be `NULL` or a non-empty character string"
  )
})

test_that("build_project creates full project structure", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE
    ),
    get_template = function() template_dir
  )

  expect_true(fs::dir_exists(out))
  expect_equal(fs::path_file(out), "Test Project")

  expect_true(fs::dir_exists(fs::path(out, "code")))
  expect_true(fs::dir_exists(fs::path(out, "code", "admin")))
  expect_true(fs::dir_exists(fs::path(out, "code", "admin", "functions")))
  expect_true(fs::dir_exists(fs::path(out, "data")))
  expect_true(fs::dir_exists(fs::path(out, "data", "raw")))
  expect_true(fs::dir_exists(fs::path(out, "documents")))
  expect_true(fs::dir_exists(fs::path(out, "documents", "prints")))
  expect_true(fs::dir_exists(fs::path(out, "documents", "prints", "published")))
  expect_true(fs::dir_exists(fs::path(out, "documents", "records")))
  expect_true(fs::dir_exists(fs::path(out, "documents", "records", "published")))
  expect_true(fs::dir_exists(fs::path(out, "documents", "zips")))
  expect_true(fs::dir_exists(fs::path(out, "figures")))
  expect_true(fs::dir_exists(fs::path(out, "notes")))
  expect_true(fs::dir_exists(fs::path(out, "reports")))
  expect_true(fs::dir_exists(fs::path(out, "tags")))

  expect_true(fs::file_exists(fs::path(out, ".gitignore")))
  expect_true(fs::file_exists(fs::path(out, "proj.Rproj")))
  expect_true(fs::file_exists(fs::path(out, "code", "admin", "libs.R")))
})

test_that("build_project excludes .Rproj.user from copied contents", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE
    ),
    get_template = function() template_dir
  )

  expect_false(fs::dir_exists(fs::path(out, ".Rproj.user")))
})

test_that("build_project removes placeholder files from created project", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE
    ),
    get_template = function() template_dir
  )

  placeholders <- fs::dir_ls(
    path = out,
    all = TRUE,
    recurse = TRUE,
    regexp = "\\.cubicle-temp$"
  )

  expect_length(placeholders, 0)
})

test_that("build_project keeps notes directory and does not expect notes.docx", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE
    ),
    get_template = function() template_dir
  )

  expect_true(fs::dir_exists(fs::path(out, "notes")))
  expect_false(fs::file_exists(fs::path(out, "notes.docx")))
})

test_that("build_project uses exact folder name supplied by user", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE
    ),
    get_template = function() template_dir
  )

  expect_equal(fs::path_file(out), "Test Project")
})

test_that("build_project renames Rproj file when use_name = TRUE", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE,
      use_name = TRUE
    ),
    get_template = function() template_dir
  )

  expect_true(fs::file_exists(fs::path(out, "test_project.Rproj")))
  expect_false(fs::file_exists(fs::path(out, "proj.Rproj")))
})

test_that("build_project appends suffix to Rproj when use_name = FALSE", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE,
      use_name = FALSE,
      append = "Draft 1!"
    ),
    get_template = function() template_dir
  )

  expect_true(fs::file_exists(fs::path(out, "proj-draft_1.Rproj")))
  expect_false(fs::file_exists(fs::path(out, "proj.Rproj")))
})

test_that("build_project appends suffix to cleaned project-name Rproj when use_name = TRUE", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE,
      use_name = TRUE,
      append = "Draft 1!"
    ),
    get_template = function() template_dir
  )

  expect_true(fs::file_exists(fs::path(out, "test_project-draft_1.Rproj")))
  expect_false(fs::file_exists(fs::path(out, "proj.Rproj")))
  expect_false(fs::file_exists(fs::path(out, "test_project.Rproj")))
})

test_that("build_project append does not create notes file when none exists in template", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = tmp,
      register = FALSE,
      append = "Draft 1!"
    ),
    get_template = function() template_dir
  )

  expect_false(any(grepl("^notes.*\\.docx$", fs::dir_ls(out, type = "file", recurse = TRUE))))
})

test_that("build_project errors when target already exists as directory", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()
  fs::dir_create(fs::path(tmp, "Test Project"))

  with_mocked_bindings(
    expect_error(
      build_project(
        name = "Test Project",
        path = tmp,
        register = FALSE
      ),
      "Target project already exists"
    ),
    get_template = function() template_dir
  )
})

test_that("build_project errors when target already exists as file", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()
  fs::file_create(fs::path(tmp, "Test Project"))

  with_mocked_bindings(
    expect_error(
      build_project(
        name = "Test Project",
        path = tmp,
        register = FALSE
      ),
      "Target project already exists"
    ),
    get_template = function() template_dir
  )
})

test_that("build_project works with relative path and explicit root", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()

  out <- with_mocked_bindings(
    build_project(
      name = "Test Project",
      path = "projects",
      root = tmp,
      register = FALSE
    ),
    get_template = function() template_dir
  )

  expect_equal(out, fs::path(tmp, "projects", "Test Project"))
  expect_true(fs::dir_exists(out))
})

test_that("build_project calls .register_project when register = TRUE", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()
  captured <- NULL

  with_mocked_bindings(
    {
      expect_invisible(
        build_project(
          name = "Test Project",
          path = tmp,
          group = "analysis",
          register = TRUE
        )
      )
    },
    get_template = function() template_dir,
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

  expect_equal(captured$name, "Test Project")
  expect_equal(captured$path, fs::path(tmp, "Test Project"))
  expect_equal(captured$group, "analysis")
  expect_equal(captured$timestamp, Sys.Date())
})

test_that("build_project does not call .register_project when register = FALSE", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()
  called <- FALSE

  with_mocked_bindings(
    {
      expect_invisible(
        build_project(
          name = "Test Project",
          path = tmp,
          register = FALSE
        )
      )
    },
    get_template = function() template_dir,
    .register_project = function(...) {
      called <<- TRUE
      invisible(NULL)
    }
  )

  expect_false(called)
})

test_that("build_project opens project in RStudio when load_project = TRUE and available", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()
  opened <- NULL

  with_mocked_bindings(
    {
      expect_invisible(
        build_project(
          name = "Test Project",
          path = tmp,
          register = FALSE,
          load_project = TRUE
        )
      )
    },
    get_template = function() template_dir,
    .is_rstudio_available = function() TRUE,
    .open_rstudio_project = function(path) {
      opened <<- path
      invisible(path)
    }
  )

  expect_true(grepl("\\.Rproj$", opened))
})

test_that("build_project does not open project when RStudio unavailable", {
  tmp <- withr::local_tempdir()
  template_dir <- local_mock_template_build_project()
  opened <- FALSE

  with_mocked_bindings(
    {
      expect_invisible(
        build_project(
          name = "Test Project",
          path = tmp,
          register = FALSE,
          load_project = TRUE
        )
      )
    },
    get_template = function() template_dir,
    .is_rstudio_available = function() FALSE,
    .open_rstudio_project = function(path) {
      opened <<- path
      invisible(path)
    }
  )

  expect_false(opened)
})
