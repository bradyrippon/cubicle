
test_that(".clean_tag cleans tag text correctly", {
  expect_equal(.clean_tag("Tag"), "#tag")
  expect_equal(.clean_tag("  My Tag  "), "#my-tag")
  expect_equal(.clean_tag("#My Tag"), "#my-tag")
  expect_equal(.clean_tag("###My Tag"), "#my-tag")
  expect_equal(.clean_tag("Tag!! Name"), "#tag-name")
  expect_equal(.clean_tag("!!!"), "#")
})

test_that(".find_project_root returns current directory when .Rproj exists there", {
  tmp <- withr::local_tempdir()
  fs::file_create(fs::path(tmp, "testproj.Rproj"))

  out <- .find_project_root(start = tmp)

  expect_equal(out, .normalize_path(tmp))
})

test_that(".find_project_root searches parent directories", {
  tmp <- withr::local_tempdir()
  proj_root <- fs::path(tmp, "project")
  nested <- fs::path(proj_root, "code", "admin", "functions")

  fs::dir_create(nested, recurse = TRUE)
  fs::file_create(fs::path(proj_root, "testproj.Rproj"))

  out <- .find_project_root(start = nested)

  expect_equal(fs::path_norm(out), fs::path_norm(proj_root))
})

test_that(".find_project_root returns NULL when no .Rproj exists", {
  tmp <- withr::local_tempdir()
  nested <- fs::path(tmp, "a", "b", "c")

  fs::dir_create(nested, recurse = TRUE)

  out <- .find_project_root(start = nested)

  expect_null(out)
})

test_that(".active_project_root uses RStudio project when available", {
  tmp <- withr::local_tempdir()
  proj <- fs::path(tmp, "rstudio-project")
  fs::dir_create(proj)

  with_mocked_bindings(
    {
      out <- .active_project_root()
      expect_equal(out, .normalize_path(proj))
    },
    .get_active_rstudio_project = function() proj
  )
})

test_that(".active_project_root falls back to .find_project_root when RStudio unavailable", {
  tmp <- withr::local_tempdir()
  proj_root <- fs::path(tmp, "project")
  nested <- fs::path(proj_root, "code")

  fs::dir_create(nested, recurse = TRUE)
  fs::file_create(fs::path(proj_root, "testproj.Rproj"))

  old <- setwd(nested)
  on.exit(setwd(old), add = TRUE)

  with_mocked_bindings(
    {
      out <- .active_project_root()
      expect_equal(fs::path_norm(out), fs::path_norm(proj_root))
    },
    .get_active_rstudio_project = function() NULL
  )
})

test_that(".active_project_root returns NULL when no project found", {
  tmp <- withr::local_tempdir()
  nested <- fs::path(tmp, "code")
  fs::dir_create(nested)

  old <- setwd(nested)
  on.exit(setwd(old), add = TRUE)

  with_mocked_bindings(
    {
      out <- .active_project_root()
      expect_null(out)
    },
    .get_active_rstudio_project = function() NULL
  )
})

test_that("add_tag validates tag input", {
  expect_error(add_tag(""), "must be a non-empty character string")
  expect_error(add_tag(NA_character_), "must be a non-empty character string")
  expect_error(add_tag(1), "must be a non-empty character string")
  expect_error(add_tag(c("a", "b")), "must be a non-empty character string")
})

test_that("add_tag errors when cleaned tag is just #", {
  with_mocked_bindings(
    {
      expect_error(
        add_tag("!!!"),
        "Tag must contain at least one letter/number"
      )
    },
    .active_project_root = function() withr::local_tempdir()
  )
})

test_that("add_tag errors when active project root cannot be determined", {
  with_mocked_bindings(
    {
      expect_error(
        add_tag("test"),
        "Could not determine the current root"
      )
    },
    .active_project_root = function() NULL
  )
})

test_that("add_tag creates tags directory and writes tag file", {
  tmp <- withr::local_tempdir()

  with_mocked_bindings(
    {
      out <- add_tag("Test Tag")

      expect_true(fs::dir_exists(fs::path(tmp, "tags")))
      expect_true(fs::file_exists(out))
      expect_equal(fs::path_file(out), "#test-tag.txt")
    },
    .active_project_root = function() tmp
  )
})

test_that("add_tag returns tag file path invisibly", {
  tmp <- withr::local_tempdir()

  expect_invisible(
    with_mocked_bindings(
      add_tag("Another Tag"),
      .active_project_root = function() tmp
    )
  )
})

test_that("add_tag can be called twice for the same tag", {
  tmp <- withr::local_tempdir()

  with_mocked_bindings(
    {
      out1 <- add_tag("Same Tag")
      out2 <- add_tag("Same Tag")

      expect_equal(out1, out2)
      expect_true(fs::file_exists(out1))
    },
    .active_project_root = function() tmp
  )
})
