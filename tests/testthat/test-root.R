
test_that("get_root returns session root when available", {
  withr::local_options(list(cubicle.root = "C:/session/root"))

  out <- with_mocked_bindings(
    get_root(),
    .read_config = function() {
      list(
        root = "C:/saved/root",
        template = NULL,
        projects = list()
      )
    }
  )

  expect_equal(out, "C:/session/root")
})

test_that("get_root falls back to config root when session root missing", {
  withr::local_options(list(cubicle.root = NULL))

  out <- with_mocked_bindings(
    get_root(),
    .read_config = function() {
      list(
        root = "C:/saved/root",
        template = NULL,
        projects = list()
      )
    }
  )

  expect_equal(out, "C:/saved/root")
})

test_that("get_root returns NULL when no root exists", {
  withr::local_options(list(cubicle.root = NULL))

  out <- with_mocked_bindings(
    get_root(),
    .read_config = function() {
      list(
        root = NULL,
        template = NULL,
        projects = list()
      )
    }
  )

  expect_null(out)
})

test_that("set_root errors when directory does not exist", {
  expect_error(
    set_root("C:/does/not/exist"),
    "Directory does not exist"
  )
})

test_that("set_root updates session option and config when save = TRUE", {
  tmp <- withr::local_tempdir()
  check <- NULL

  withr::local_options(list(cubicle.root = NULL))

  expect_invisible(
    with_mocked_bindings(
      set_root(tmp, save = TRUE),
      .read_config = function() {
        list(
          root = NULL,
          template = "C:/saved/template",
          projects = list(list(name = "proj-a", path = "a"))
        )
      },
      .write_config = function(config) {
        check <<- config
        invisible(config)
      }
    )
  )

  expect_equal(getOption("cubicle.root"), fs::path_norm(tmp))
  expect_equal(check$root, fs::path_norm(tmp))
  expect_equal(check$template, "C:/saved/template")
  expect_equal(check$projects, list(list(name = "proj-a", path = "a")))
})

test_that("set_root updates session option only when save = FALSE", {
  tmp <- withr::local_tempdir()
  wrote <- FALSE

  withr::local_options(list(cubicle.root = NULL))

  expect_invisible(
    with_mocked_bindings(
      set_root(tmp, save = FALSE),
      .write_config = function(config) {
        wrote <<- TRUE
        invisible(config)
      }
    )
  )

  expect_equal(getOption("cubicle.root"), fs::path_norm(tmp))
  expect_false(wrote)
})

test_that("reset_root clears session option and config when save = TRUE", {
  check <- NULL

  withr::local_options(list(cubicle.root = "C:/session/root"))

  expect_invisible(
    with_mocked_bindings(
      reset_root(save = TRUE),
      .read_config = function() {
        list(
          root = "C:/saved/root",
          template = "C:/saved/template",
          projects = list(list(name = "proj-a", path = "a"))
        )
      },
      .write_config = function(config) {
        check <<- config
        invisible(config)
      }
    )
  )

  expect_null(getOption("cubicle.root"))
  expect_null(check$root)
  expect_equal(check$template, "C:/saved/template")
  expect_equal(check$projects, list(list(name = "proj-a", path = "a")))
})

test_that("reset_root clears session option only when save = FALSE", {
  wrote <- FALSE

  withr::local_options(list(cubicle.root = "C:/session/root"))

  expect_invisible(
    with_mocked_bindings(
      reset_root(save = FALSE),
      .write_config = function(config) {
        wrote <<- TRUE
        invisible(config)
      }
    )
  )

  expect_null(getOption("cubicle.root"))
  expect_false(wrote)
})
