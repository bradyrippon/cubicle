
test_that(".default_template_path returns a character path", {
  out <- .default_template_path()

  expect_type(out, "character")
  expect_length(out, 1)
})

test_that("get_template returns session template when it exists", {
  tmp <- withr::local_tempdir()

  withr::local_options(list(cubicle.template = tmp))

  out <- with_mocked_bindings(
    get_template(),
    .read_config = function() {
      list(
        root = NULL,
        template = "C:/saved/template",
        projects = list()
      )
    }
  )

  expect_equal(out, tmp)
})

test_that("get_template ignores missing session template and uses config template", {
  tmp <- withr::local_tempdir()

  withr::local_options(list(cubicle.template = "C:/missing/template"))

  out <- with_mocked_bindings(
    get_template(),
    .read_config = function() {
      list(
        root = NULL,
        template = tmp,
        projects = list()
      )
    }
  )

  expect_equal(out, tmp)
})

test_that("get_template falls back to default template", {
  tmp <- withr::local_tempdir()

  withr::local_options(list(cubicle.template = NULL))

  out <- with_mocked_bindings(
    get_template(),
    .read_config = function() {
      list(
        root = NULL,
        template = NULL,
        projects = list()
      )
    },
    .default_template_path = function() tmp
  )

  expect_equal(out, tmp)
})

test_that("get_template errors when default template missing", {
  withr::local_options(list(cubicle.template = NULL))

  with_mocked_bindings(
    expect_error(
      get_template(),
      "Default package template could not be found"
    ),
    .read_config = function() {
      list(
        root = NULL,
        template = NULL,
        projects = list()
      )
    },
    .default_template_path = function() ""
  )
})

test_that("show_template runs and returns invisibly", {
  tmp <- withr::local_tempdir()

  expect_invisible(
    with_mocked_bindings(
      show_template(),
      get_template = function() tmp
    )
  )
})

test_that("set_template errors when directory does not exist", {
  expect_error(
    set_template("C:/does/not/exist"),
    "Template directory does not exist"
  )
})

test_that("set_template updates session option and config when save = TRUE", {
  tmp <- withr::local_tempdir()
  check <- NULL

  withr::local_options(list(cubicle.template = NULL))

  expect_invisible(
    with_mocked_bindings(
      set_template(tmp, save = TRUE),
      .read_config = function() {
        list(
          root = "C:/saved/root",
          template = NULL,
          projects = list(list(name = "proj-a", path = "a"))
        )
      },
      .write_config = function(config) {
        check <<- config
        invisible(config)
      }
    )
  )

  expect_equal(getOption("cubicle.template"), fs::path_norm(tmp))
  expect_equal(check$template, fs::path_norm(tmp))
  expect_equal(check$root, "C:/saved/root")
  expect_equal(check$projects, list(list(name = "proj-a", path = "a")))
})

test_that("set_template updates session option only when save = FALSE", {
  tmp <- withr::local_tempdir()
  wrote <- FALSE

  withr::local_options(list(cubicle.template = NULL))

  expect_invisible(
    with_mocked_bindings(
      set_template(tmp, save = FALSE),
      .write_config = function(config) {
        wrote <<- TRUE
        invisible(config)
      }
    )
  )

  expect_equal(getOption("cubicle.template"), fs::path_norm(tmp))
  expect_false(wrote)
})

test_that("reset_template clears session option and config when save = TRUE", {
  check <- NULL

  withr::local_options(list(cubicle.template = "C:/session/template"))

  expect_invisible(
    with_mocked_bindings(
      reset_template(save = TRUE),
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

  expect_null(getOption("cubicle.template"))
  expect_null(check$template)
  expect_equal(check$root, "C:/saved/root")
  expect_equal(check$projects, list(list(name = "proj-a", path = "a")))
})

test_that("reset_template clears session option only when save = FALSE", {
  wrote <- FALSE

  withr::local_options(list(cubicle.template = "C:/session/template"))

  expect_invisible(
    with_mocked_bindings(
      reset_template(save = FALSE),
      .write_config = function(config) {
        wrote <<- TRUE
        invisible(config)
      }
    )
  )

  expect_null(getOption("cubicle.template"))
  expect_false(wrote)
})
