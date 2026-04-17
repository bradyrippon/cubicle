
test_that(
  "get_root: session root returned when available",
  {
    withr::local_options(
      list(cubicle.root = "C:/session/root")
    )

    out <- with_mocked_bindings(
      get_root(),
      .read_config = function() {
        list(root = "C:/saved/root", template = NULL)
      }
    )

    expect_equal(out, "C:/session/root")
  }
)



test_that(
  "get_root: saved root returns when session root missing",
  {
    withr::local_options(
      list(cubicle.root = NULL)
    )

    out <- with_mocked_bindings(
      get_root(),
      .read_config = function() {
        list(
          root     = "C:/saved/root",
          template = NULL
        )
      }
    )

    expect_equal(out, "C:/saved/root")
  }
)



test_that(
  "get_root: NULL returned when root exists",
  {
    withr::local_options(
      list(cubicle.root = NULL)
    )

    out <- with_mocked_bindings(
      get_root(),
      .read_config = function() {
        list(
          root     = NULL,
          template = NULL
        )
      }
    )

    expect_null(out)
  }
)



test_that(
  "set_root: error; directory does not exist",
  {
    expect_error(
      set_root("C:/does/not/exist"),
      "Directory does not exist"
    )
  }
)



test_that(
  "set_root: session root set/configuration written when [save] is TRUE",
  {
    tmp   <- withr::local_tempdir()
    check <- NULL

    withr::local_options(
      list(cubicle.root = NULL)
    )

    expect_invisible(
      with_mocked_bindings(
        set_root(tmp, save = TRUE),
        .read_config = function() {
          list(
            root     = NULL,
            template = "C:/saved/template"
          )
        },
        .write_config = function(config) {
          check <<- config
          invisible(config)
        }
      )
    )

    expect_equal(getOption("cubicle.root"), fs::path_norm(tmp))
    expect_equal(check$root,      fs::path_norm(tmp))
    expect_equal(check$template, "C:/saved/template")
  }
)



test_that(
  "set_root: session root set/configuration not written when [save] is FALSE",
  {
    tmp   <- withr::local_tempdir()
    check <- FALSE

    withr::local_options(
      list(cubicle.root = NULL)
    )

    expect_invisible(
      with_mocked_bindings(
        set_root(tmp, save = FALSE),
        .read_config = function() {
          list(
            root     = NULL,
            template = NULL
          )
        },
        .write_config = function(config) {
          check <<- TRUE
          invisible(config)
        }
      )
    )

    expect_equal(getOption("cubicle.root"), fs::path_norm(tmp))
    expect_false(check)
  }
)



test_that(
  "reset_root: session root/configuration deleted when [save] is TRUE",
  {
    check <- NULL
    withr::local_options(
      list(cubicle.root = "C:/session/root")
    )

    expect_invisible(
      with_mocked_bindings(
        reset_root(save = TRUE),
        .read_config = function() {
          list(
            root     = "C:/saved/root",
            template = "C:/saved/template"
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
  }
)


test_that(
  "reset_root: session root deleted only when [save] is FALSE",
  {
    check <- FALSE
    withr::local_options(
      list(cubicle.root = "C:/session/root")
    )

    expect_invisible(
      with_mocked_bindings(
        reset_root(save = FALSE),
        .read_config = function() {
          list(
            root     = "C:/saved/root",
            template = NULL
          )
        },
        .write_config = function(config) {
          check <<- TRUE
          invisible(config)
        }
      )
    )

    expect_null(getOption("cubicle.root"))
    expect_false(check)
  }
)
