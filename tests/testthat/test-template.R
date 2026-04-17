
test_that(
  "get_template: session template returned when available",
  {
    tmp <- withr::local_tempdir()
    withr::local_options(
      list(cubicle.template = tmp)
    )

    out <- with_mocked_bindings(
      get_template(),
      .read_config = function() {
        list(
          root     = NULL,
          template = "C:/saved/template"
        )
      }
    )

    expect_equal(out, tmp)
  }
)



test_that(
  "get_template: saved template returns when session template missing",
  {
    tmp <- withr::local_tempdir()
    withr::local_options(
      list(cubicle.template = NULL)
    )

    out <- with_mocked_bindings(
      get_template(),
      .read_config = function() {
        list(
          root     = NULL,
          template = tmp
        )
      }
    )

    expect_equal(out, tmp)
  }
)



test_that(
  "get_template: default returned when session/saved template missing",
  {
    tmp <- withr::local_tempdir()
    withr::local_options(
      list(cubicle.template = NULL)
    )

    out <- with_mocked_bindings(
      get_template(),
      .read_config = function() {
        list(
          root     = NULL,
          template = NULL
        )
      },
      .default_template_path = function() tmp
    )

    expect_equal(out, tmp)
  }
)



test_that(
  "get_template: error; no default package template",
  {
    withr::local_options(
      list(cubicle.template = NULL)
    )

    with_mocked_bindings(
      expect_error(
        get_template(),
        "Default package template could not be found"
      ),
      .read_config = function() {
        list(
          root     = NULL,
          template = NULL
        )
      },
      .default_template_path = function() ""
    )
  }
)



test_that(
  "show_template: invisible and runs without error",
  {
    tmp <- withr::local_tempdir()

    expect_invisible(
      with_mocked_bindings(
        show_template(),
        get_template = function() tmp
      )
    )
  }
)



test_that(
  "set_template: error; directory does not exist",
  {
    expect_error(
      set_template("C:/does/not/exist"),
      "Template directory does not exist"
    )
  }
)



test_that(
  "set_template: session template set/configuration written when [save] is TRUE",
  {
    tmp <- withr::local_tempdir()
    check <- NULL

    withr::local_options(
      list(cubicle.template = NULL)
    )

    expect_invisible(
      with_mocked_bindings(
        set_template(tmp, save = TRUE),
        .read_config = function() {
          list(
            root     = "C:/saved/root",
            template = NULL
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
    expect_equal(check$root,        "C:/saved/root")
  }
)



test_that(
  "set_template: session template set/configuration not written when [save] is FALSE",
  {
    tmp <- withr::local_tempdir()
    check <- FALSE

    withr::local_options(
      list(cubicle.template = NULL)
    )

    expect_invisible(
      with_mocked_bindings(
        set_template(tmp, save = FALSE),
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

    expect_equal(getOption("cubicle.template"), fs::path_norm(tmp))
    expect_false(check)
  }
)



test_that(
  "reset_template: session template/configuration deleted when [save] is TRUE",
  {
    check <- NULL
    withr::local_options(
      list(cubicle.template = "C:/session/template")
    )

    expect_invisible(
      with_mocked_bindings(
        reset_template(save = TRUE),
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

    expect_null(getOption("cubicle.template"))
    expect_null(check$template)
    expect_equal(check$root, "C:/saved/root")
  }
)



test_that(
  "reset_template: session template deleted only when [save] is FALSE",
  {
    check <- FALSE
    withr::local_options(
      list(cubicle.template = "C:/session/template")
    )

    expect_invisible(
      with_mocked_bindings(
        reset_template(save = FALSE),
        .read_config = function() {
          list(
            root     = NULL,
            template = "C:/saved/template"
          )
        },
        .write_config = function(config) {
          check <<- TRUE
          invisible(config)
        }
      )
    )

    expect_null(getOption("cubicle.template"))
    expect_false(check)
  }
)
