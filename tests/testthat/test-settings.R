
test_that(
  "cubicle_settings: session/saved settings returned",
  {
    fake_config <- list(
      root     = "C:/saved/root",
      template = "C:/saved/template"
    )

    withr::local_options(
      list(
        cubicle.root     = "C:/session/root",
        cubicle.template = "C:/session/template"
      )
    )

    out <- with_mocked_bindings(
      cubicle_settings(),
      .read_config = function() fake_config
    )

    expect_type(out, "list")
    expect_named(
      out,
      c(
        "session_root",
        "saved_root",
        "session_template",
        "saved_template"
      )
    )

    expect_equal(out$session_root,     "C:/session/root")
    expect_equal(out$saved_root,       "C:/saved/root")
    expect_equal(out$session_template, "C:/session/template")
    expect_equal(out$saved_template,   "C:/saved/template")
  }
)



test_that(
  "cubicle_settings: NULL session/saved settings properly handled",
  {
    fake_config <- list(
      root     = NULL,
      template = NULL
    )

    withr::local_options(
      list(
        cubicle.root     = NULL,
        cubicle.template = NULL
      )
    )

    out <- with_mocked_bindings(
      cubicle_settings(),
      .read_config = function() fake_config
    )

    expect_null(out$session_root)
    expect_null(out$saved_root)
    expect_null(out$session_template)
    expect_null(out$saved_template)
  }
)



test_that(
  "cubicle_settings: returned invisibly",
  {
    fake_config <- list(
      root     = "C:/saved/root",
      template = "C:/saved/template"
    )

    withr::local_options(
      list(
        cubicle.root     = "C:/session/root",
        cubicle.template = "C:/session/template"
      )
    )

    expect_invisible(
      with_mocked_bindings(
        cubicle_settings(),
        .read_config = function() fake_config
      )
    )
  }
)
