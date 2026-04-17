
test_that(
  ".config_path: settings.rds path returned and directory created",
  {
    tmp <- withr::local_tempdir()
    with_mocked_bindings(
      {
        out <- .config_path()

        expect_true(fs::dir_exists(fs::path_dir(out)))
        expect_equal(fs::path_file(out), "settings.rds")
      },
      .user_config_dir = function() tmp
    )
  }
)



test_that(
  ".read_config: default configuration returned with no settings file",
  {
    tmp <- withr::local_tempdir()
    with_mocked_bindings(
      {
        out <- .read_config()

        expect_equal(
          out,
          list(
            root = NULL,
            template = NULL
          )
        )
      },
      .user_config_dir = function() tmp
    )
  }
)



test_that(
  ".write_config: write a readable configuration",
  {
    tmp <- withr::local_tempdir()
    config <- list(
      root = "C:/projects",
      template = "C:/templates/cubicle"
    )

    with_mocked_bindings(
      {
        .write_config(config)
        out <- .read_config()

        expect_equal(out, config)
      },
      .user_config_dir = function() tmp
    )
  }
)



test_that(
  ".write_config: settings.rds file created",
  {
    tmp <- withr::local_tempdir()
    config <- list(
      root = "C:/projects",
      template = NULL
    )

    with_mocked_bindings(
      {
        path <- .config_path()
        .write_config(config)

        expect_true(fs::file_exists(path))
      },
      .user_config_dir = function() tmp
    )
  }
)
