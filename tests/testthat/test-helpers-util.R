
test_that(
  "%||%: left returned when not NULL",
  {
    expect_equal("a" %||% "b", "a")
    expect_equal("" %||% "b", "")
    expect_equal(1 %||% 2, 1)
    expect_equal(FALSE %||% TRUE, FALSE)
  }
)



test_that(
  "%||%: right returned when left is NULL",
  {
    expect_equal(NULL %||% "b", "b")
    expect_equal(NULL %||% 2, 2)
  }
)



test_that(
  ".clean_string: whitespace/special characters removed",
  {
    expect_equal(.clean_string("Test Test"), "test_test")
    expect_equal(.clean_string(" Test, Test "), "test_test")
    expect_equal(.clean_string("Test---Test"), "test_test")
    expect_equal(.clean_string("___Test__Test___"), "test_test")
    expect_equal(.clean_string("Test 1!"), "test_1")
  }
)



test_that(
  ".decide_path: clean absolute path returned",
  {
    tmp <- withr::local_tempdir()
    abs_path <- fs::path(tmp, "my_project")

    out <- .decide_path(abs_path)

    expect_equal(out, fs::path_norm(abs_path))
  }
)



test_that(
  ".decide_path: path and root combined appropriately",
  {
    tmp <- withr::local_tempdir()

    expect_equal(
      .decide_path("projects/test", root = tmp),
      fs::path_norm(fs::path(tmp, "projects/test"))
    )
  }
)



test_that(
  ".decide_path: saved cubicle root used for relative paths",
  {
    tmp <- withr::local_tempdir()

    with_mocked_bindings(
      {
        expect_equal(
          .decide_path("projects/test"),
          fs::path_norm(fs::path(tmp, "projects/test"))
        )
      },
      get_root = function() tmp
    )
  }
)



test_that(
  ".decide_path: error; relative path with no root specified",
  {
    with_mocked_bindings(
      {
        expect_error(
          .decide_path("projects/test"),
          "Relative path supplied but no root directory was found"
        )
      },
      get_root = function() NULL
    )
  }
)



test_that(
  ".decide_path: explicit root used instead of saved root",
  {
    tmp_explicit <- withr::local_tempdir()
    tmp_saved    <- withr::local_tempdir()

    with_mocked_bindings(
      {
        expect_equal(
          .decide_path("projects/test", root = tmp_explicit),
          fs::path_norm(fs::path(tmp_explicit, "projects/test"))
        )
      },
      get_root = function() tmp_saved
    )
  }
)
