
test_that("%||% returns left value when not NULL", {
  expect_equal("a" %||% "b", "a")
  expect_equal("" %||% "b", "")
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(1 %||% 2, 1)
})

test_that("%||% returns right value when left is NULL", {
  expect_equal(NULL %||% "b", "b")
  expect_equal(NULL %||% 2, 2)
})

test_that(".clean_string cleans text as expected", {
  expect_equal(.clean_string("Test Test"), "test_test")
  expect_equal(.clean_string(" Test, Test "), "test_test")
  expect_equal(.clean_string("Test---Test"), "test_test")
  expect_equal(.clean_string("___Test__Test___"), "test_test")
  expect_equal(.clean_string("Test 1!"), "test_1")
  expect_equal(.clean_string("already_clean"), "already_clean")
})

test_that(".normalize_path returns normalized absolute path", {
  tmp <- withr::local_tempdir()
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  expect_equal(
    .normalize_path("abc"),
    fs::path_norm(fs::path_abs("abc"))
  )

  abs_path <- fs::path(tmp, "xyz")
  expect_equal(
    .normalize_path(abs_path),
    fs::path_norm(fs::path_abs(abs_path))
  )
})

test_that(".decide_path returns normalized absolute path unchanged", {
  tmp <- withr::local_tempdir()
  abs_path <- fs::path(tmp, "my_project")

  expect_equal(
    .decide_path(abs_path),
    fs::path_norm(abs_path)
  )
})

test_that(".decide_path combines relative path with explicit root", {
  tmp <- withr::local_tempdir()

  expect_equal(
    .decide_path("projects/test", root = tmp),
    fs::path_norm(fs::path(tmp, "projects/test"))
  )
})

test_that(".decide_path uses saved root when explicit root not supplied", {
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
})

test_that(".decide_path explicit root takes precedence over saved root", {
  tmp_explicit <- withr::local_tempdir()
  tmp_saved <- withr::local_tempdir()

  with_mocked_bindings(
    {
      expect_equal(
        .decide_path("projects/test", root = tmp_explicit),
        fs::path_norm(fs::path(tmp_explicit, "projects/test"))
      )
    },
    get_root = function() tmp_saved
  )
})

test_that(".decide_path errors for relative path when no root available", {
  with_mocked_bindings(
    {
      expect_error(
        .decide_path("projects/test"),
        "Relative path supplied but no root directory was found"
      )
    },
    get_root = function() NULL
  )
})

test_that(".validate_flag accepts TRUE and FALSE", {
  expect_invisible(.validate_flag(TRUE, "x"))
  expect_invisible(.validate_flag(FALSE, "x"))
})

test_that(".validate_flag errors on invalid inputs", {
  expect_error(.validate_flag(NA, "x"), "must be `TRUE` or `FALSE`")
  expect_error(.validate_flag(c(TRUE, FALSE), "x"), "must be `TRUE` or `FALSE`")
  expect_error(.validate_flag("TRUE", "x"), "must be `TRUE` or `FALSE`")
  expect_error(.validate_flag(1, "x"), "must be `TRUE` or `FALSE`")
})

test_that(".validate_string_required accepts valid input", {
  expect_invisible(.validate_string_required("abc", "x"))
})

test_that(".validate_string_required errors on invalid inputs", {
  expect_error(.validate_string_required(NULL, "x"), "must be a non-empty character string")
  expect_error(.validate_string_required(NA_character_, "x"), "must be a non-empty character string")
  expect_error(.validate_string_required("", "x"), "must be a non-empty character string")
  expect_error(.validate_string_required(1, "x"), "must be a non-empty character string")
  expect_error(.validate_string_required(c("a", "b"), "x"), "must be a non-empty character string")
})

test_that(".validate_string_optional accepts NULL and valid string", {
  expect_invisible(.validate_string_optional(NULL, "x"))
  expect_invisible(.validate_string_optional("abc", "x"))
})

test_that(".validate_string_optional errors on invalid inputs", {
  expect_error(.validate_string_optional("", "x"), "must be `NULL` or a non-empty character string")
  expect_error(.validate_string_optional(NA_character_, "x"), "must be `NULL` or a non-empty character string")
  expect_error(.validate_string_optional(1, "x"), "must be `NULL` or a non-empty character string")
  expect_error(.validate_string_optional(c("a", "b"), "x"), "must be `NULL` or a non-empty character string")
})

test_that(".get_registered_projects returns projects element", {
  fake_projects <- list(
    list(name = "proj-a", path = "a"),
    list(name = "proj-b", path = "b")
  )

  out <- with_mocked_bindings(
    .get_registered_projects(),
    .read_config = function() {
      list(
        root = NULL,
        template = NULL,
        projects = fake_projects
      )
    }
  )

  expect_equal(out, fake_projects)
})

test_that(".get_registered_projects returns empty list when no projects", {
  out <- with_mocked_bindings(
    .get_registered_projects(),
    .read_config = function() {
      list(
        root = NULL,
        template = NULL,
        projects = list()
      )
    }
  )

  expect_equal(out, list())
})

test_that(".get_active_rstudio_project returns NULL when RStudio unavailable", {
  out <- with_mocked_bindings(
    .get_active_rstudio_project(),
    .is_rstudio_available = function() FALSE
  )

  expect_null(out)
})

test_that(".open_rstudio_project errors when RStudio unavailable", {
  with_mocked_bindings(
    {
      expect_error(
        .open_rstudio_project("abc.Rproj"),
        "You can only open a project from within RStudio"
      )
    },
    .is_rstudio_available = function() FALSE
  )
})

