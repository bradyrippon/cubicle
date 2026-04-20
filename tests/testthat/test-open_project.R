
test_that("open_project errors when RStudio not available", {
  with_mocked_bindings(
    {
      expect_error(
        open_project("proj-a"),
        "You can only open a project from within RStudio"
      )
    },
    .find_project_path = function(project) "a",
    .find_rproj = function(path) fs::path("a", "proj-a.Rproj"),
    .is_rstudio_available = function() FALSE
  )
})

test_that("open_project errors when no .Rproj file found", {
  with_mocked_bindings(
    {
      expect_error(
        open_project("proj-a"),
        "No `.Rproj` file was found in the registered project directory"
      )
    },
    .find_project_path = function(project) "a",
    .find_rproj = function(path) "a",
    .is_rstudio_available = function() TRUE
  )
})

test_that("open_project opens valid .Rproj file and returns it invisibly", {
  tmp <- withr::local_tempdir()
  rproj <- fs::path(tmp, "proj-a.Rproj")
  fs::file_create(rproj)
  opened <- NULL

  out <- with_mocked_bindings(
    open_project("proj-a"),
    .find_project_path = function(project) tmp,
    .find_rproj = function(path) rproj,
    .is_rstudio_available = function() TRUE,
    .open_rstudio_project = function(path) {
      opened <<- path
      invisible(path)
    }
  )

  expect_equal(opened, rproj)
  expect_equal(out, rproj)
})

test_that("open_project returns invisibly", {
  tmp <- withr::local_tempdir()
  rproj <- fs::path(tmp, "proj-a.Rproj")
  fs::file_create(rproj)

  expect_invisible(
    with_mocked_bindings(
      open_project("proj-a"),
      .find_project_path = function(project) tmp,
      .find_rproj = function(path) rproj,
      .is_rstudio_available = function() TRUE,
      .open_rstudio_project = function(path) invisible(path)
    )
  )
})

test_that("open_project propagates lookup errors", {
  with_mocked_bindings(
    {
      expect_error(
        open_project("proj-a"),
        "No project found with name"
      )
    },
    .find_project_path = function(project) {
      cli::cli_abort("No project found with name {.val {project}}.")
    }
  )
})
