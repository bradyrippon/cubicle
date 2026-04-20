
test_that("edit_project updates group", {
  projects <- list(
    list(name = "proj-a", path = "a", timestamp = "2026-01-01", group = NULL),
    list(name = "proj-b", path = "b", timestamp = "2026-01-02", group = "old")
  )
  saved <- NULL

  out <- with_mocked_bindings(
    edit_project("proj-a", group = "analysis"),
    .get_registered_projects = function() projects,
    .find_project_index = function(project) 1,
    .save_registered_projects = function(x) {
      saved <<- x
      invisible(x)
    }
  )

  expect_equal(out$group, "analysis")
  expect_equal(saved[[1]]$group, "analysis")
  expect_equal(saved[[2]]$group, "old")
})

test_that("edit_project updates timestamp", {
  projects <- list(
    list(name = "proj-a", path = "a", timestamp = "2026-01-01", group = NULL)
  )
  saved <- NULL

  out <- with_mocked_bindings(
    edit_project("proj-a", timestamp = as.Date("2026-02-01")),
    .get_registered_projects = function() projects,
    .find_project_index = function(project) 1,
    .save_registered_projects = function(x) {
      saved <<- x
      invisible(x)
    }
  )

  expect_equal(out$timestamp, "2026-02-01")
  expect_equal(saved[[1]]$timestamp, "2026-02-01")
})

test_that("edit_project updates both group and timestamp", {
  projects <- list(
    list(name = "proj-a", path = "a", timestamp = "2026-01-01", group = NULL)
  )

  out <- with_mocked_bindings(
    edit_project("proj-a", group = "analysis", timestamp = as.Date("2026-02-01")),
    .get_registered_projects = function() projects,
    .find_project_index = function(project) 1,
    .save_registered_projects = function(x) invisible(x)
  )

  expect_equal(out$group, "analysis")
  expect_equal(out$timestamp, "2026-02-01")
})

test_that("edit_project leaves entry unchanged when both group and timestamp are NULL", {
  projects <- list(
    list(name = "proj-a", path = "a", timestamp = "2026-01-01", group = "old")
  )
  saved <- NULL

  out <- with_mocked_bindings(
    edit_project("proj-a"),
    .get_registered_projects = function() projects,
    .find_project_index = function(project) 1,
    .save_registered_projects = function(x) {
      saved <<- x
      invisible(x)
    }
  )

  expect_equal(out, projects[[1]])
  expect_equal(saved[[1]], projects[[1]])
})

test_that("edit_project returns invisibly", {
  projects <- list(
    list(name = "proj-a", path = "a", timestamp = "2026-01-01", group = NULL)
  )

  expect_invisible(
    with_mocked_bindings(
      edit_project("proj-a", group = "analysis"),
      .get_registered_projects = function() projects,
      .find_project_index = function(project) 1,
      .save_registered_projects = function(x) invisible(x)
    )
  )
})

test_that("edit_project propagates errors from project lookup", {
  with_mocked_bindings(
    {
      expect_error(
        edit_project("proj-a", group = "analysis"),
        "No project found with name"
      )
    },
    .get_registered_projects = function() list(),
    .find_project_index = function(project) {
      cli::cli_abort("No project found with name {.val {project}}.")
    }
  )
})
