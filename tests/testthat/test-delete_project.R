
test_that("delete_project removes project by unique name", {
  projects <- list(
    list(name = "proj-a", path = "a"),
    list(name = "proj-b", path = "b")
  )
  saved <- NULL

  out <- with_mocked_bindings(
    delete_project("proj-a"),
    .get_registered_projects = function() projects,
    .find_project_index = function(project) 1,
    .save_registered_projects = function(x) {
      saved <<- x
      invisible(x)
    }
  )

  expect_equal(out, projects[[1]])
  expect_length(saved, 1)
  expect_equal(saved[[1]]$name, "proj-b")
})

test_that("delete_project removes project by registered path", {
  projects <- list(
    list(name = "proj-a", path = "a"),
    list(name = "proj-b", path = "b")
  )
  saved <- NULL

  out <- with_mocked_bindings(
    delete_project("b"),
    .get_registered_projects = function() projects,
    .find_project_index = function(project) 2,
    .save_registered_projects = function(x) {
      saved <<- x
      invisible(x)
    }
  )

  expect_equal(out, projects[[2]])
  expect_length(saved, 1)
  expect_equal(saved[[1]]$name, "proj-a")
})

test_that("delete_project returns invisibly", {
  projects <- list(list(name = "proj-a", path = "a"))

  expect_invisible(
    with_mocked_bindings(
      delete_project("proj-a"),
      .get_registered_projects = function() projects,
      .find_project_index = function(project) 1,
      .save_registered_projects = function(x) invisible(x)
    )
  )
})

test_that("delete_project propagates no-projects error", {
  with_mocked_bindings(
    {
      expect_error(
        delete_project("proj-a"),
        "No registered projects were found"
      )
    },
    .get_registered_projects = function() list(),
    .find_project_index = function(project) {
      cli::cli_abort("No registered projects were found.")
    }
  )
})

test_that("delete_project propagates missing-project error", {
  with_mocked_bindings(
    {
      expect_error(
        delete_project("proj-a"),
        "No project found with name"
      )
    },
    .get_registered_projects = function() list(list(name = "proj-b", path = "b")),
    .find_project_index = function(project) {
      cli::cli_abort("No project found with name {.val {project}}.")
    }
  )
})

test_that("delete_project propagates duplicate-name error", {
  with_mocked_bindings(
    {
      expect_error(
        delete_project("proj"),
        "Multiple registered projects are named"
      )
    },
    .get_registered_projects = function() list(
      list(name = "proj", path = "a"),
      list(name = "proj", path = "b")
    ),
    .find_project_index = function(project) {
      cli::cli_abort("Multiple registered projects are named {.val {project}}.")
    }
  )
})
