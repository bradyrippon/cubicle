
test_that("project_info returns project invisibly", {
  proj <- list(
    name = "proj-a",
    path = "a",
    timestamp = "2026-01-01",
    group = "analysis"
  )

  out <- with_mocked_bindings(
    project_info("proj-a"),
    .get_registered_projects = function() list(proj),
    .find_project_index = function(project) 1,
    .list_tags = function(path) character(0)
  )

  expect_equal(out, proj)
})

test_that("project_info handles NULL group as None branch", {
  proj <- list(
    name = "proj-a",
    path = "a",
    timestamp = "2026-01-01",
    group = NULL
  )

  out <- with_mocked_bindings(
    project_info("proj-a"),
    .get_registered_projects = function() list(proj),
    .find_project_index = function(project) 1,
    .list_tags = function(path) character(0)
  )

  expect_null(out$group)
})

test_that("project_info handles tags when present", {
  proj <- list(
    name = "proj-a",
    path = "a",
    timestamp = "2026-01-01",
    group = "analysis"
  )

  out <- with_mocked_bindings(
    project_info("proj-a"),
    .get_registered_projects = function() list(proj),
    .find_project_index = function(project) 1,
    .list_tags = function(path) c("#tag1", "#tag2")
  )

  expect_equal(out, proj)
})

test_that("project_info handles no tags branch", {
  proj <- list(
    name = "proj-a",
    path = "a",
    timestamp = "2026-01-01",
    group = "analysis"
  )

  out <- with_mocked_bindings(
    project_info("proj-a"),
    .get_registered_projects = function() list(proj),
    .find_project_index = function(project) 1,
    .list_tags = function(path) character(0)
  )

  expect_equal(out, proj)
})

test_that("project_info returns invisibly", {
  proj <- list(
    name = "proj-a",
    path = "a",
    timestamp = "2026-01-01",
    group = "analysis"
  )

  expect_invisible(
    with_mocked_bindings(
      project_info("proj-a"),
      .get_registered_projects = function() list(proj),
      .find_project_index = function(project) 1,
      .list_tags = function(path) character(0)
    )
  )
})

test_that("project_info propagates project lookup errors", {
  with_mocked_bindings(
    {
      expect_error(
        project_info("proj-a"),
        "No project found with name"
      )
    },
    .get_registered_projects = function() list(),
    .find_project_index = function(project) {
      cli::cli_abort("No project found with name {.val {project}}.")
    }
  )
})
