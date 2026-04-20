
test_that(".filter_projects returns unchanged input when no filters supplied", {
  projects <- list(
    list(name = "a", path = "x/a", timestamp = "2026-01-01", group = "g1"),
    list(name = "b", path = "x/b", timestamp = "2026-01-02", group = NULL)
  )

  expect_equal(.filter_projects(projects), projects)
})

test_that(".filter_projects filters by group", {
  projects <- list(
    list(name = "a", path = "x/a", timestamp = "2026-01-01", group = "g1"),
    list(name = "b", path = "x/b", timestamp = "2026-01-02", group = "g2"),
    list(name = "c", path = "x/c", timestamp = "2026-01-03", group = NULL)
  )

  out <- .filter_projects(projects, group = "g1")

  expect_length(out, 1)
  expect_equal(out[[1]]$name, "a")
})

test_that(".filter_projects filters by timestamp", {
  projects <- list(
    list(name = "a", path = "x/a", timestamp = "2026-01-01", group = "g1"),
    list(name = "b", path = "x/b", timestamp = "2026-01-02", group = "g2")
  )

  out <- .filter_projects(projects, timestamp = "2026-01-02")

  expect_length(out, 1)
  expect_equal(out[[1]]$name, "b")
})

test_that(".filter_projects filters by group and timestamp", {
  projects <- list(
    list(name = "a", path = "x/a", timestamp = "2026-01-01", group = "g1"),
    list(name = "b", path = "x/b", timestamp = "2026-01-01", group = "g2"),
    list(name = "c", path = "x/c", timestamp = "2026-01-01", group = "g1")
  )

  out <- .filter_projects(projects, group = "g1", timestamp = "2026-01-01")

  expect_length(out, 2)
  expect_equal(vapply(out, `[[`, character(1), "name"), c("a", "c"))
})

test_that(".filter_projects returns empty list when no matches", {
  projects <- list(
    list(name = "a", path = "x/a", timestamp = "2026-01-01", group = "g1")
  )

  out <- .filter_projects(projects, group = "g2")

  expect_equal(out, list())
})

test_that(".find_path_levels returns expected tail of path", {
  tmp <- fs::path("projects", "analysis", "proj-a")

  expect_equal(
    .find_path_levels(tmp, levels = 1),
    "proj-a"
  )

  expect_equal(
    .find_path_levels(tmp, levels = 2),
    "analysis/proj-a"
  )

  expect_equal(
    .find_path_levels(tmp, levels = 5),
    paste(tail(.path_components(tmp), 5), collapse = "/")
  )
})

test_that(".find_path_levels validates levels", {
  expect_error(.find_path_levels("a", levels = "1"), "must be a positive integer")
  expect_error(.find_path_levels("a", levels = NA), "must be a positive integer")
  expect_error(.find_path_levels("a", levels = 0), "must be a positive integer")
  expect_error(.find_path_levels("a", levels = 1.5), "must be a positive integer")
})

test_that(".find_parts splits label into prefix and name", {
  out1 <- .find_parts(fs::path("proj-a"), levels = 1)
  expect_equal(out1$prefix, "")
  expect_equal(out1$name, "proj-a")

  out2 <- .find_parts(fs::path("analysis", "proj-a"), levels = 2)
  expect_equal(out2$prefix, "analysis/")
  expect_equal(out2$name, "proj-a")
})

test_that(".list_tags returns character(0) when tags directory missing", {
  tmp <- withr::local_tempdir()

  expect_equal(.list_tags(tmp), character(0))
})

test_that(".list_tags returns character(0) when no txt files exist", {
  tmp <- withr::local_tempdir()
  tag_dir <- fs::path(tmp, "tags")
  fs::dir_create(tag_dir)
  fs::file_create(fs::path(tag_dir, "notes.csv"))

  expect_equal(.list_tags(tmp), character(0))
})

test_that(".list_tags returns tag names without extension", {
  tmp <- withr::local_tempdir()
  tag_dir <- fs::path(tmp, "tags")
  fs::dir_create(tag_dir)
  writeLines("x", fs::path(tag_dir, "#tag1.txt"))
  writeLines("x", fs::path(tag_dir, "#tag2.txt"))
  fs::file_create(fs::path(tag_dir, "other.csv"))

  out <- .list_tags(tmp)

  expect_length(out, 2)
  expect_true("#tag1" %in% out)
  expect_true("#tag2" %in% out)
})

test_that(".order_projects returns unchanged input when length <= 1", {
  projects <- list(list(name = "a", path = "x/a", timestamp = "2026-01-01", group = "g1"))

  expect_equal(.order_projects(projects), projects)
})

test_that(".order_projects orders by timestamp", {
  projects <- list(
    list(name = "b", path = fs::path("root", "b"), timestamp = "2026-01-02", group = "g2"),
    list(name = "a", path = fs::path("root", "a"), timestamp = "2026-01-01", group = "g1")
  )

  out <- .order_projects(projects, arrange = "timestamp")

  expect_equal(vapply(out, `[[`, character(1), "name"), c("a", "b"))
})

test_that(".order_projects orders by group", {
  projects <- list(
    list(name = "b", path = fs::path("root", "b"), timestamp = "2026-01-02", group = "g2"),
    list(name = "a", path = fs::path("root", "a"), timestamp = "2026-01-01", group = "g1")
  )

  out <- .order_projects(projects, arrange = "group")

  expect_equal(vapply(out, `[[`, character(1), "name"), c("a", "b"))
})

test_that(".order_projects orders by label", {
  projects <- list(
    list(name = "b", path = fs::path("root", "z", "b"), timestamp = "2026-01-02", group = "g2"),
    list(name = "a", path = fs::path("root", "a", "a"), timestamp = "2026-01-01", group = "g1")
  )

  out <- .order_projects(projects, arrange = "label", levels = 2)

  expect_equal(vapply(out, `[[`, character(1), "name"), c("a", "b"))
})

test_that(".order_projects orders by multiple fields", {
  projects <- list(
    list(name = "c", path = fs::path("root", "a", "c"), timestamp = "2026-01-01", group = "g2"),
    list(name = "a", path = fs::path("root", "a", "a"), timestamp = "2026-01-01", group = "g1"),
    list(name = "b", path = fs::path("root", "b", "b"), timestamp = "2026-01-02", group = "g1")
  )

  out <- .order_projects(projects, arrange = c("timestamp", "group", "label"), levels = 2)

  expect_equal(vapply(out, `[[`, character(1), "name"), c("a", "c", "b"))
})

test_that(".order_projects errors on invalid arrange values", {
  projects <- list(
    list(name = "a", path = "x/a", timestamp = "2026-01-01", group = "g1"),
    list(name = "b", path = "x/b", timestamp = "2026-01-02", group = "g2")
  )

  expect_error(
    .order_projects(projects, arrange = "bad"),
    "must be one of"
  )
})

test_that("show_projects returns invisibly with empty registry", {
  expect_invisible(
    with_mocked_bindings(
      show_projects(),
      .get_registered_projects = function() list()
    )
  )
})

test_that("show_projects returns invisibly with projects present", {
  projects <- list(
    list(name = "a", path = fs::path("root", "a"), timestamp = "2026-01-01", group = "g1")
  )

  out <- with_mocked_bindings(
    show_projects(),
    .get_registered_projects = function() projects
  )

  expect_equal(out, projects)
})

test_that("show_projects filters by group and timestamp", {
  projects <- list(
    list(name = "a", path = fs::path("root", "a"), timestamp = "2026-01-01", group = "g1"),
    list(name = "b", path = fs::path("root", "b"), timestamp = "2026-01-02", group = "g2")
  )

  out <- with_mocked_bindings(
    show_projects(group = "g1", timestamp = "2026-01-01"),
    .get_registered_projects = function() projects
  )

  expect_length(out, 1)
  expect_equal(out[[1]]$name, "a")
})

test_that("show_projects works with tags = TRUE", {
  projects <- list(
    list(name = "a", path = withr::local_tempdir(), timestamp = "2026-01-01", group = "g1")
  )

  tag_dir <- fs::path(projects[[1]]$path, "tags")
  fs::dir_create(tag_dir)
  writeLines("x", fs::path(tag_dir, "#tag1.txt"))

  out <- with_mocked_bindings(
    show_projects(tags = TRUE),
    .get_registered_projects = function() projects
  )

  expect_equal(out, projects)
})

test_that("show_projects works with levels > 1 and arrange = label", {
  projects <- list(
    list(name = "b", path = fs::path("root", "z", "b"), timestamp = "2026-01-02", group = "g2"),
    list(name = "a", path = fs::path("root", "a", "a"), timestamp = "2026-01-01", group = "g1")
  )

  out <- with_mocked_bindings(
    show_projects(levels = 2, arrange = "label"),
    .get_registered_projects = function() projects
  )

  expect_equal(vapply(out, `[[`, character(1), "name"), c("a", "b"))
})
