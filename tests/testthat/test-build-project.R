
test_that(
  "build_project: project structure created",
  {
    tmp <- withr::local_tempdir()
    out <- build_project(
      name = "Test Project",
      path = tmp
    )

    expect_true(fs::dir_exists(out))
    expect_equal(fs::path_file(out), "Test Project")

    expect_true(fs::dir_exists(fs::path(out, "code")))
    expect_true(fs::dir_exists(fs::path(out, "code", "admin")))
    expect_true(fs::dir_exists(fs::path(out, "data")))
    expect_true(fs::dir_exists(fs::path(out, "data", "raw")))
    expect_true(fs::dir_exists(fs::path(out, "documents")))
    expect_true(fs::dir_exists(fs::path(out, "documents", "prints")))
    expect_true(fs::dir_exists(fs::path(out, "documents", "prints", "published")))
    expect_true(fs::dir_exists(fs::path(out, "documents", "records")))
    expect_true(fs::dir_exists(fs::path(out, "documents", "records", "published")))
    expect_true(fs::dir_exists(fs::path(out, "documents", "zips")))
    expect_true(fs::dir_exists(fs::path(out, "figures")))
    expect_true(fs::dir_exists(fs::path(out, "reports")))
    expect_true(fs::dir_exists(fs::path(out, "tags")))

    expect_true(fs::file_exists(fs::path(out, "notes.docx")))
    expect_true(fs::file_exists(fs::path(out, "proj.Rproj")))
  }
)



test_that(
  "build_project: exact folder name supplied by user used",
  {
    tmp <- withr::local_tempdir()
    out <- build_project(
      name = "Test Project",
      path = tmp
    )

    expect_true(fs::dir_exists(out))
    expect_equal(fs::path_file(out), "Test Project")
  }
)



test_that(
  "build_project: Rproj file renamed when [use_name] is TRUE",
  {
    tmp <- withr::local_tempdir()
    out <- build_project(
      name = "Test Project",
      path = tmp,
      use_name = TRUE
    )

    expect_true(fs::file_exists(fs::path(out, "test_project.Rproj")))
    expect_false(fs::file_exists(fs::path(out, "proj.Rproj")))
  }
)



test_that(
  "build_project: suffix appended to notes/project files",
  {
    tmp <- withr::local_tempdir()
    out <- build_project(
      name = "Test Project",
      path = tmp,
      use_name = TRUE,
      append = "Draft 1!"
    )

    expect_true(fs::file_exists(fs::path(out, "notes-draft_1.docx")))
    expect_true(fs::file_exists(fs::path(out, "test_project-draft_1.Rproj")))

    expect_false(fs::file_exists(fs::path(out, "notes.docx")))
    expect_false(fs::file_exists(fs::path(out, "proj.Rproj")))
  }
)



test_that(
  "build_project: suffix appended but keeps proj base name when [use_name] is FALSE",
  {
    tmp <- withr::local_tempdir()
    out <- build_project(
      name = "Test Project",
      path = tmp,
      use_name = FALSE,
      append = "Draft 1!"
    )

    expect_true(fs::file_exists(fs::path(out, "notes-draft_1.docx")))
    expect_true(fs::file_exists(fs::path(out, "proj-draft_1.Rproj")))
  }
)



test_that(
  "build_project: placeholder files removed from created project",
  {
    tmp <- withr::local_tempdir()
    out <- build_project(
      name = "Test Project",
      path = tmp
    )

    placeholders <- fs::dir_ls(
      path = out,
      all = TRUE,
      recurse = TRUE,
      regexp = "\\.cubicle-temp$"
    )

    expect_length(placeholders, 0)
  }
)



test_that(
  "build_project: error; target project already exists",
  {
    tmp <- withr::local_tempdir()
    fs::dir_create(fs::path(tmp, "Test Project"))

    expect_error(
      build_project(
        name = "Test Project",
        path = tmp
      ),
      "Target project already exists"
    )
  }
)



test_that(
  "build_project: error; [name] is missing or empty",
  {
    tmp <- withr::local_tempdir()

    expect_error(
      build_project(path = tmp),
      "`name` must be provided."
    )

    expect_error(
      build_project(
        name = "",
        path = tmp
      ),
      "`name` must be provided."
    )
  }
)



test_that(
  "build_project: error; [path] is missing or empty",
  {
    expect_error(
      build_project(name = "Test Project"),
      "`path` must be provided."
    )

    expect_error(
      build_project(
        name = "Test Project",
        path = ""
      ),
      "`path` must be provided."
    )
  }
)
