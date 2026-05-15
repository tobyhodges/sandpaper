# Tests for the transient Quarto project (`_quarto.yml`) management
# helpers used during .qmd builds.

# ---- build_quarto_project_yaml() -------------------------------------------

test_that("build_quarto_project_yaml() generates a minimal default project", {
  yml <- sandpaper:::build_quarto_project_yaml()
  expect_match(yml, "project:\\s*\\n\\s*type: default")
  expect_match(yml, "sandpaper-managed")
  expect_false(grepl("filters:", yml))
  expect_false(grepl("engine:", yml))
})

test_that("build_quarto_project_yaml() sets engine when requested", {
  yml <- sandpaper:::build_quarto_project_yaml(engine = "knitr")
  expect_match(yml, "engine: knitr")
})

test_that("build_quarto_project_yaml() emits execute.error when requested", {
  yml_permissive <- sandpaper:::build_quarto_project_yaml(execute_error = TRUE)
  expect_match(yml_permissive, "error: true")

  yml_strict <- sandpaper:::build_quarto_project_yaml(execute_error = FALSE)
  expect_match(yml_strict, "error: false")

  yml_unset <- sandpaper:::build_quarto_project_yaml()
  expect_false(grepl("error:", yml_unset))
})

test_that("build_quarto_project_yaml() always emits execute.daemon: false", {
  # Disabling the kernel daemon prevents Quarto from wedging on a dead
  # kernel socket between renders. See build_quarto_project_yaml() docs.
  yml_default <- sandpaper:::build_quarto_project_yaml()
  expect_match(yml_default, "execute:\\s*\\n\\s*daemon: false")

  yml_with_error <- sandpaper:::build_quarto_project_yaml(execute_error = TRUE)
  expect_match(yml_with_error, "daemon: false")
  expect_match(yml_with_error, "error: true")
})

test_that("build_quarto_project_yaml() starts with the sandpaper sentinel", {
  yml <- sandpaper:::build_quarto_project_yaml()
  first_line <- strsplit(yml, "\n", fixed = TRUE)[[1]][1]
  expect_identical(first_line, sandpaper:::sandpaper_sentinel)
})

# ---- is_sandpaper_quarto_yml() ---------------------------------------------

test_that("is_sandpaper_quarto_yml() detects the sentinel header", {
  tmp <- withr::local_tempdir()
  yml <- fs::path(tmp, "_quarto.yml")
  writeLines(
    c(sandpaper:::sandpaper_sentinel, "project:", "  type: website"),
    yml
  )
  expect_true(sandpaper:::is_sandpaper_quarto_yml(yml))
})

test_that("is_sandpaper_quarto_yml() returns FALSE for user-authored files", {
  tmp <- withr::local_tempdir()
  yml <- fs::path(tmp, "_quarto.yml")
  writeLines(c("project:", "  type: website"), yml)
  expect_false(sandpaper:::is_sandpaper_quarto_yml(yml))
})

test_that("is_sandpaper_quarto_yml() returns FALSE when file is missing", {
  tmp <- withr::local_tempdir()
  expect_false(sandpaper:::is_sandpaper_quarto_yml(fs::path(tmp, "_quarto.yml")))
})

# ---- with_quarto_project() -------------------------------------------------

test_that("with_quarto_project() writes file when none exists and cleans up", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  if (fs::file_exists(yml)) fs::file_delete(yml)

  state <- new.env()
  state$observed_during <- FALSE
  sandpaper:::with_quarto_project(tmp, {
    state$observed_during <- fs::file_exists(yml) &&
      sandpaper:::is_sandpaper_quarto_yml(yml)
  }, quiet = TRUE)

  expect_true(state$observed_during)
  expect_false(fs::file_exists(yml))
})

test_that("with_quarto_project() returns the value of expr", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  if (fs::file_exists(yml)) fs::file_delete(yml)

  result <- sandpaper:::with_quarto_project(tmp, 42L, quiet = TRUE)
  expect_identical(result, 42L)
})

test_that("with_quarto_project() cleans up on error", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  if (fs::file_exists(yml)) fs::file_delete(yml)

  expect_error(
    sandpaper:::with_quarto_project(tmp, stop("boom"), quiet = TRUE),
    "boom"
  )
  expect_false(fs::file_exists(yml))
})

test_that("with_quarto_project() respects and restores an existing user file", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  user_contents <- c("project:", "  type: default", "bibliography: refs.bib")
  writeLines(user_contents, yml)

  sandpaper:::with_quarto_project(tmp, {
    # During the build, the file on disk is the sandpaper-managed merge
    expect_true(sandpaper:::is_sandpaper_quarto_yml(yml))
  }, quiet = TRUE)

  # After the build, the user's original file is restored verbatim
  expect_true(fs::file_exists(yml))
  expect_identical(readLines(yml), user_contents)
})

test_that("with_quarto_project() merged file preserves user keys and adds engine", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  writeLines(c("project:", "  type: default", "bibliography: refs.bib"), yml)

  state <- new.env()
  state$merged_during <- character()
  sandpaper:::with_quarto_project(tmp, {
    state$merged_during <- readLines(yml)
  }, engine = "knitr", quiet = TRUE)

  expect_true(any(grepl("bibliography: refs.bib", state$merged_during)))
  expect_true(any(grepl("engine: knitr", state$merged_during)))
})

test_that("merge_quarto_yaml() adds execute.error when absent from user file", {
  user_lines <- c("project:", "  type: default", "bibliography: refs.bib")
  merged <- sandpaper:::merge_quarto_yaml(user_lines,
    execute_error = FALSE, quiet = TRUE)
  expect_true(any(grepl("bibliography: refs.bib", merged, fixed = TRUE)))
  joined <- paste(merged, collapse = "\n")
  expect_match(joined, "error: false")
})

test_that("merge_quarto_yaml() preserves a user-set execute.error", {
  # The user has explicitly set execute.error: true. Sandpaper must
  # respect that even if its fail_on_error config would imply false.
  user_lines <- c(
    "project:", "  type: default",
    "execute:", "  error: true",
    "bibliography: refs.bib"
  )
  merged <- sandpaper:::merge_quarto_yaml(user_lines,
    execute_error = FALSE, quiet = TRUE)
  joined <- paste(merged, collapse = "\n")
  expect_match(joined, "error: true")
  expect_false(grepl("error: false", joined, fixed = TRUE))
})

test_that("merge_quarto_yaml() adds execute.daemon: false when absent", {
  user_lines <- c("project:", "  type: default", "bibliography: refs.bib")
  merged <- sandpaper:::merge_quarto_yaml(user_lines, quiet = TRUE)
  joined <- paste(merged, collapse = "\n")
  expect_match(joined, "daemon: false")
})

test_that("merge_quarto_yaml() preserves a user-set execute.daemon", {
  # Author has explicitly opted into the persistent kernel daemon.
  # Sandpaper must respect that even though its default is to disable it.
  user_lines <- c(
    "project:", "  type: default",
    "execute:", "  daemon: true",
    "bibliography: refs.bib"
  )
  merged <- sandpaper:::merge_quarto_yaml(user_lines, quiet = TRUE)
  joined <- paste(merged, collapse = "\n")
  expect_match(joined, "daemon: true")
  expect_false(grepl("daemon: false", joined, fixed = TRUE))
})

test_that("merge_quarto_yaml() adds daemon alongside a user-set execute.error", {
  # User set execute.error but not execute.daemon. Sandpaper should
  # leave the error value alone and still add daemon: false.
  user_lines <- c(
    "project:", "  type: default",
    "execute:", "  error: true"
  )
  merged <- sandpaper:::merge_quarto_yaml(user_lines, quiet = TRUE)
  joined <- paste(merged, collapse = "\n")
  expect_match(joined, "error: true")
  expect_match(joined, "daemon: false")
})

test_that("with_quarto_project() logs info when merging existing user file", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  writeLines(c("project:", "  type: default"), yml)

  expect_message(
    sandpaper:::with_quarto_project(tmp, NULL, quiet = FALSE),
    "Merging"
  )
})

test_that("with_quarto_project() aborts on output-redirecting project types", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")

  for (type in c("website", "book", "manuscript")) {
    user_contents <- c("project:", paste0("  type: ", type),
      "bibliography: refs.bib")
    writeLines(user_contents, yml)

    expect_error(
      sandpaper:::with_quarto_project(tmp, NULL, quiet = TRUE),
      paste0("project\\.type: ", type)
    )
    # User file is untouched after the abort.
    expect_identical(readLines(yml), user_contents)
  }
})

test_that("with_quarto_project() treats orphaned sandpaper file as absent", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  # Simulate a leftover file from a crashed build
  writeLines(
    c(sandpaper:::sandpaper_sentinel, "project:", "  type: website"),
    yml
  )

  sandpaper:::with_quarto_project(tmp, NULL, quiet = TRUE)

  # Orphan file should be removed on exit
  expect_false(fs::file_exists(yml))
})

# ---- build_markdown() integration ------------------------------------------

test_that("build_markdown() creates and removes transient _quarto.yml for .qmd builds", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  if (fs::file_exists(yml)) fs::file_delete(yml)

  # Add a .qmd episode with no executable content so jupyter/knitr are not
  # required for the build.
  qmd <- fs::path(tmp, "episodes", "qproject.qmd")
  writeLines(c(
    "---", "title: 'Project Test'", "teaching: 1", "exercises: 1",
    "engine: markdown", "---",
    "",
    "::::::::::::::::::::::::::::::::::::: questions", "- Q?",
    "::::::::::::::::::::::::::::::::::::::::::::::::",
    "",
    "::::::::::::::::::::::::::::::::::::: objectives", "- O",
    "::::::::::::::::::::::::::::::::::::::::::::::::",
    "",
    "Hello from a .qmd file.",
    "",
    "::::::::::::::::::::::::::::::::::::: keypoints", "- K",
    "::::::::::::::::::::::::::::::::::::::::::::::::"
  ), qmd)
  set_episodes(tmp, order = c("introduction.Rmd", "qproject.qmd"), write = TRUE)

  expect_no_error(build_markdown(tmp, quiet = TRUE))

  # Transient file should have been removed on completion
  expect_false(fs::file_exists(yml))
})

test_that("build_markdown() does not create a transient file for .md-only lessons", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  if (fs::file_exists(yml)) fs::file_delete(yml)

  # Replace the Rmd introduction with an md version so no Rmd/qmd is present
  rmd <- fs::path(tmp, "episodes", "introduction.Rmd")
  md  <- fs::path(tmp, "episodes", "introduction.md")
  if (fs::file_exists(rmd)) {
    txt <- readLines(rmd)
    writeLines(txt, md)
    fs::file_delete(rmd)
  }
  sandpaper:::.resources$clear()
  set_episodes(tmp, order = "introduction.md", write = TRUE)

  expect_no_error(build_markdown(tmp, quiet = TRUE))
  expect_false(fs::file_exists(yml))
})

test_that("build_markdown() restores an existing user _quarto.yml after .qmd build", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  user_contents <- c("project:", "  type: default", "toc: true")
  writeLines(user_contents, yml)

  qmd <- fs::path(tmp, "episodes", "qproject.qmd")
  writeLines(c(
    "---", "title: 'Project Test'", "teaching: 1", "exercises: 1",
    "engine: markdown", "---",
    "",
    "::::::::::::::::::::::::::::::::::::: questions", "- Q?",
    "::::::::::::::::::::::::::::::::::::::::::::::::",
    "",
    "::::::::::::::::::::::::::::::::::::: objectives", "- O",
    "::::::::::::::::::::::::::::::::::::::::::::::::",
    "",
    "Hello.",
    "",
    "::::::::::::::::::::::::::::::::::::: keypoints", "- K",
    "::::::::::::::::::::::::::::::::::::::::::::::::"
  ), qmd)
  set_episodes(tmp, order = c("introduction.Rmd", "qproject.qmd"), write = TRUE)

  expect_no_error(build_markdown(tmp, quiet = TRUE))

  # User's file is restored verbatim
  expect_true(fs::file_exists(yml))
  expect_identical(readLines(yml), user_contents)
})
