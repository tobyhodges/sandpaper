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

test_that("build_quarto_project_yaml() adds shinylive filter when requested", {
  yml <- sandpaper:::build_quarto_project_yaml(shinylive = TRUE)
  expect_match(yml, "filters:\\s*\\n\\s*- shinylive")
})

test_that("build_quarto_project_yaml() sets engine when requested", {
  yml <- sandpaper:::build_quarto_project_yaml(engine = "knitr")
  expect_match(yml, "engine: knitr")
})

test_that("build_quarto_project_yaml() starts with the sandpaper sentinel", {
  yml <- sandpaper:::build_quarto_project_yaml()
  first_line <- strsplit(yml, "\n", fixed = TRUE)[[1]][1]
  expect_identical(first_line, sandpaper:::sandpaper_sentinel)
})

# ---- has_shinylive() -------------------------------------------------------

test_that("has_shinylive() returns FALSE when no extension present", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  expect_false(sandpaper:::has_shinylive(tmp))
})

test_that("has_shinylive() returns FALSE when _extensions/ has other extensions", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  fs::dir_create(fs::path(tmp, "_extensions", "some-other", "ext"))
  expect_false(sandpaper:::has_shinylive(tmp))
})

test_that("has_shinylive() returns TRUE when shinylive extension present", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  fs::dir_create(fs::path(tmp, "_extensions", "quarto-ext", "shinylive"))
  expect_true(sandpaper:::has_shinylive(tmp))
})

# ---- detect_qmd_executable_engines() ---------------------------------------

test_that("detect_qmd_executable_engines() returns empty when no .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  # Remove any .qmd that might be created by other tests
  qmds <- fs::dir_ls(tmp, glob = "*.qmd", recurse = TRUE, fail = FALSE)
  if (length(qmds)) fs::file_delete(qmds)
  expect_length(sandpaper:::detect_qmd_executable_engines(tmp), 0)
})

test_that("detect_qmd_executable_engines() finds {python} and {r} cells", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd <- fs::path(tmp, "episodes", "mixed.qmd")
  writeLines(c(
    "---", "title: Mixed", "---",
    "",
    "```{python}",
    "print('hi')",
    "```",
    "",
    "```{r}",
    "x <- 1",
    "```"
  ), qmd)
  engines <- sandpaper:::detect_qmd_executable_engines(tmp)
  expect_setequal(engines, c("python", "r"))
})

test_that("detect_qmd_executable_engines() ignores shinylive cells", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd <- fs::path(tmp, "episodes", "shinyonly.qmd")
  writeLines(c(
    "---", "title: Shinyonly", "---",
    "",
    "```{shinylive-python}",
    "from shiny import App",
    "```",
    "",
    "```{shinylive-r}",
    "library(shiny)",
    "```"
  ), qmd)
  engines <- sandpaper:::detect_qmd_executable_engines(tmp)
  expect_false("python" %in% engines)
  expect_false("r" %in% engines)
  expect_false("shinylive-python" %in% engines)
})

test_that("detect_qmd_executable_engines() scans across multiple .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(c("---", "title: A", "---", "", "```{python}", "x", "```"),
    fs::path(tmp, "episodes", "a.qmd"))
  writeLines(c("---", "title: B", "---", "", "```{julia}", "y", "```"),
    fs::path(tmp, "episodes", "b.qmd"))
  engines <- sandpaper:::detect_qmd_executable_engines(tmp)
  expect_setequal(engines, c("python", "julia"))
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
  user_contents <- c("project:", "  type: website", "bibliography: refs.bib")
  writeLines(user_contents, yml)

  sandpaper:::with_quarto_project(tmp, {
    # During the build, the file on disk is the sandpaper-managed merge
    expect_true(sandpaper:::is_sandpaper_quarto_yml(yml))
  }, quiet = TRUE)

  # After the build, the user's original file is restored verbatim
  expect_true(fs::file_exists(yml))
  expect_identical(readLines(yml), user_contents)
})

test_that("with_quarto_project() merged file includes user keys when shinylive requested", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  writeLines(c("project:", "  type: website", "bibliography: refs.bib"), yml)

  state <- new.env()
  state$merged_during <- character()
  sandpaper:::with_quarto_project(tmp, {
    state$merged_during <- readLines(yml)
  }, shinylive = TRUE, quiet = TRUE)

  expect_true(any(grepl("bibliography: refs.bib", state$merged_during)))
  expect_true(any(grepl("shinylive", state$merged_during)))
})

test_that("with_quarto_project() logs info when merging existing user file", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  yml <- fs::path(sandpaper:::root_path(tmp), "_quarto.yml")
  writeLines(c("project:", "  type: website"), yml)

  expect_message(
    sandpaper:::with_quarto_project(tmp, NULL, quiet = FALSE),
    "Merging"
  )
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
