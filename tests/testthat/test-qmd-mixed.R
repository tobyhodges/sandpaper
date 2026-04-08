# Tests for mixed format lessons (.Rmd and .qmd episodes coexisting)

test_that("lessons can contain both .Rmd and .qmd episodes", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  # Fixture already has introduction.Rmd; add a .qmd episode
  writeLines(c(
    "---",
    "title: 'Quarto Episode'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "Hello from Quarto"
  ), fs::path(tmp, "episodes", "quarto-episode.qmd"))
  set_episodes(tmp, order = c("introduction.Rmd", "quarto-episode.qmd"),
    write = TRUE)
  eps <- get_episodes(tmp)
  expect_true("introduction.Rmd" %in% eps)
  expect_true("quarto-episode.qmd" %in% eps)
})

test_that("get_sources() discovers both .Rmd and .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(c(
    "---",
    "title: 'Quarto Episode'",
    "---",
    "",
    "Hello"
  ), fs::path(tmp, "episodes", "quarto-episode.qmd"))
  sources <- sandpaper:::get_sources(tmp)
  expect_true(any(grepl("\\.Rmd$", sources)))
  expect_true(any(grepl("\\.qmd$", sources)))
})

test_that("no_renv_needed is FALSE for mixed .Rmd and .qmd lessons", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(c(
    "---",
    "title: 'Quarto Episode'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "Hello"
  ), fs::path(tmp, "episodes", "quarto-episode.qmd"))
  set_episodes(tmp, order = c("introduction.Rmd", "quarto-episode.qmd"),
    write = TRUE)
  sandpaper:::.resources$clear()
  sources <- sandpaper:::get_build_sources(tmp, fs::path(tmp, "site", "built"),
    NULL, TRUE)
  no_renv_needed <- !any(fs::path_ext(sources) %in% c("Rmd", "rmd"))
  # Should be FALSE because .Rmd files are present
  expect_false(no_renv_needed)
})

test_that("check_lesson() passes for qmd lessons", {
  tmpdir <- fs::file_temp()
  fs::dir_create(tmpdir)
  tmp <- fs::path(tmpdir, "lesson-check-qmd")
  withr::defer(fs::dir_delete(tmpdir))
  suppressMessages(capture.output(
    create_lesson(tmp, format = "qmd", rstudio = FALSE, open = FALSE)
  ))
  expect_true(check_lesson(tmp))
})
