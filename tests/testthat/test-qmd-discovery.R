# Tests for .qmd file discovery in sandpaper

test_that("get_resource_list() discovers .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  # Add a .qmd episode
  writeLines(
    "---\ntitle: 'Quarto Episode'\nteaching: 10\nexercises: 2\n---\n\nHello from Quarto\n",
    fs::path(tmp, "episodes", "quarto-test.qmd")
  )
  res <- get_resource_list(tmp)
  qmd_files <- grep("\\.qmd$", res$episodes, value = TRUE)
  expect_length(qmd_files, 1)
})

test_that("get_sources() discovers .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(
    "---\ntitle: 'Quarto Episode'\nteaching: 10\nexercises: 2\n---\n\nHello\n",
    fs::path(tmp, "episodes", "quarto-test.qmd")
  )
  sources <- sandpaper:::get_sources(tmp)
  qmd_sources <- grep("\\.qmd$", sources, value = TRUE)
  expect_length(qmd_sources, 1)
})

test_that("get_source_artifacts() excludes .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(
    "---\ntitle: 'Quarto Episode'\n---\n\nHello\n",
    fs::path(tmp, "episodes", "quarto-test.qmd")
  )
  artifacts <- sandpaper:::get_source_artifacts(tmp)
  qmd_artifacts <- grep("\\.qmd$", artifacts, value = TRUE)
  expect_length(qmd_artifacts, 0)
})

test_that("set_dropdown() lists .qmd episodes", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(
    "---\ntitle: 'Quarto Episode'\nteaching: 10\nexercises: 2\n---\n\nHello\n",
    fs::path(tmp, "episodes", "quarto-test.qmd")
  )
  set_episodes(tmp, order = c("introduction.md", "quarto-test.qmd"), write = TRUE)
  eps <- get_episodes(tmp)
  expect_true("quarto-test.qmd" %in% eps)
})
