# Tests for .qmd file discovery in sandpaper
#
# These tests replace the .Rmd episode with a .qmd episode to verify that
# .qmd files are discovered as first-class source files.

test_that("get_resource_list() discovers .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  # Replace the .Rmd episode with a .qmd episode
  fs::file_delete(fs::path(tmp, "episodes", "introduction.Rmd"))
  writeLines(
    "---\ntitle: 'Introduction'\nteaching: 10\nexercises: 2\n---\n\nHello from Quarto\n",
    fs::path(tmp, "episodes", "introduction.qmd")
  )
  set_episodes(tmp, order = "introduction.qmd", write = TRUE)
  res <- get_resource_list(tmp)
  expect_true(any(grepl("introduction\\.qmd$", res$episodes)))
})

test_that("get_sources() discovers .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  fs::file_delete(fs::path(tmp, "episodes", "introduction.Rmd"))
  writeLines(
    "---\ntitle: 'Introduction'\nteaching: 10\nexercises: 2\n---\n\nHello\n",
    fs::path(tmp, "episodes", "introduction.qmd")
  )
  sources <- sandpaper:::get_sources(tmp)
  expect_true(any(grepl("introduction\\.qmd$", sources)))
})

test_that("get_source_artifacts() excludes .qmd files", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(
    "---\ntitle: 'Introduction'\n---\n\nHello\n",
    fs::path(tmp, "episodes", "introduction.qmd")
  )
  artifacts <- sandpaper:::get_source_artifacts(tmp)
  qmd_artifacts <- grep("\\.qmd$", artifacts, value = TRUE)
  expect_length(qmd_artifacts, 0)
})

test_that("set_dropdown() lists .qmd episodes", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  fs::file_delete(fs::path(tmp, "episodes", "introduction.Rmd"))
  writeLines(
    "---\ntitle: 'Introduction'\nteaching: 10\nexercises: 2\n---\n\nHello\n",
    fs::path(tmp, "episodes", "introduction.qmd")
  )
  set_episodes(tmp, order = "introduction.qmd", write = TRUE)
  eps <- get_episodes(tmp)
  expect_true("introduction.qmd" %in% eps)
})
