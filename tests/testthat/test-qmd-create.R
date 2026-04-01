# Tests for .qmd episode creation

test_that("create_episode() accepts ext = 'qmd'", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  ep <- create_episode("test-quarto", ext = "qmd", path = tmp, open = FALSE)
  expect_true(fs::file_exists(ep))
  expect_equal(fs::path_ext(ep), "qmd")
})

test_that("create_episode_qmd() creates a .qmd file", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  ep <- create_episode_qmd("quarto-episode", path = tmp, open = FALSE)
  expect_true(fs::file_exists(ep))
  expect_equal(fs::path_ext(ep), "qmd")
  # Should appear in episode list
  eps <- get_episodes(tmp)
  expect_true("quarto-episode.qmd" %in% eps)
})

test_that("draft_episode_qmd() creates a draft .qmd file", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  ep <- draft_episode_qmd("draft-quarto", path = tmp, open = FALSE)
  expect_true(fs::file_exists(ep))
  expect_equal(fs::path_ext(ep), "qmd")
  # Draft should NOT appear in episode list
  eps <- get_episodes(tmp)
  expect_false("draft-quarto.qmd" %in% eps)
})

test_that("create_episode_qmd() sets correct YAML frontmatter", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  ep <- create_episode_qmd("yaml-test", path = tmp, open = FALSE)
  content <- readLines(ep)
  yaml_lines <- content[2:(which(content == "---")[2] - 1)]
  expect_true(any(grepl("title:", yaml_lines)))
})
