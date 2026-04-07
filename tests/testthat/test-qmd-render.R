# Tests for end-to-end rendering of .qmd files with executable code

test_that("build_episode_md() renders executable code in a .qmd file", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  skip_if_not(sandpaper:::jupyter_available(), "Jupyter not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(c(
    "---",
    "title: 'Quarto Render Test'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "## Code example",
    "",
    "```{python}",
    "print('hello from quarto')",
    "```",
    ""
  ), qmd_path)
  outdir <- fs::path(tmp, "site", "built")
  fs::dir_create(outdir)
  result <- build_episode_md(qmd_path, outdir = outdir, workdir = outdir)
  expect_true(fs::file_exists(result))
  content <- readLines(result)
  expect_true(any(grepl("hello from quarto", content)))
})

test_that("build_markdown() renders a .qmd episode end-to-end", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  skip_if_not(sandpaper:::jupyter_available(), "Jupyter not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  # Replace .Rmd with .qmd containing executable code
  fs::file_delete(fs::path(tmp, "episodes", "introduction.Rmd"))
  qmd_path <- fs::path(tmp, "episodes", "introduction.qmd")
  writeLines(c(
    "---",
    "title: 'Introduction'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "::::::::::::::::::::::::::::::::::::: questions",
    "",
    "- How does Quarto work?",
    "",
    "::::::::::::::::::::::::::::::::::::::::::::::::",
    "",
    "::::::::::::::::::::::::::::::::::::: objectives",
    "",
    "- Render a Quarto document",
    "",
    "::::::::::::::::::::::::::::::::::::::::::::::::",
    "",
    "## Introduction",
    "",
    "```{python}",
    "x = 1 + 1",
    "print(f'The answer is {x}')",
    "```",
    "",
    "::::::::::::::::::::::::::::::::::::: keypoints",
    "",
    "- Quarto works",
    "",
    "::::::::::::::::::::::::::::::::::::::::::::::::"
  ), qmd_path)
  set_episodes(tmp, order = "introduction.qmd", write = TRUE)
  sandpaper:::.resources$clear()
  expect_no_error(build_markdown(tmp, quiet = TRUE))
  built_md <- fs::path(tmp, "site", "built", "introduction.md")
  expect_true(fs::file_exists(built_md))
  content <- readLines(built_md)
  expect_true(any(grepl("The answer is 2", content)))
})
