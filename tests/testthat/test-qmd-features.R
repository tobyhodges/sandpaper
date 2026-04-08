# Tests for Quarto-specific feature support in the build pipeline.
# These test that Quarto features are correctly mapped to the formats
# that sandpaper and varnish expect.

test_that("Quarto callouts are mapped to Carpentries divs", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  skip_if_not(sandpaper:::jupyter_available(), "Jupyter not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(c(
    "---",
    "title: 'Callout Test'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "```{python}",
    "x = 1",
    "```",
    "",
    "::: {.callout-note}",
    "## My Note",
    "This is a note callout.",
    ":::",
    "",
    "::: {.callout-warning}",
    "This is a warning callout.",
    ":::"
  ), qmd_path)
  outdir <- fs::path(tmp, "site", "built")
  fs::dir_create(outdir)
  result <- build_episode_md(qmd_path, outdir = outdir, workdir = outdir)
  content <- readLines(result)
  # callout-note should become a callout div
  expect_true(any(grepl("^::: callout", content)))
  # callout-warning should become a caution div
  expect_true(any(grepl("^::: caution", content)))
  # GFM alert syntax should not remain
  expect_false(any(grepl("\\[!NOTE\\]", content)))
  expect_false(any(grepl("\\[!WARNING\\]", content)))
})

test_that("Quarto panel-tabset is mapped to Carpentries tab div", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  skip_if_not(sandpaper:::jupyter_available(), "Jupyter not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(c(
    "---",
    "title: 'Tab Test'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "```{python}",
    "x = 1",
    "```",
    "",
    "::: {.panel-tabset}",
    "",
    "### Python",
    "```python",
    "print('hello')",
    "```",
    "",
    "### R",
    "```r",
    "print('hello')",
    "```",
    "",
    ":::"
  ), qmd_path)
  outdir <- fs::path(tmp, "site", "built")
  fs::dir_create(outdir)
  result <- build_episode_md(qmd_path, outdir = outdir, workdir = outdir)
  content <- readLines(result)
  # Should have a tab div, not panel-tabset
  expect_true(any(grepl("^::: tab", content)))
  expect_false(any(grepl("panel-tabset", content)))
})

test_that("Mermaid code blocks pass through for varnish rendering", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  skip_if_not(sandpaper:::jupyter_available(), "Jupyter not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(c(
    "---",
    "title: 'Mermaid Test'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "```{python}",
    "x = 1",
    "```",
    "",
    "```mermaid",
    "graph LR",
    "  A --> B",
    "  B --> C",
    "```"
  ), qmd_path)
  outdir <- fs::path(tmp, "site", "built")
  fs::dir_create(outdir)
  result <- build_episode_md(qmd_path, outdir = outdir, workdir = outdir)
  content <- readLines(result)
  # Mermaid block should be preserved as a fenced code block
  expect_true(any(grepl("^``` mermaid", content) | grepl("^```mermaid", content)))
  expect_true(any(grepl("graph LR", content)))
})

test_that("Inline Python expressions are resolved", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  skip_if_not(sandpaper:::jupyter_available(), "Jupyter not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(c(
    "---",
    "title: 'Inline Test'",
    "teaching: 10",
    "exercises: 2",
    "---",
    "",
    "```{python}",
    "answer = 42",
    "```",
    "",
    "The answer is `{python} answer`."
  ), qmd_path)
  outdir <- fs::path(tmp, "site", "built")
  fs::dir_create(outdir)
  result <- build_episode_md(qmd_path, outdir = outdir, workdir = outdir)
  content <- readLines(result)
  # Inline expression should be resolved to the value
  expect_true(any(grepl("The answer is 42", content)))
  # Raw expression syntax should not remain
  expect_false(any(grepl("\\{python\\} answer", content)))
})
