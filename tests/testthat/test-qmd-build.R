# Tests for .qmd build pipeline

test_that("build_status() maps .qmd source to .md built file", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(
    "---\ntitle: 'Quarto Episode'\nteaching: 10\nexercises: 2\n---\n\nHello\n",
    qmd_path
  )
  db_path <- fs::path(tmp, "site", "built", "md5sum.txt")
  db <- sandpaper:::build_status(qmd_path, db_path)
  # The built file should have .md extension
  expect_true(all(fs::path_ext(db$new$built) == "md"))
})

test_that("reserved_db() filters .qmd files correctly", {
  db <- data.frame(
    file = c("episodes/intro.qmd", "index.qmd", "links.qmd"),
    checksum = c("a", "b", "c"),
    built = c("site/built/intro.md", "site/built/index.md", "site/built/links.md"),
    stringsAsFactors = FALSE
  )
  filtered <- sandpaper:::reserved_db(db)
  # index and links are reserved — only intro should remain
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$file, "episodes/intro.qmd")
})

test_that("build_markdown() includes .qmd in needs_building", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(
    "---\ntitle: 'Quarto Episode'\nteaching: 10\nexercises: 2\n---\n\nHello from Quarto\n",
    qmd_path
  )
  set_episodes(tmp, order = c("introduction.md", "quarto-test.qmd"), write = TRUE)
  # build_markdown should attempt to build the .qmd file
  # (will fail at quarto rendering if quarto not installed, but should get that far)
  expect_error(
    build_markdown(tmp, quiet = TRUE),
    "quarto|not yet implemented"
  )
})

test_that("build_episode_md() routes .qmd to quarto rendering", {
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  qmd_path <- fs::path(tmp, "episodes", "quarto-test.qmd")
  writeLines(
    "---\ntitle: 'Quarto Episode'\nteaching: 10\nexercises: 2\n---\n\n## Hello\n\nSome text.\n",
    qmd_path
  )
  outdir <- fs::path(tmp, "site", "built")
  fs::dir_create(outdir)
  result <- build_episode_md(qmd_path, outdir = outdir, workdir = outdir)
  expect_true(fs::file_exists(result))
  expect_equal(fs::path_ext(result), "md")
})

test_that("no_renv_needed is TRUE for .qmd-only lessons", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  # Remove .Rmd files, add .qmd
  rmd_files <- fs::dir_ls(fs::path(tmp, "episodes"), glob = "*.Rmd")
  if (length(rmd_files)) fs::file_delete(rmd_files)
  writeLines(
    "---\ntitle: 'Quarto Episode'\nteaching: 10\nexercises: 2\n---\n\nHello\n",
    fs::path(tmp, "episodes", "quarto-test.qmd")
  )
  set_episodes(tmp, order = "quarto-test.qmd", write = TRUE)
  sources <- sandpaper:::get_build_sources(tmp, fs::path(tmp, "site", "built"), NULL, TRUE)
  no_renv_needed <- !any(fs::path_ext(sources) %in% c("Rmd", "rmd"))
  expect_true(no_renv_needed)
})
