# Tests for create_lesson(format = "qmd")

{
  tmpdir <- fs::file_temp()
  fs::dir_create(tmpdir)
  tmp_qmd <- fs::path(tmpdir, "lesson-qmd-example")
  withr::defer(fs::dir_delete(tmpdir))
}

test_that("create_lesson(format = 'qmd') creates a lesson with introduction.qmd", {
  suppressMessages({capture.output({
    res <- create_lesson(tmp_qmd, name = "QUARTO LESSON", format = "qmd",
      rstudio = FALSE, open = FALSE)
  }) %>%
    expect_message("Lesson successfully created")
  })
  tmp_qmd <<- normalizePath(tmp_qmd)
  expect_true(fs::file_exists(fs::path(tmp_qmd, "episodes", "introduction.qmd")))
  expect_false(fs::file_exists(fs::path(tmp_qmd, "episodes", "introduction.Rmd")))
  expect_false(fs::file_exists(fs::path(tmp_qmd, "episodes", "introduction.md")))
})

test_that("create_lesson(format = 'qmd') includes environment.yml", {
  expect_true(fs::file_exists(fs::path(tmp_qmd, "environment.yml")))
  env <- readLines(fs::path(tmp_qmd, "environment.yml"))
  expect_true(any(grepl("python", env)))
  expect_true(any(grepl("jupyter", env)))
})

test_that("create_lesson(format = 'qmd') does not create renv directory", {
  expect_false(fs::dir_exists(fs::path(tmp_qmd, "renv")))
})

test_that("create_lesson(format = 'qmd') has .quarto/ in .gitignore", {
  gi <- readLines(fs::path(tmp_qmd, ".gitignore"))
  expect_true(any(grepl("\\.quarto", gi)))
})

test_that("format parameter takes precedence over rmd with a warning", {
  tmp2 <- fs::path(tmpdir, "lesson-format-wins")
  withr::defer(fs::dir_delete(tmp2))
  expect_warning(
    suppressMessages(capture.output(
      create_lesson(tmp2, format = "md", rmd = TRUE, rstudio = FALSE, open = FALSE)
    )),
    "deprecated"
  )
  tmp2 <- normalizePath(tmp2)
  expect_true(fs::file_exists(fs::path(tmp2, "episodes", "introduction.md")))
  expect_false(fs::file_exists(fs::path(tmp2, "episodes", "introduction.Rmd")))
})
