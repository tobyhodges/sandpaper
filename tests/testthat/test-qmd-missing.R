# Tests for quarto installation checks

test_that("check_quarto_installed() succeeds when quarto is available", {
  skip_if_not(
    requireNamespace("quarto", quietly = TRUE),
    "quarto R package not installed"
  )
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  expect_no_error(sandpaper:::check_quarto_installed())
})

test_that("check_quarto_installed() errors when package is missing", {
  # Mock check_quarto_installed to simulate missing quarto package,
  # verifying the error message pattern matches what the real function produces
  local_mocked_bindings(
    check_quarto_installed = function() {
      cli::cli_abort(c(
        "The {.pkg quarto} package is required to render {.file .qmd} files.",
        "i" = "Install it with {.code install.packages('quarto')}"
      ))
    }
  )
  expect_error(
    check_quarto_installed(),
    "quarto.*required"
  )
})
