# Tests for check_quarto_installed().

test_that("check_quarto_installed() succeeds when quarto is available", {
  skip_if_not(
    requireNamespace("quarto", quietly = TRUE),
    "quarto R package not installed"
  )
  skip_if_not(nzchar(Sys.which("quarto")), "Quarto CLI not installed")
  expect_no_error(sandpaper:::check_quarto_installed())
})

test_that("check_quarto_installed() errors when the Quarto CLI is missing", {
  # Exercise the real function on the CLI-missing branch by stubbing
  # quarto::quarto_path() to return NULL. The requireNamespace() branch
  # is not directly covered: simulating a missing quarto package
  # in-process is unreliable, and a subprocess test would add a callr
  # dependency for a single informational error message.
  skip_if_not(
    requireNamespace("quarto", quietly = TRUE),
    "quarto R package not installed"
  )
  testthat::with_mocked_bindings(
    quarto_path = function() NULL,
    {
      expect_error(
        sandpaper:::check_quarto_installed(),
        "Quarto CLI"
      )
    },
    .package = "quarto"
  )
})
