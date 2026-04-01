# Tests for informative errors when quarto is missing

test_that("check_quarto_installed() errors without quarto package", {
  # Mock requireNamespace to return FALSE
  mockr::with_mock(
    requireNamespace = function(...) FALSE,
    expect_error(
      check_quarto_installed(),
      "quarto.*required"
    ),
    .env = asNamespace("sandpaper")
  )
})
