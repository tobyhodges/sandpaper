check_quarto_installed <- function() {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg quarto} package is required to render {.file .qmd} files.",
      "i" = "Install it with {.code install.packages('quarto')}"
    ))
  }
  if (is.null(quarto::quarto_path())) {
    cli::cli_abort(c(
      "The Quarto CLI is required to render {.file .qmd} files but was not found.",
      "i" = "Install it from {.url https://quarto.org/docs/get-started/}"
    ))
  }
}
