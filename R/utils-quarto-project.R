# Helpers for managing a transient Quarto project (`_quarto.yml`) at the
# lesson root during a build. The Quarto rendering pipeline for `.qmd` files
# requires a `_quarto.yml` to be discoverable via walk-up from the source
# file. Sandpaper writes one on demand at the start of a build and removes
# it on completion. If the lesson author already has a `_quarto.yml` the
# original is preserved and restored; sandpaper writes a merged copy that
# adds only the keys it needs.
#
# The generated file is tagged with `sandpaper_sentinel` as a header comment
# so that crashed builds can be cleaned up on the next run.

sandpaper_sentinel <- "# sandpaper-managed transient Quarto project file (do not edit)"

# Check whether the lesson has the shinylive Quarto extension installed.
#
# @param path the path to the lesson root
# @return logical, TRUE if `_extensions/quarto-ext/shinylive/` exists
# @keywords internal
has_shinylive <- function(path) {
  ext <- fs::path(root_path(path), "_extensions", "quarto-ext", "shinylive")
  fs::dir_exists(ext)
}

# Scan all `.qmd` files in the lesson for executable code cell language
# markers (`{python}`, `{r}`, `{julia}`). Shinylive cells (`{shinylive-r}`,
# `{shinylive-python}`) are NOT counted because they are intercepted by the
# shinylive Lua filter and never executed by the Quarto engine.
#
# @param path the path to the lesson root
# @return a character vector of unique executable engine languages found
# @keywords internal
detect_qmd_executable_engines <- function(path) {
  root <- root_path(path)
  qmd_files <- fs::dir_ls(root, glob = "*.qmd", recurse = TRUE, fail = FALSE)
  if (length(qmd_files) == 0) return(character())
  langs <- character()
  re <- "^[[:space:]]*```\\{([a-zA-Z][a-zA-Z0-9]*)[[:space:]}]"
  for (f in qmd_files) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    m <- regmatches(lines, regexec(re, lines))
    for (match in m) {
      if (length(match) == 2L) langs <- c(langs, match[2])
    }
  }
  unique(langs)
}

# Build the YAML content string for a sandpaper-managed transient
# `_quarto.yml`. Pure function — no filesystem side effects.
#
# @param shinylive logical, whether to include `filters: [shinylive]`
# @param engine NULL or a string, e.g. "knitr" to force the engine
# @return a single character string, the YAML document
# @keywords internal
build_quarto_project_yaml <- function(shinylive = FALSE, engine = NULL) {
  lines <- c(
    sandpaper_sentinel,
    "# This file is written at the start of every build and removed on",
    "# completion. Edits will be lost.",
    "project:",
    "  type: website"
  )
  if (isTRUE(shinylive)) {
    lines <- c(lines, "filters:", "  - shinylive")
  }
  if (!is.null(engine) && nzchar(engine)) {
    lines <- c(lines, paste0("engine: ", engine))
  }
  paste(lines, collapse = "\n")
}

# Detect whether an existing `_quarto.yml` at the given path was written by
# sandpaper (has the sentinel header). Orphaned files from a crashed build
# are treated as absent and overwritten.
#
# @param yml_path path to a `_quarto.yml` file
# @return logical
# @keywords internal
is_sandpaper_quarto_yml <- function(yml_path) {
  if (!fs::file_exists(yml_path)) return(FALSE)
  first <- tryCatch(readLines(yml_path, n = 1L, warn = FALSE),
    error = function(e) character())
  length(first) >= 1L && identical(first, sandpaper_sentinel)
}

# Run `expr` with a transient `_quarto.yml` present at the lesson root.
#
# Behavior:
# - If no file exists, writes a generated sandpaper-managed file and removes
#   it on exit (success, error, or interrupt).
# - If a sandpaper-managed orphan file exists (leftover from a crashed
#   build), treats it as absent and overwrites it.
# - If a user-authored file exists, backs it up, writes a merged copy that
#   adds the keys sandpaper needs without losing any of the user's keys,
#   and restores the original on exit. Logs `cli_alert_info` when merging.
#
# @param path path to the lesson root
# @param expr an expression to evaluate while the transient file is present
# @param shinylive logical, whether to include `filters: [shinylive]`
# @param engine NULL or a string to force the engine
# @param quiet if TRUE, suppress info messages
# @return the value of `expr`
# @keywords internal
with_quarto_project <- function(path, expr, shinylive = FALSE, engine = NULL,
                                quiet = FALSE) {
  stop("with_quarto_project() not yet implemented")
}
