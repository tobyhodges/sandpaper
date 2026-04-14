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

# Build the YAML content string for a sandpaper-managed transient
# `_quarto.yml`. Pure function — no filesystem side effects.
#
# @param engine NULL or a string, e.g. "knitr" to force the engine
# @param execute_error NULL or a logical. When non-NULL, emit
#   `execute.error: true/false` to control whether Quarto halts on a
#   code cell error (TRUE = capture errors and continue, FALSE = halt).
#   This mirrors knitr's `error` option for `.Rmd` lessons so `.qmd`
#   lessons inherit sandpaper's `fail_on_error` config uniformly.
# @return a single character string, the YAML document
# @keywords internal
build_quarto_project_yaml <- function(engine = NULL, execute_error = NULL) {
  lines <- c(
    sandpaper_sentinel,
    "# This file is written at the start of every build and removed on",
    "# completion. Edits will be lost.",
    "project:",
    "  type: default"
  )
  if (!is.null(engine) && nzchar(engine)) {
    lines <- c(lines, paste0("engine: ", engine))
  }
  if (!is.null(execute_error)) {
    lines <- c(lines, "execute:",
      paste0("  error: ", if (isTRUE(execute_error)) "true" else "false"))
  }
  paste(lines, collapse = "\n")
}

# Reject a user-authored `_quarto.yml` whose `project.type` redirects
# rendered output to a subdirectory. Sandpaper expects the built `.md`
# to land next to its source, so `website`, `book`, and `manuscript`
# project types break the build. Abort up front with a clear message
# rather than letting Quarto silently emit to a directory sandpaper
# does not look in.
#
# @param user_lines character vector, contents of the user's `_quarto.yml`
# @param yml_path path used only for the error message
# @return invisible NULL on success, stops with `cli_abort` otherwise
# @keywords internal
check_user_quarto_project_compat <- function(user_lines, yml_path) {
  user_data <- tryCatch(
    yaml::yaml.load(paste(user_lines, collapse = "\n"), eval.expr = FALSE),
    error = function(e) NULL
  )
  if (!is.list(user_data)) return(invisible())
  project_type <- user_data$project$type
  incompatible <- c("website", "book", "manuscript")
  if (!is.null(project_type) && project_type %in% incompatible) {
    cli::cli_abort(c(
      "{.file {yml_path}} has {.code project.type: {project_type}}, which is incompatible with sandpaper.",
      "i" = "Quarto's {.val {project_type}} project type redirects rendered output to a subdirectory.",
      "i" = "Sandpaper needs the built {.file .md} to land next to its source.",
      "x" = "Change it to {.code project.type: default} or remove the {.code project} key."
    ))
  }
  invisible()
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

# Merge the sandpaper-required keys into a user-authored `_quarto.yml`,
# returning a character vector of lines for the merged document. The
# returned content starts with the sandpaper sentinel so the merged file
# is distinguishable from both the user's original and from plain sandpaper
# builds. Adds only missing keys; never clobbers user values.
#
# @param user_lines character vector, contents of the user's `_quarto.yml`
# @param engine NULL or a string, forced `engine` key if not set by user
# @param execute_error NULL or a logical. When non-NULL and the user
#   has not already set `execute.error`, adds it. Never clobbers an
#   existing user value — author frontmatter / project-level
#   intentional overrides win.
# @param quiet if TRUE, suppress the "added keys" warning
# @return a character vector of lines suitable for `writeLines()`
# @keywords internal
merge_quarto_yaml <- function(user_lines, engine = NULL, execute_error = NULL,
                              quiet = FALSE) {
  user_content <- paste(user_lines, collapse = "\n")
  user_data <- tryCatch(
    yaml::yaml.load(user_content, eval.expr = FALSE),
    error = function(e) NULL
  )
  if (is.null(user_data) || !is.list(user_data)) user_data <- list()

  added <- character()

  # Ensure project.type is set. `default` is used rather than `website`
  # because `website` redirects rendered output to `_site/`, which breaks
  # sandpaper's expectation that the built .md lands next to its source.
  if (is.null(user_data$project)) {
    user_data$project <- list(type = "default")
    added <- c(added, "project.type")
  } else if (is.null(user_data$project$type)) {
    user_data$project$type <- "default"
    added <- c(added, "project.type")
  }

  if (!is.null(engine) && nzchar(engine) && is.null(user_data$engine)) {
    user_data$engine <- engine
    added <- c(added, "engine")
  }

  if (!is.null(execute_error)) {
    if (is.null(user_data$execute)) {
      user_data$execute <- list(error = isTRUE(execute_error))
      added <- c(added, "execute.error")
    } else if (is.null(user_data$execute$error)) {
      user_data$execute$error <- isTRUE(execute_error)
      added <- c(added, "execute.error")
    }
  }

  if (!quiet && length(added)) {
    cli::cli_alert_warning(
      "Added {length(added)} key{?s} not present in {.file _quarto.yml}: {.val {added}}"
    )
  }

  # Quarto uses YAML 1.2 (strict `true`/`false` booleans) while R's yaml
  # package defaults to YAML 1.1 output (`yes`/`no`). Override the logical
  # handler so merged files parse under Quarto.
  serialized <- yaml::as.yaml(user_data, handlers = list(
    logical = function(x) {
      v <- if (isTRUE(x)) "true" else "false"
      class(v) <- "verbatim"
      v
    }
  ))
  c(
    sandpaper_sentinel,
    "# Merged from user _quarto.yml; original restored on build completion.",
    strsplit(serialized, "\n", fixed = TRUE)[[1]]
  )
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
# @param engine NULL or a string to force the engine
# @param execute_error NULL or a logical propagated to
#   `build_quarto_project_yaml()` / `merge_quarto_yaml()` to control
#   whether Quarto halts on cell errors. Mirrors knitr's `error`
#   option from sandpaper's `fail_on_error` config.
# @param quiet if TRUE, suppress info messages
# @return the value of `expr`
# @keywords internal
with_quarto_project <- function(path, expr, engine = NULL,
                                execute_error = NULL, quiet = FALSE) {
  root <- root_path(path)
  yml <- fs::path(root, "_quarto.yml")

  has_user_file <- fs::file_exists(yml) && !is_sandpaper_quarto_yml(yml)
  backup <- NULL

  if (has_user_file) {
    user_lines <- readLines(yml, warn = FALSE)
    # Fails fast before we back up or touch anything on disk.
    check_user_quarto_project_compat(user_lines, yml)
    if (!quiet) {
      cli::cli_alert_info(
        "Merging sandpaper Quarto project config into existing {.file _quarto.yml}"
      )
    }
    backup <- fs::file_temp(pattern = "sandpaper-quarto-yml-", ext = "yml")
    fs::file_copy(yml, backup, overwrite = TRUE)
    merged <- merge_quarto_yaml(user_lines, engine = engine,
      execute_error = execute_error, quiet = quiet)
    writeLines(merged, yml)
  } else {
    # No user file, or an orphaned sandpaper file from a crashed build:
    # overwrite with freshly generated content.
    writeLines(
      build_quarto_project_yaml(engine = engine, execute_error = execute_error),
      yml
    )
  }

  on.exit({
    if (is.null(backup)) {
      if (fs::file_exists(yml)) {
        tryCatch(fs::file_delete(yml), error = function(e) NULL)
      }
    } else {
      tryCatch({
        fs::file_copy(backup, yml, overwrite = TRUE)
        fs::file_delete(backup)
      }, error = function(e) NULL)
    }
  }, add = TRUE)

  force(expr)
}
