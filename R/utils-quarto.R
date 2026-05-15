# Check if Jupyter is available for Quarto to use
# @return logical
# @keywords internal
jupyter_available <- function() {
  res <- tryCatch(
    system2("quarto", c("check", "jupyter"), stdout = TRUE, stderr = TRUE),
    error = function(e) "not available"
  )
  !any(grepl("not available", res, ignore.case = TRUE))
}

# Check that conda (or mamba) is available
# @return the path to the conda/mamba executable, or NULL
# @keywords internal
find_conda <- function() {
  mamba <- Sys.which("mamba")
  if (nzchar(mamba)) return(mamba)
  conda <- Sys.which("conda")
  if (nzchar(conda)) return(conda)
  NULL
}

# Set up the conda environment for a quarto lesson
#
# Creates or updates a prefix conda environment at `.conda/` from the lesson's
# `environment.yml`. Returns the path to the Python binary in that environment.
#
# @param path the path to the lesson root
# @param quiet if TRUE, suppress output
# @return the path to the Python binary in the conda environment
# @keywords internal
setup_quarto_env <- function(path, quiet = FALSE) {
  root <- root_path(path)
  env_yml <- fs::path(root, "environment.yml")
  conda_prefix <- fs::path(root, ".conda")
  python_path <- fs::path(conda_prefix, "bin", "python")
  hash_file <- fs::path(conda_prefix, ".env_hash")

  if (!fs::file_exists(env_yml)) {
    return(NULL)
  }

  conda <- find_conda()
  if (is.null(conda)) {
    cli::cli_abort(c(
      "conda or mamba is required to manage dependencies for {.file .qmd} lessons.",
      "i" = "Install miniforge from {.url https://github.com/conda-forge/miniforge}"
    ))
  }

  # Hash environment.yml to detect changes
  current_hash <- unname(tools::md5sum(env_yml))
  env_exists <- fs::file_exists(python_path)
  stored_hash <- if (env_exists && fs::file_exists(hash_file)) {
    readLines(hash_file, n = 1L)
  } else {
    ""
  }
  up_to_date <- env_exists && identical(current_hash, stored_hash)

  if (up_to_date) {
    if (!quiet) {
      cli::cli_alert_info("Using conda environment in {.file {conda_prefix}}")
    }
    return(python_path)
  }

  # Create or update the environment
  action <- if (env_exists) "Updating" else "Creating"
  if (!quiet) {
    cli::cli_alert_info("{action} conda environment from {.file environment.yml}")
  }
  subcmd <- if (env_exists) "update" else "create"
  yes_flag <- if (subcmd == "create") "--yes" else NULL
  res <- system2(conda,
    args = c("env", subcmd, "-p", conda_prefix, "-f", env_yml, yes_flag,
      if (quiet) "--quiet"),
    stdout = if (quiet) FALSE else "",
    stderr = if (quiet) FALSE else ""
  )
  if (res != 0) {
    cli::cli_abort("Failed to {tolower(action)} conda environment from {.file {env_yml}}")
  }

  # Store the hash so we can detect future changes
  writeLines(current_hash, hash_file)

  python_path
}

# Build the env var list for the qmd callr subprocess.
#
# Conda-forge's `quarto` package sets a family of `QUARTO_*`,
# `DENO_*`, and `TYPST_*` env vars from its
# `etc/conda/activate.d/*.sh` scripts. Removing the package (or
# pruning the env) deletes the scripts but does not unset the
# vars from any shell that was already activated. Stale vars
# pointing at deleted paths can silently deadlock the Quarto CLI:
# notably `QUARTO_DENO_DOM` pointing at a path from the
# conda-forge build server that never existed locally, where
# Quarto parks on a recvfrom waiting for an FFI plugin that will
# never load. Scrub every such inherited var before invoking
# Quarto, then re-set `QUARTO_PYTHON` to the conda env's Python
# when one is available.
#
# @param python_path NULL or a path to the python binary in a
#   conda env (typically the value returned by `setup_quarto_env()`)
# @return a named character vector suitable for `callr::r(env = ...)`
# @keywords internal
quarto_callr_env <- function(python_path = NULL) {
  inherited <- grep("^(QUARTO_|DENO_|TYPST_)",
    names(Sys.getenv()), value = TRUE)
  if (!is.null(python_path)) {
    inherited <- setdiff(inherited, "QUARTO_PYTHON")
  }
  unset <- if (length(inherited)) {
    stats::setNames(rep(NA_character_, length(inherited)), inherited)
  } else {
    character()
  }
  set <- if (!is.null(python_path)) {
    c(QUARTO_PYTHON = python_path)
  } else {
    character()
  }
  c(callr::rcmd_safe_env(), unset, set)
}

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
