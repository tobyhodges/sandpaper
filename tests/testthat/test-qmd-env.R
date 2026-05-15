# Tests for conda environment management (setup_quarto_env, find_conda)

test_that("find_conda() returns conda or mamba path when available", {
  conda_path <- sandpaper:::find_conda()
  if (nzchar(Sys.which("conda")) || nzchar(Sys.which("mamba"))) {
    expect_true(!is.null(conda_path))
    expect_true(nzchar(conda_path))
  } else {
    expect_null(conda_path)
  }
})

test_that("setup_quarto_env() returns NULL when no environment.yml exists", {
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  # The default fixture has no environment.yml
  result <- sandpaper:::setup_quarto_env(tmp, quiet = TRUE)
  expect_null(result)
})

test_that("setup_quarto_env() creates conda env and hash file", {
  skip_if(is.null(sandpaper:::find_conda()), "conda/mamba not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(c("name: test", "dependencies:", "  - python>=3.11"),
    fs::path(tmp, "environment.yml"))
  result <- sandpaper:::setup_quarto_env(tmp, quiet = TRUE)
  expect_true(fs::file_exists(result))
  expect_true(fs::file_exists(fs::path(tmp, ".conda", ".env_hash")))
  # Hash should match current environment.yml
  stored_hash <- readLines(fs::path(tmp, ".conda", ".env_hash"), n = 1L)
  current_hash <- unname(tools::md5sum(fs::path(tmp, "environment.yml")))
  expect_equal(stored_hash, current_hash)
  # Clean up conda env
  unlink(fs::path(tmp, ".conda"), recursive = TRUE)
})

test_that("quarto_callr_env() unsets inherited QUARTO_*, DENO_*, TYPST_* vars", {
  # Conda-forge's quarto activate scripts export a family of these
  # env vars, and they persist after the package is removed,
  # pointing at deleted paths. The callr subprocess that invokes
  # Quarto must not inherit them or the Quarto CLI can silently
  # deadlock — notably on QUARTO_DENO_DOM pointing at a path from
  # the conda-forge build server that never existed locally.
  withr::with_envvar(c(
    QUARTO_PANDOC = "/stale/pandoc",
    QUARTO_DENO_DOM = "/buildbot/path",
    DENO_DOM_PLUGIN = "/stale.dylib",
    TYPST_PACKAGE_PATH = "/stale/typst"
  ), {
    env <- sandpaper:::quarto_callr_env()
    expect_true(is.na(env[["QUARTO_PANDOC"]]))
    expect_true(is.na(env[["QUARTO_DENO_DOM"]]))
    expect_true(is.na(env[["DENO_DOM_PLUGIN"]]))
    expect_true(is.na(env[["TYPST_PACKAGE_PATH"]]))
  })
})

test_that("quarto_callr_env() sets QUARTO_PYTHON when supplied", {
  env <- sandpaper:::quarto_callr_env(python_path = "/path/to/python")
  expect_equal(unname(env[["QUARTO_PYTHON"]]), "/path/to/python")
  expect_false(is.na(env[["QUARTO_PYTHON"]]))
})

test_that("quarto_callr_env() unsets inherited QUARTO_PYTHON when none supplied", {
  withr::with_envvar(c(QUARTO_PYTHON = "/stale/python"), {
    env <- sandpaper:::quarto_callr_env(python_path = NULL)
    expect_true(is.na(env[["QUARTO_PYTHON"]]))
  })
})

test_that("quarto_callr_env() explicit QUARTO_PYTHON wins over inherited", {
  # An inherited QUARTO_PYTHON must not result in a stray NA entry
  # that could shadow the explicit value when callr applies the env.
  withr::with_envvar(c(QUARTO_PYTHON = "/stale/python"), {
    env <- sandpaper:::quarto_callr_env(python_path = "/new/python")
    py <- env[names(env) == "QUARTO_PYTHON"]
    expect_length(py, 1L)
    expect_equal(unname(py), "/new/python")
  })
})

test_that("quarto_callr_env() leaves unrelated env vars alone", {
  # The helper only declares unsets for inherited QUARTO_/DENO_/TYPST_
  # vars. Unrelated inherited vars should not appear in the result
  # at all (they continue to be inherited by callr's default).
  withr::with_envvar(c(
    SANDPAPER_TEST_UNRELATED = "/keep/me",
    QUARTO_PANDOC = "/strip/me"
  ), {
    env <- sandpaper:::quarto_callr_env()
    expect_false("SANDPAPER_TEST_UNRELATED" %in% names(env))
    expect_true("QUARTO_PANDOC" %in% names(env))
  })
})

test_that("setup_quarto_env() detects environment.yml changes", {
  skip_if(is.null(sandpaper:::find_conda()), "conda/mamba not available")
  tmp <- restore_fixture()
  withr::defer(clear_globals())
  writeLines(c("name: test", "dependencies:", "  - python>=3.11"),
    fs::path(tmp, "environment.yml"))
  # First call creates the env
  result1 <- sandpaper:::setup_quarto_env(tmp, quiet = TRUE)
  hash1 <- readLines(fs::path(tmp, ".conda", ".env_hash"), n = 1L)
  # Modify environment.yml
  writeLines(c("name: test", "dependencies:", "  - python>=3.11", "  - pip"),
    fs::path(tmp, "environment.yml"))
  # Second call should detect the change
  result2 <- sandpaper:::setup_quarto_env(tmp, quiet = TRUE)
  hash2 <- readLines(fs::path(tmp, ".conda", ".env_hash"), n = 1L)
  expect_false(identical(hash1, hash2))
  # Clean up
  unlink(fs::path(tmp, ".conda"), recursive = TRUE)
})
