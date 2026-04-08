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
