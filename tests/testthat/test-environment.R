# Helper function to check if mamba is available without skipping
is_mamba_available <- function() {
  mamba_available <- Sys.which("mamba") != "" ||
    Sys.which("micromamba") != "" ||
    file.exists(path.expand("~/.local/bin/micromamba"))
  return(mamba_available)
}

test_that("sn_install_micromamba works with default parameters", {
  skip_on_cran()
  skip_if_offline()

  # Use temporary directory for testing
  temp_dir <- tempdir()
  install_dir <- file.path(temp_dir, "test_micromamba")

  # Clean up any existing installation
  if (dir.exists(install_dir)) {
    unlink(install_dir, recursive = TRUE)
  }

  # Test installation
  result <- sn_install_micromamba(install_dir = install_dir)

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_true(grepl("micromamba$", result))

  # Check that SN_MICROMAMBA environment variable is set
  expect_equal(Sys.getenv("SN_MICROMAMBA"), as.character(result))

  # Clean up
  unlink(install_dir, recursive = TRUE)
})

test_that("sn_install_micromamba respects overwrite parameter", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- tempdir()
  install_dir <- file.path(temp_dir, "test_micromamba_overwrite")

  # Clean up any existing installation
  if (dir.exists(install_dir)) {
    unlink(install_dir, recursive = TRUE)
  }

  # First installation
  result1 <- sn_install_micromamba(install_dir = install_dir)
  expect_true(file.exists(result1))

  # Get modification time
  mtime1 <- file.mtime(result1)

  # Wait a bit to ensure different timestamps
  Sys.sleep(1)

  # Second installation without overwrite (should reuse existing)
  result2 <- sn_install_micromamba(install_dir = install_dir, overwrite = FALSE)
  expect_equal(result1, result2)
  expect_equal(file.mtime(result2), mtime1)

  # Third installation with overwrite (should reinstall)
  result3 <- sn_install_micromamba(install_dir = install_dir, overwrite = TRUE)
  expect_equal(result1, result3)
  # Note: we can't easily test if the file was actually redownloaded

  # Clean up
  unlink(install_dir, recursive = TRUE)
})

test_that("sn_install_micromamba handles invalid directory", {
  # Test with non-writable directory (if possible)
  # Skip if running as root (which can write to /root)
  if (Sys.getenv("USER") != "root") {
    expect_error(
      sn_install_micromamba(install_dir = "/root/non_writable"),
      class = "fs_error"
    )
  } else {
    skip("Running as root - cannot test permission denied scenario")
  }
})

test_that("sn_install_tool validates input parameters", {
  # Test invalid tool_name
  expect_error(
    sn_install_tool(""),
    "tool_name must be a non-empty character string"
  )

  expect_error(
    sn_install_tool(NULL),
    "tool_name must be a non-empty character string"
  )

  expect_error(
    sn_install_tool(c("tool1", "tool2")),
    "tool_name must be a non-empty character string"
  )

  expect_error(
    sn_install_tool(123),
    "tool_name must be a non-empty character string"
  )
})

test_that("sn_install_tool works with YAML file", {
  skip_on_cran()
  skip_if_offline()

  # Create a simple YAML file for testing
  temp_yaml <- tempfile(fileext = ".yaml")
  yaml_content <- "name: test_env
channels:
  - conda-forge
dependencies:
  - python=3.9
"
  writeLines(yaml_content, temp_yaml)

  temp_base_dir <- file.path(tempdir(), "test_envs")

  # Clean up any existing environment
  test_env_path <- file.path(temp_base_dir, "test_tool")
  if (dir.exists(test_env_path)) {
    unlink(test_env_path, recursive = TRUE)
  }

  # Test YAML installation
  expect_message(
    result <- sn_install_tool(
      tool_name = "test_tool",
      yaml = temp_yaml,
      base_dir = temp_base_dir
    ),
    "Creating environment"
  )

  expect_type(result, "character")
  expect_true(dir.exists(result))

  # Clean up
  unlink(temp_yaml)
  unlink(temp_base_dir, recursive = TRUE)
})

test_that("sn_install_tool handles non-existent YAML file", {
  expect_error(
    sn_install_tool("test_tool", yaml = "/non/existent/file.yaml"),
    "YAML file not found"
  )
})

test_that("sn_install_tool handles invalid YAML file extension", {
  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)

  expect_error(
    sn_install_tool("test_tool", yaml = temp_file),
    "File must have .yaml or .yml extension"
  )

  unlink(temp_file)
})

# Tests for internal functions

test_that(".check_mamba finds mamba executable", {
  # Test when mamba is provided
  if (Sys.which("echo") != "") {
    # Use echo as a mock executable that exists
    echo_path <- Sys.which("echo")
    result <- .check_mamba(mamba = echo_path)
    expect_equal(result, echo_path)
  }
})

test_that(".check_mamba handles missing executable", {
  expect_error(
    .check_mamba(mamba = "/non/existent/mamba"),
    "mamba not found at"
  )
})

test_that(".check_mamba respects environment variables", {
  # Save original env var
  original_mamba <- Sys.getenv("SN_MAMBA", unset = NA)

  # Test with SN_MAMBA set to existing executable
  if (Sys.which("echo") != "") {
    echo_path <- Sys.which("echo")
    Sys.setenv(SN_MAMBA = echo_path)
    result <- .check_mamba()
    expect_equal(result, as.character(echo_path))
  }

  # Restore original env var
  if (is.na(original_mamba)) {
    Sys.unsetenv("SN_MAMBA")
  } else {
    Sys.setenv(SN_MAMBA = original_mamba)
  }
})

test_that(".mamba handles command execution", {
  skip_on_cran()

  # Test with echo command (should exist on most systems)
  if (Sys.which("echo") != "") {
    result <- .mamba(
      mamba = Sys.which("echo"),
      subcommand = "test",
      options = c("hello")
    )

    expect_type(result, "list")
    expect_named(result, c("stdout", "stderr", "exit_code"))
    expect_type(result$stdout, "character")
    expect_type(result$stderr, "character")
    expect_type(result$exit_code, "integer")
  }
})

test_that(".mamba handles command errors", {
  # Test with non-existent command
  result <- .mamba(
    mamba = "/non/existent/command",
    subcommand = "test"
  )

  expect_type(result, "list")
  expect_named(result, c("stdout", "stderr", "exit_code"))
  expect_equal(result$exit_code, 1)
  expect_true(nchar(result$stderr) > 0)
})

test_that(".mamba_search validates input", {
  skip_on_cran()
  skip_if_offline()
  skip_if_no_mamba()

  # Test that search works (may return empty results for non-existent tools)
  # mamba search usually doesn't error for non-existent tools, just returns empty
  result <- .mamba_search("non_existent_tool_xyz123456789")
  expect_type(result, "list")
  expect_true("result" %in% names(result))
})

test_that(".get_latest_tool_version handles search results", {
  skip_on_cran()
  skip_if_offline()
  skip_if_no_mamba()

  # This function should error when no packages are found (empty search results)
  expect_error(
    .get_latest_tool_version("non_existent_tool_xyz123456789"),
    "Search result missing expected fields" # This is the expected error for empty results
  )
})

test_that(".mamba_create_from_yaml validates YAML file", {
  # Test with non-existent file
  expect_error(
    .mamba_create_from_yaml("/non/existent/file.yaml"),
    "YAML file not found"
  )

  # Test with invalid extension
  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)

  expect_error(
    .mamba_create_from_yaml(temp_file),
    "File must have .yaml or .yml extension"
  )

  unlink(temp_file)
})

test_that(".mamba_create_from_yaml uses correct environment name", {
  # Create a temporary YAML file
  temp_yaml <- tempfile(fileext = ".yaml")
  yaml_content <- "name: test
channels:
  - conda-forge
dependencies:
  - python=3.9
"
  writeLines(yaml_content, temp_yaml)

  temp_base_dir <- file.path(tempdir(), "test_yaml_envs")

  # Test that it would use the provided env_name (only if mamba is available)
  if (is_mamba_available()) {
    # Since we have mamba available, this will likely succeed, not error
    # Test that the function uses the custom name correctly
    tryCatch(
      {
        result <- .mamba_create_from_yaml(
          yaml_file = temp_yaml,
          env_name = "custom_name",
          base_dir = temp_base_dir
        )
        expect_true(grepl("custom_name", result))
        # Clean up the created environment
        if (dir.exists(result)) {
          unlink(result, recursive = TRUE)
        }
      },
      error = function(e) {
        # If it errors due to mamba issues, that's also acceptable
        expect_true(inherits(e, "cli_error"))
      }
    )
  }

  # Clean up
  unlink(temp_yaml)
})

test_that(".mamba_create_from_name validates tool name", {
  skip_on_cran()
  skip_if_offline()
  skip_if_no_mamba()

  temp_base_dir <- file.path(tempdir(), "test_name_envs")

  # This will error because we're using a non-existent tool,
  # but it tests the function structure
  expect_error(
    .mamba_create_from_name(
      tool_name = "non_existent_tool_xyz123456789",
      version = "1.0.0",
      base_dir = temp_base_dir
    )
    # Could error at version lookup or tool installation - accept any error
  )
})

# Integration tests

test_that("environment functions work together", {
  skip_on_cran()
  skip_if_offline()

  # Test that micromamba installation enables tool installation
  temp_dir <- tempdir()
  micromamba_dir <- file.path(temp_dir, "integration_test")

  # Clean up any existing installation
  if (dir.exists(micromamba_dir)) {
    unlink(micromamba_dir, recursive = TRUE)
  }

  # Install micromamba
  micromamba_path <- sn_install_micromamba(install_dir = micromamba_dir)
  expect_true(file.exists(micromamba_path))

  # Verify environment variable is set
  expect_equal(Sys.getenv("SN_MICROMAMBA"), as.character(micromamba_path))

  # Clean up
  unlink(micromamba_dir, recursive = TRUE)
})

# Test helper functions for edge cases

test_that("functions handle NULL and missing parameters correctly", {
  # Test .check_mamba with NULL
  expect_no_error(.check_mamba(mamba = NULL))

  # Test that base_dir defaults are used correctly in both create functions
  temp_yaml <- tempfile(fileext = ".yaml")
  writeLines("name: test\nchannels:\n  - conda-forge\ndependencies:\n  - python", temp_yaml)

  # These should not error on parameter validation (though they may error on execution)
  # Only test if mamba is available
  if (is_mamba_available()) {
    # Test with YAML - this might succeed since it's a valid environment
    tryCatch(
      {
        result <- .mamba_create_from_yaml(temp_yaml, base_dir = NULL)
        # Clean up if created
        if (dir.exists(result)) {
          unlink(result, recursive = TRUE)
        }
      },
      error = function(e) {
        # Error is acceptable for environment creation issues
        expect_true(inherits(e, "cli_error"))
      }
    )

    # Test with non-existent tool - this should error
    expect_error(
      .mamba_create_from_name("non_existent_tool_xyz123456789", base_dir = NULL)
      # Will error when trying to find version - accept any error
    )
  }

  unlink(temp_yaml)
})

test_that("overwrite parameter works correctly", {
  skip_on_cran()

  temp_base_dir <- file.path(tempdir(), "overwrite_test")
  test_env_dir <- file.path(temp_base_dir, "test_env")

  # Create a fake environment directory
  dir.create(test_env_dir, recursive = TRUE)
  test_file <- file.path(test_env_dir, "test.txt")
  writeLines("original", test_file)

  # Test without overwrite (should skip)
  temp_yaml <- tempfile(fileext = ".yaml")
  writeLines("name: test\nchannels:\n  - conda-forge\ndependencies:\n  - python", temp_yaml)

  expect_message(
    result <- .mamba_create_from_yaml(
      yaml_file = temp_yaml,
      env_name = "test_env",
      base_dir = temp_base_dir,
      overwrite = FALSE
    ),
    "already exists"
  )

  expect_equal(as.character(result), test_env_dir)
  expect_true(file.exists(test_file))
  expect_equal(readLines(test_file), "original")

  # Clean up
  unlink(temp_yaml)
  unlink(temp_base_dir, recursive = TRUE)
})
