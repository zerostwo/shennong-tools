# Test helper functions

#' Skip test if offline (no internet connection)
skip_if_offline <- function() {
  # Try to connect to a reliable host
  tryCatch(
    {
      # Test connection to Google DNS
      con <- url("http://www.google.com", open = "rb")
      close(con)
    },
    error = function(e) {
      testthat::skip("Offline - no internet connection")
    }
  )
}

#' Skip test if mamba/micromamba is not available
skip_if_no_mamba <- function() {
  # Check if mamba or micromamba is available
  mamba_available <- Sys.which("mamba") != "" ||
    Sys.which("micromamba") != "" ||
    file.exists(path.expand("~/.local/bin/micromamba"))

  if (!mamba_available) {
    testthat::skip("mamba/micromamba not available")
  }
}

#' Create a temporary directory for testing that gets cleaned up
with_temp_dir <- function(code) {
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)

  withr::with_dir(temp_dir, {
    withr::defer(unlink(temp_dir, recursive = TRUE))
    code
  })
}

#' Mock a successful mamba command result
mock_mamba_success <- function(stdout = "", stderr = "") {
  list(
    stdout = stdout,
    stderr = stderr,
    exit_code = 0
  )
}

#' Mock a failed mamba command result
mock_mamba_failure <- function(stderr = "Command failed") {
  list(
    stdout = "",
    stderr = stderr,
    exit_code = 1
  )
}
