#' Install micromamba
#'
#' This function installs [micromamba](https://mamba.readthedocs.io/en/latest/user_guide/micromamba.html),
#' a lightweight `conda`-compatible package manager, into a user-defined directory. It automatically detects the
#' operating system and architecture, downloads the appropriate binary, and extracts it into the specified location.
#' 
#' If `micromamba` is already installed at the target location, and `force = FALSE`, the existing binary is reused.
#' 
#' The function sets the `SN_MICROMAMBA` environment variable to the installed binary path, so that other ShennongTools
#' functions can access it automatically.
#'
#' @param install_dir Character. The directory where `micromamba` should be installed.
#'        Default is `~/.local/bin`.
#' @param force Logical. If `TRUE`, forces reinstallation even if `micromamba` is already installed. Default is `FALSE`.
#' @param version Character. The version of micromamba to install.
#'        Use `"latest"` (default) for the latest stable version.
#'
#' @return A character string: the full path to the installed `micromamba` binary.
#'
#' @examples
#' \dontrun{
#' # Install micromamba to the default location
#' sn_install_micromamba()
#'
#' # Force reinstall micromamba
#' sn_install_micromamba(force = TRUE)
#'
#' # Install a specific version
#' sn_install_micromamba(version = "1.5.6")
#' }
#'
#' @export
sn_install_micromamba <- function(
    install_dir = "~/.local/bin",
    force = FALSE, version = "latest") {
  install_dir <- path_expand(install_dir)
  micromamba_bin <- path(install_dir, "micromamba")

  if (!force && file_exists(micromamba_bin)) {
    Sys.setenv(SN_MICROMAMBA = micromamba_bin)
    return(micromamba_bin)
  }

  dir_create(install_dir, recurse = TRUE)

  # Determine platform
  os <- Sys.info()[["sysname"]]
  arch <- Sys.info()[["machine"]]

  platform <- switch(os,
    "Linux" = "linux",
    "Darwin" = "osx",
    cli_abort("Unsupported OS: {os}")
  )

  arch_tag <- switch(arch,
    "x86_64" = "64",
    "aarch64" = "aarch64",
    "arm64" = "arm64",
    cli_abort("Unsupported arch: {arch}")
  )

  url <- glue(
    "https://micro.mamba.pm/api/micromamba/{platform}-{arch_tag}/{version}"
  )

  cli_alert_info("Downloading micromamba from {url}")

  # Download and extract micromamba binary using processx
  tryCatch(
    {
      # Download the file
      temp_file <- tempfile(fileext = ".tar.bz2")
      run("curl", args = c("-Ls", url, "-o", temp_file))

      # Extract micromamba binary
      run("tar", args = c(
        "-xvj", "-C", install_dir, "-f", temp_file,
        "bin/micromamba", "--strip-components=1"
      ))

      # Clean up temp file
      unlink(temp_file)
    },
    error = function(e) {
      cli_abort("Failed to download and extract micromamba: {e$message}")
    }
  )

  # Verify install
  if (!file_exists(micromamba_bin)) {
    cli_abort("micromamba installation failed at {micromamba_bin}")
  }

  file_chmod(micromamba_bin, "u+x")
  Sys.setenv(SN_MICROMAMBA = micromamba_bin)
  cli_alert_success("micromamba installed at {micromamba_bin}")
  return(micromamba_bin)
}

sn_check_mamba <- function(mamba = NULL) {
  # Find mamba/micromamba binary
  if (is.null(mamba)) {
    # First check SN_MAMBA env var
    mamba <- Sys.getenv("SN_MAMBA", unset = "")
    if (mamba == "") {
      # Then check system PATH for mamba
      mamba_in_path <- Sys.which("mamba")
      if (mamba_in_path != "" && mamba_in_path != "mamba") {
        # Found mamba in PATH with full path
        mamba <- mamba_in_path
      } else {
        # Check for micromamba
        micromamba_in_path <- Sys.which("micromamba")
        if (micromamba_in_path != "" && micromamba_in_path != "micromamba") {
          mamba <- micromamba_in_path
        } else {
          # Check our installed micromamba
          micromamba_bin <- path_expand("~/.local/bin/micromamba")
          if (file_exists(micromamba_bin)) {
            mamba <- micromamba_bin
          } else {
            # If not found, install micromamba
            cli_alert_info("mamba not found, installing micromamba...")
            mamba <- sn_install_micromamba()
          }
        }
      }
    }
  }

  if (!file_exists(mamba)) {
    cli_abort("mamba not found at {mamba}, please install it first")
  }
  return(mamba)
}

#' Install a tool
#'
#' Install a bioinformatics tool using mamba/micromamba.
#'
#' @param tool Character. Tool name or Tool object.
#' @param package_name Character. Package name for installation (optional, used if tool is a string).
#' @param binary_name Character. Binary name to check after installation (optional).
#' @param version Character. Version to install (NULL for latest).
#' @param base_dir Character. Base directory for installations.
#' @param mamba Character. Path to mamba/micromamba executable.
#' @param channel Character. Conda channel to use.
#' @param force Logical. Force reinstallation.
#' @return Character. Path to the installed environment.
#' @export
sn_install_tool <- function(tool,
                            package_name = NULL,
                            binary_name = NULL,
                            version = NULL,
                            base_dir = NULL,
                            mamba = NULL,
                            channel = "bioconda",
                            force = FALSE) {
  # Handle Tool object vs tool name
  if (inherits(tool, "Tool")) {
    tool_name <- tool@name
    package_name <- package_name %||% tool@package_name
    binary_name <- binary_name %||% tool@binary_name
    channel <- tool@channel
    version <- version %||% tool@version
  } else {
    tool_name <- tool
    package_name <- package_name %||% tool_name
    binary_name <- binary_name %||% tool_name
  }

  mamba <- mamba %||% sn_check_mamba(mamba = mamba)
  base_dir <- base_dir %||% tools::R_user_dir(
    package = "shennong-tools", which = "data"
  )

  # Use package_name for version lookup
  version <- version %||% .get_latest_tool_version(package_name, mamba, channel)

  # Expand base path and env path - use tool_name for directory naming
  base_dir <- path_expand(base_dir)
  env_path <- path(base_dir, tool_name, version)

  # Skip if already installed
  if (!force && dir_exists(env_path)) {
    cli_alert_success("{tool_name} {version} already installed at {env_path}")
    return(env_path)
  }

  # Build command to install - use package_name for installation
  cli_alert_info("Installing {package_name}={version} into {env_path}")
  .sn_run_command(
    command = mamba,
    args = c(
      "create",
      "-y",
      "-p", env_path,
      "-c", "conda-forge",
      if (channel != "conda-forge") paste0("-c ", channel),
      paste0(package_name, "=", version)
    )
  )

  # Check installation - use binary_name for verification
  binary_path <- path(env_path, "bin", binary_name)
  if (!file_exists(binary_path)) {
    cli_alert_warning("{binary_name} binary not found at {binary_path}")

    # Try alternative locations
    alt_paths <- c(
      path(env_path, "bin", tool_name),
      path(env_path, "bin", package_name)
    )

    found_binary <- FALSE
    for (alt_path in alt_paths) {
      if (file_exists(alt_path)) {
        cli_alert_info("Found binary at alternative location: {alt_path}")
        found_binary <- TRUE
        break
      }
    }

    if (!found_binary) {
      cli_alert_warning("Could not find any binary for {tool_name} in {env_path}/bin/")
    }
  } else {
    cli_alert_success("Binary {binary_name} found at {binary_path}")
  }

  cli_alert_success("{tool_name} installed successfully at {env_path}")
  return(env_path)
}

.get_latest_tool_version <- function(tool, mamba = NULL, channel = "bioconda") {
  if (is.null(mamba)) {
    mamba <- Sys.getenv(
      x = "SN_MAMBA",
      unset = path_expand("~/.local/bin/micromamba")
    )
  }
  if (!file_exists(mamba)) {
    cli_abort("mamba not found at {mamba}")
  }

  result <- .sn_run_command(
    mamba,
    args = c("search", tool, "-c", channel, "--json")
  )
  json <- jsonlite::fromJSON(result$stdout)

  pkgs <- json$result$pkgs
  if (!"version" %in% names(pkgs) || !"timestamp" %in% names(pkgs)) {
    cli_abort("Search result missing expected fields.")
  }

  pkgs <- pkgs[order(pkgs$timestamp, decreasing = TRUE), ]
  pkgs <- pkgs[pkgs$build_number == 0, ]
  latest_version <- pkgs$version[1]
  return(latest_version)
}

#' Internal Command Runner
#'
#' Internal function to run system commands and capture output.
#'
#' @param command Character. Command to run.
#' @param args Character vector. Command arguments.
#'
#' @return List with stdout, stderr, exit_code.
#' @keywords internal
.sn_run_command <- function(command, args = character()) {
  tryCatch(
    {
      # Use run for better error handling and output capture
      result <- run(
        command = command,
        args = args,
        stdout = TRUE,
        stderr = TRUE,
        error_on_status = FALSE
      )

      return(list(
        stdout = result$stdout,
        stderr = result$stderr,
        exit_code = result$status
      ))
    },
    error = function(e) {
      return(list(
        stdout = "",
        stderr = as.character(e),
        exit_code = 1
      ))
    }
  )
}

#' Get or Set ShennongTools Options
#'
#' This function provides a convenient interface to get or set global options
#' for ShennongTools.
#'
#' @param log_level Character or Integer. Logging level: "silent"/"quiet"/0,
#'                                       "minimal"/1, "normal"/2.
#' @param log_dir Character. Directory to save log files. NULL for smart detection.
#' @param ... Additional options to set.
#'
#' @return If no arguments provided, returns current options. Otherwise invisibly returns the previous values.
#' @export
#'
#' @examples
#' \dontrun{
#' # View current options
#' sn_options()
#'
#' # Set options
#' sn_options(log_level = "minimal", log_dir = "~/my_logs")
#' sn_options(log_dir = "~/analysis_logs")
#'
#' # Use numeric log levels
#' sn_options(log_level = 1) # minimal
#'
#' # Reset to defaults
#' sn_options(log_level = "normal", log_dir = NULL)
#' }
sn_options <- function(log_level = NULL, log_dir = NULL, ...) {
  # Current options
  current_opts <- list(
    log_level = getOption("sn.log_level"),
    log_dir = getOption("sn.log_dir")
  )

  # If no arguments provided, return current options
  if (missing(log_level) && missing(log_dir) && length(list(...)) == 0) {
    return(current_opts)
  }

  # Prepare new options
  new_opts <- list()

  if (!is.null(log_level)) {
    # Validate log_level
    tryCatch(
      {
        .sn_normalize_log_level(log_level)
        new_opts$sn.log_level <- log_level
      },
      error = function(e) {
        cli_abort("Invalid log_level: {e$message}")
      }
    )
  }

  if (!is.null(log_dir)) {
    if (!is.character(log_dir)) {
      cli_abort("log_dir must be NULL or a character string")
    }
    new_opts$sn.log_dir <- log_dir
  }

  # Handle additional options
  extra_opts <- list(...)
  if (length(extra_opts) > 0) {
    # Add sn. prefix to option names
    names(extra_opts) <- paste0("sn.", names(extra_opts))
    new_opts <- c(new_opts, extra_opts)
  }

  # Set new options
  if (length(new_opts) > 0) {
    options(new_opts)
    cli_alert_success("Updated ShennongTools options")
  }

  invisible(current_opts)
}

#' Validate Tool Environment
#'
#' Check if a tool is properly installed and available.
#'
#' @param tool Character. Tool name.
#' @param registry List. Tool registry (optional).
#' @param env_path Character. Environment path (optional).
#'
#' @return Logical. TRUE if tool is available, FALSE otherwise.
#' @export
sn_validate_tool <- function(tool, registry = NULL, env_path = NULL) {
  if (is.null(registry)) {
    registry <- sn_load_registry()
  }

  tool_config <- sn_get_tool_config(tool, registry)

  if (is.null(env_path)) {
    env_path <- .sn_find_tool_env(tool, tool_config)
  }

  if (is.null(env_path)) {
    cli_alert_danger("Tool '{tool}' environment not found")
    return(FALSE)
  }

  # Check if environment directory exists
  if (!dir_exists(env_path)) {
    cli_alert_danger("Environment path does not exist: {env_path}")
    return(FALSE)
  }

  # Check if binaries are available
  all_binaries_found <- TRUE
  for (cmd_name in names(tool_config$commands)) {
    cmd_config <- tool_config$commands[[cmd_name]]
    binary_path <- path(env_path, "bin", cmd_config$binary)

    if (!file_exists(binary_path)) {
      cli_alert_warning("Binary '{cmd_config$binary}' not found for command '{cmd_name}'")
      all_binaries_found <- FALSE
    }
  }

  if (all_binaries_found) {
    cli_alert_success("Tool '{tool}' is properly installed at {env_path}")
    return(TRUE)
  } else {
    cli_alert_danger("Tool '{tool}' installation incomplete")
    return(FALSE)
  }
}

#' Initialize Package
#'
#' Initialize the ShennongTools package by checking dependencies and
#' loading the tool registry.
#'
#' @param check_mamba Logical. Whether to check mamba/micromamba installation.
#'
#' @return List with initialization status.
#' @export
sn_initialize <- function(check_mamba = TRUE) {
  cli_h1("Initializing ShennongTools")

  # Load registry
  cli_alert_info("Loading tool registry...")
  registry <- sn_load_registry()

  # Check mamba if requested
  mamba_status <- if (check_mamba) {
    tryCatch(
      {
        mamba_path <- sn_check_mamba()
        cli_alert_success("Mamba/micromamba found at: {mamba_path}")
        TRUE
      },
      error = function(e) {
        cli_alert_warning("Mamba/micromamba not found: {e$message}")
        FALSE
      }
    )
  } else {
    NULL
  }

  cli_alert_success("Package initialized successfully!")
  cli_alert_info("Use sn_list_available_tools() to see available tools")
  cli_alert_info("Use sn_help('tool_name') for tool help")

  invisible(list(
    registry = registry,
    mamba_available = mamba_status,
    tools_count = length(registry)
  ))
}

#' Format File Size
#'
#' @param bytes Numeric. File size in bytes.
#'
#' @return Character. Formatted file size.
#' @keywords internal
.sn_format_file_size <- function(bytes) {
  if (is.na(bytes) || bytes == 0) {
    return("0 B")
  }

  units <- c("B", "KB", "MB", "GB", "TB")
  i <- 1
  while (bytes >= 1024 && i < length(units)) {
    bytes <- bytes / 1024
    i <- i + 1
  }

  return(sprintf("%.1f %s", bytes, units[i]))
}

#' Format Memory Size
#'
#' @param bytes Numeric. Memory size in bytes.
#'
#' @return Character. Formatted memory size.
#' @keywords internal
.sn_format_memory <- function(bytes) {
  if (is.na(bytes) || bytes == 0) {
    return("0 B")
  }

  units <- c("B", "KB", "MB", "GB", "TB")
  i <- 1
  while (bytes >= 1024 && i < length(units)) {
    bytes <- bytes / 1024
    i <- i + 1
  }

  return(sprintf("%.1f %s", bytes, units[i]))
}

#' Format Duration
#'
#' @param seconds Numeric. Duration in seconds.
#'
#' @return Character. Formatted duration string.
#' @keywords internal
.sn_format_duration <- function(seconds) {
  if (is.na(seconds)) {
    return("unknown")
  }

  if (seconds < 1) {
    return(sprintf("%.0f ms", seconds * 1000))
  } else if (seconds < 60) {
    return(sprintf("%.1f s", seconds))
  } else if (seconds < 3600) {
    minutes <- floor(seconds / 60)
    secs <- seconds %% 60
    return(sprintf("%d min %.1f s", minutes, secs))
  } else {
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds %% 3600) / 60)
    secs <- seconds %% 60
    return(sprintf("%d h %d min %.1f s", hours, minutes, secs))
  }
}