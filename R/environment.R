#' Install micromamba
#'
#' This function installs [micromamba](https://mamba.readthedocs.io/en/latest/user_guide/micromamba.html),
#' a lightweight `conda`-compatible package manager, into a user-defined directory. It automatically detects the
#' operating system and architecture, downloads the appropriate binary, and extracts it into the specified location.
#'
#' If `micromamba` is already installed at the target location, and `overwrite = FALSE`, the existing binary is reused.
#'
#' The function sets the `SN_MICROMAMBA` environment variable to the installed binary path, so that other ShennongTools
#' functions can access it automatically.
#'
#' @param install_dir Character. The directory where `micromamba` should be installed.
#'        Default is `~/.local/bin`.
#' @param overwrite Logical. If `TRUE`, forces reinstallation even if `micromamba` is already installed. Default is `FALSE`.
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
#' sn_install_micromamba(overwrite = TRUE)
#'
#' # Install a specific version
#' sn_install_micromamba(version = "1.5.6")
#' }
#'
#' @export
sn_install_micromamba <- function(
    install_dir = "~/.local/bin",
    version = "latest",
    overwrite = FALSE) {
  install_dir <- path_expand(install_dir)
  micromamba_bin <- path(install_dir, "micromamba")

  if (!overwrite && file_exists(micromamba_bin)) {
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

#' Install a tool or environment
#'
#' Install a bioinformatics tool using mamba/micromamba. Can install either:
#' - A single tool by name and version
#' - A complete environment from YAML file
#'
#' @param tool_name Character. Tool name (used for environment naming and tool installation).
#' @param version Character. Version to install (NULL for latest). Ignored if yaml is provided.
#' @param base_dir Character. Base directory for installations.
#' @param channel Character. Conda channel to use. Ignored if yaml is provided.
#' @param yaml Character. Path to YAML environment file (optional). If provided, installs
#'        the environment to base_dir/tool_name.
#' @param overwrite Logical. Force reinstallation.
#' @param mamba Character. Path to mamba/micromamba executable.
#' @return Character. Path to the installed environment.
#'
#' @examples
#' \dontrun{
#' # Install a single tool
#' sn_install_tool("fastqc")
#'
#' # Install with specific version
#' sn_install_tool("samtools", version = "1.16")
#'
#' # Install from YAML file to custom environment name
#' sn_install_tool("bioinformatics", yaml = "path/to/environment.yaml")
#' }
#'
#' @export
sn_install_tool <- function(tool_name,
                            version = NULL,
                            base_dir = NULL,
                            channel = "bioconda",
                            yaml = NULL,
                            overwrite = FALSE,
                            mamba = NULL) {

  # Validate tool_name
  if (!is.character(tool_name) || length(tool_name) != 1 || nchar(tool_name) == 0) {
    cli_abort("tool_name must be a non-empty character string")
  }

  # Check if yaml file is provided
  if (!is.null(yaml)) {
    return(.mamba_create_from_yaml(
      yaml_file = yaml,
      env_name = tool_name,
      base_dir = base_dir,
      mamba = mamba,
      overwrite = overwrite
    ))
  }

  # Otherwise, install single tool by name
  return(.mamba_create_from_name(
    tool_name = tool_name,
    version = version,
    base_dir = base_dir,
    mamba = mamba,
    channel = channel,
    overwrite = overwrite
  ))
}

# Internal ------------------------------------------------------------
#' @keywords internal
.check_mamba <- function(mamba = NULL) {
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

#' @keywords internal
.get_latest_tool_version <- function(tool, mamba = NULL, channel = "bioconda") {
  results <- .mamba_search(tool, channel = channel)
  pkgs <- results$result$pkgs

  if (!"version" %in% names(pkgs) || !"timestamp" %in% names(pkgs)) {
    cli_abort("Search result missing expected fields.")
  }

  pkgs <- pkgs[order(pkgs$timestamp, decreasing = TRUE), ]
  pkgs <- pkgs[pkgs$build_number == 0, ]
  latest_version <- pkgs$version[1]
  return(latest_version)
}

#' @keywords internal
.mamba <- function(mamba = NULL, subcommand, options = character()) {
  mamba <- mamba %||% .check_mamba()
  tryCatch(
    {
      # Use run for better error handling and output capture
      result <- run(
        command = mamba,
        args = c(subcommand, options)
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

#' @keywords internal
.mamba_search <- function(tool, channel = "bioconda") {
  result <- .mamba(
    subcommand = "search",
    options = c(tool, "-c", channel, "--json")
  )

  if (result$exit_code != 0) {
    cli_abort("Failed to search for tool '{tool}': {result$stderr}")
  }

  return(jsonlite::fromJSON(result$stdout))
}

#' @keywords internal
.mamba_create_from_yaml <- function(
    yaml_file,
    env_name = NULL,
    base_dir = NULL,
    mamba = NULL,
    overwrite = FALSE) {
  # Validate YAML file exists
  if (!file_exists(yaml_file)) {
    cli_abort("YAML file not found: {yaml_file}")
  }

  if (!grepl("\\.(yaml|yml)$", yaml_file, ignore.case = TRUE)) {
    cli_abort("File must have .yaml or .yml extension: {yaml_file}")
  }

  mamba <- mamba %||% .check_mamba()
  base_dir <- base_dir %||% tools::R_user_dir(
    package = "shennong-tools", which = "data"
  )

  # Use provided env_name or default to yaml filename
  if (is.null(env_name)) {
    env_name <- tools::file_path_sans_ext(basename(yaml_file))
  }

  # Expand base path and env path
  base_dir <- path_expand(base_dir)
  env_path <- path(base_dir, env_name)

  # Skip if already installed
  if (!overwrite && dir_exists(env_path)) {
    cli_alert_success("Environment '{env_name}' already exists at {env_path}")
    return(env_path)
  }

  # Remove existing environment if overwrite is TRUE
  if (overwrite && dir_exists(env_path)) {
    cli_alert_info("Removing existing environment at {env_path}")
    unlink(env_path, recursive = TRUE)
  }

  # Create environment from YAML file
  cli_alert_info("Creating environment '{env_name}' from {yaml_file}")
  result <- .mamba(
    subcommand = "env",
    options = c(
      "create",
      "-y",
      "-p", env_path,
      "-f", yaml_file
    )
  )

  if (result$exit_code != 0) {
    cli_abort("Failed to create environment from YAML '{yaml_file}': {result$stderr}")
  }

  # Verify environment was created
  if (!dir_exists(env_path)) {
    cli_abort("Environment creation failed at {env_path}")
  }

  cli_alert_success("Environment '{env_name}' created successfully at {env_path}")
  return(env_path)
}

#' @keywords internal
.mamba_create_from_name <- function(
    tool_name,
    version = NULL,
    base_dir = NULL,
    mamba = NULL,
    channel = "bioconda",
    overwrite = FALSE) {

  base_dir <- base_dir %||% tools::R_user_dir(
    package = "shennong-tools", which = "data"
  )

  # Use tool_name for version lookup
  version <- version %||% .get_latest_tool_version(tool_name, mamba, channel)

  # Expand base path and env path
  base_dir <- path_expand(base_dir)
  env_path <- path(base_dir, tool_name, version)

  # Skip if already installed
  if (!overwrite && dir_exists(env_path)) {
    cli_alert_success("{tool_name} {version} already installed at {env_path}")
    return(env_path)
  }

  # Build command to install
  cli_alert_info("Installing {tool_name}={version} into {env_path}")
  result <- .mamba(
    subcommand = "create",
    options = c(
      "-y",
      "-p", env_path,
      "-c", "conda-forge",
      if (channel != "conda-forge") paste0("-c ", channel),
      paste0(tool_name, "=", version)
    )
  )

  if (result$exit_code != 0) {
    cli_abort("Failed to create environment for '{tool_name}': {result$stderr}")
  }

  # Basic verification - check if environment directory exists and has bin folder
  bin_dir <- path(env_path, "bin")
  if (!dir_exists(bin_dir)) {
    cli_alert_warning("Binary directory not found at {bin_dir}")
  } else {
    # Check for any executable files in bin directory
    bin_files <- list.files(bin_dir, full.names = FALSE)
    if (length(bin_files) > 0) {
      cli_alert_success("Environment created with {length(bin_files)} executable(s)")
    } else {
      cli_alert_warning("No executables found in {bin_dir}")
    }
  }

  cli_alert_success("{tool_name} installed successfully at {env_path}")
  return(env_path)
}
