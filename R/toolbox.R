#' Initialize a Toolbox
#'
#' Creates a new Toolbox object with empty tool list and valid mamba path.
#' This is the starting point for managing bioinformatics tools.
#'
#' @param base_dir Character. Base directory for tool installations.
#'   Installation paths will be \\{base_dir\\}/\\{tool_name\\}/\\{version\\}.
#' @param mamba_path Character. Path to mamba/micromamba executable.
#'   If NULL, will try to find automatically.
#'
#' @return Toolbox object.
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' toolbox <- sn_initialize_toolbox("/opt/biotools")
#' }
sn_initialize_toolbox <- function(base_dir = NULL, mamba_path = NULL) {
  base_dir <- base_dir %||% R_user_dir(
    package = "shennong-tools", which = "data"
  )

  mamba_path <- mamba_path %||% .check_mamba()

  toolbox <- new("Toolbox",
    base_dir = base_dir,
    mamba_path = mamba_path,
    tools = list()
  )

  cli_alert_success("Initialized toolbox with base directory: {base_dir}")
  cli_alert_info("Using mamba: {mamba_path}")

  return(toolbox)
}

#' Add Tool to Toolbox
#'
#' Adds a tool to the toolbox. Can add from built-in registry using tool_name,
#' or from custom YAML file.
#'
#' @param toolbox Toolbox object.
#' @param tool_name Character. Name of the tool (for built-in tools).
#' @param version Character. Version of the tool. If NULL, uses latest for built-in tools.
#' @param yaml Character. Path to custom YAML file (alternative to tool_name).
#' @param install Logical. Whether to install the tool immediately.
#' @param ... Additional arguments (reserved for future use).
#'
#' @return Updated Toolbox object.
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' # Add built-in tool
#' toolbox <- sn_add_tool(toolbox, tool_name = "samtools", version = "1.22")
#'
#' # Add from custom YAML
#' toolbox <- sn_add_tool(toolbox, yaml = "my_tool.yaml")
#' }
sn_add_tool <- function(
    toolbox,
    tool_name = NULL,
    version = NULL,
    yaml = NULL,
    install = FALSE, ...) {
  if (!inherits(toolbox, "Toolbox")) {
    cli_abort("First argument must be a Toolbox object")
  }

  # Determine if adding from built-in or custom YAML
  if (!is.null(tool_name) && !is.null(yaml)) {
    cli_abort("Provide either tool_name or yaml, not both")
  }

  if (is.null(tool_name) && is.null(yaml)) {
    cli_abort("Must provide either tool_name or yaml")
  }

  # Load tool configuration
  if (is.null(tool_name)) {
    # Load from custom YAML
    tool_config <- .load_tool_from_yaml(yaml)
    tool_name <- tool_config$tool_name
    if (is.null(version)) {
      version <- .extract_version_from_config(tool_config)
    }
  } else {
    # Load from built-in registry
    tool_config <- .load_builtin_tool(tool_name, version)
    version <- .resolve_latest_version(tool_config)
  }

  # Get existing tool or create new one
  existing_tool <- if (tool_name %in% sn_get_toolbox_tools(toolbox)) {
    toolbox@tools[[tool_name]]
  } else {
    NULL
  }

  # Create or update Tool object
  tool <- .create_tool_from_config(tool_config, version, toolbox@base_dir, existing_tool)

  # Add to toolbox (replaces any existing tool object)
  toolbox@tools[[tool_name]] <- tool

  cli_alert_success("Added {tool_name} version {version} to toolbox")

  # Install if requested
  if (install) {
    install_success <- .install_tool(toolbox, tool_name, version, tool_config)
    if (install_success) {
      # Update the tool's installed status - need to refresh the tool object
      tool <- .create_tool_from_config(tool_config, version, toolbox@base_dir, tool)
      toolbox@tools[[tool_name]] <- tool
    } else {
      cli_alert_warning("Installation failed for {tool_name} version {version}")
    }
  }

  # Auto-save user toolbox if modified
  if (.should_save_toolbox(toolbox)) {
    .save_user_toolbox(toolbox)
  }

  return(toolbox)
}

#' Remove Tool from Toolbox
#'
#' Removes a tool (and optionally uninstalls it) from the toolbox.
#'
#' @param toolbox Toolbox object.
#' @param tool_name Character. Name of the tool to remove.
#' @param version Character. Version to remove. If NULL, removes all versions.
#' @param uninstall Logical. Whether to uninstall before removing.
#'
#' @return Updated Toolbox object.
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove specific version
#' toolbox <- sn_remove_tool(toolbox, "samtools", "1.22")
#'
#' # Remove all versions
#' toolbox <- sn_remove_tool(toolbox, "samtools")
#' }
sn_remove_tool <- function(toolbox, tool_name, version = NULL, uninstall = TRUE) {
  if (!inherits(toolbox, "Toolbox")) {
    cli_abort("First argument must be a Toolbox object")
  }

  if (!tool_name %in% names(toolbox@tools)) {
    cli_alert_warning("Tool {tool_name} not found in toolbox")
    return(toolbox)
  }

  tool <- toolbox@tools[[tool_name]]

  # Remove specific version or all versions
  if (!is.null(version)) {
    if (!version %in% sn_get_tool_versions(tool)) {
      cli_alert_warning("Tool {tool_name} version {version} not found")
      return(toolbox)
    }

    # Uninstall if requested
    if (uninstall && .is_installed(tool, version)) {
      success <- .uninstall_tool(toolbox, tool_name, version)
    }

    # Remove version from tool
    tool@versions <- setdiff(tool@versions, version)
    tool@install_dates[[version]] <- NULL

    # Update default version if necessary
    if (tool@default_version == version && length(tool@versions) > 0) {
      tool@default_version <- tool@versions[1]
    } else if (length(tool@versions) == 0) {
      tool@default_version <- character(0)
    }

    # If no versions left, remove tool entirely
    if (length(tool@versions) == 0) {
      toolbox@tools[[tool_name]] <- NULL
    } else {
      toolbox@tools[[tool_name]] <- tool
    }

    cli_alert_success("Removed {tool_name} version {version}")
  } else {
    # Remove all versions
    versions <- sn_get_installed_versions(tool)

    if (uninstall) {
      for (ver in versions) {
        success <- .uninstall_tool(toolbox, tool_name, ver)
      }
    }

    toolbox@tools[[tool_name]] <- NULL
    cli_alert_success("Removed all versions of {tool_name}")
  }

  # Auto-save user toolbox if modified
  if (.should_save_toolbox(toolbox)) {
    .save_user_toolbox(toolbox)
  }

  return(toolbox)
}

# Internal helper functions ---------------------------------------------------
#' Load Built-in Tool Configuration
#' @keywords internal
.load_builtin_tool <- function(tool_name, version) {
  tools_dir <- system.file("tools", package = "ShennongTools")
  if (tools_dir == "") {
    tools_dir <- file.path("inst", "tools")
  }

  yaml_file <- file.path(tools_dir, paste0(tool_name, ".yaml"))

  if (!file.exists(yaml_file)) {
    cli_abort("Built-in tool {tool_name} not found")
  }

  config <- yaml.load_file(yaml_file)

  # # Handle version resolution for different environment types
  if (is.null(version)) {
    version <- .resolve_latest_version(config)
  }

  # Fix dependencies array processing - ifelse breaks array structure
  if (!is.null(config$environment$dependencies)) {
    # Process dependencies correctly to maintain array structure
    for (i in seq_along(config$environment$dependencies)) {
      dep <- config$environment$dependencies[[i]]
      if (is.character(dep) && dep == tool_name) {
        config$environment$dependencies[[i]] <- paste0(tool_name, "=", version)
      }
    }
  }

  return(config)
}

#' Load Tool from Custom YAML
#' @keywords internal
.load_tool_from_yaml <- function(yaml_path) {
  if (!file.exists(yaml_path)) {
    cli_abort("YAML file not found: {yaml_path}")
  }

  config <- yaml.load_file(yaml_path)

  # Validate required fields
  required_fields <- c("tool_name", "description", "environment", "commands")
  missing_fields <- setdiff(required_fields, names(config))

  if (length(missing_fields) > 0) {
    cli_abort("Missing required fields in YAML: {paste(missing_fields, collapse = ', ')}")
  }

  return(config)
}

#' Extract Version from Tool Configuration
#' @keywords internal
.extract_version_from_config <- function(config) {
  env <- config$environment

  if (is.null(env$dependencies)) {
    return("dev")
  }

  # Look for version in main dependencies
  for (dep in env$dependencies) {
    if (is.character(dep) && grepl("=", dep)) {
      parts <- strsplit(dep, "\\s*=\\s*")[[1]]
      if (length(parts) >= 2 && parts[1] == config$tool_name) {
        return(parts[2])
      }
    }
  }

  # Check pip dependencies
  if ("pip" %in% env$dependencies) {
    pip_deps <- env$dependencies[["pip"]]
    if (is.list(pip_deps)) {
      for (dep in pip_deps) {
        if (is.character(dep) && grepl(config$tool_name, dep)) {
          if (grepl("git\\+", dep)) {
            return("dev")
          }
          # Try to extract version from pip dependency
          if (grepl("==", dep)) {
            parts <- strsplit(dep, "==")[[1]]
            if (length(parts) >= 2) {
              return(parts[2])
            }
          }
        }
      }
    }
  }

  # If no version found, get latest from PyPI if it's a Python package
  if ("python" %in% sapply(env$dependencies, function(x) {
    if (is.character(x)) strsplit(x, "\\s*=")[[1]][1] else ""
  })) {
    tryCatch(
      {
        return(.get_latest_version_from_pypi(config$tool_name))
      },
      error = function(e) {
        return("latest")
      }
    )
  }

  return("latest")
}

#' Resolve Latest Version for Tool
#' @keywords internal
.resolve_latest_version <- function(config) {
  env <- config$environment

  # Look for explicit version in dependencies
  for (dep in env$dependencies) {
    if (is.character(dep) && grepl("=", dep)) {
      parts <- strsplit(dep, "\\s*=\\s*")[[1]]
      if (length(parts) >= 2 && parts[1] == config$tool_name) {
        return(parts[2])
      }
    }
  }

  channels <- env$channels
  # If no explicit version found, try to get latest version from conda channels
  channels <- channels[order(match(channels, c("bioconda")))]

  # Try to get latest version from conda
  for (channel in channels) {
    tryCatch(
      {
        latest_version <- .get_latest_version_from_conda(config$tool_name, channel = channel)
        if (!is.null(latest_version) && latest_version != "" && latest_version != "latest") {
          return(latest_version)
        }
      },
      error = function(e) {
        # Continue to next channel
      }
    )
  }

  # For Python packages, try PyPI
  if ("python" %in% sapply(env$dependencies, function(x) {
    if (is.character(x)) strsplit(x, "\\s*=")[[1]][1] else ""
  })) {
    tryCatch(
      {
        return(.get_latest_version_from_pypi(config$tool_name))
      },
      error = function(e) {
        return("latest")
      }
    )
  }

  return("latest")
}

#' Create or Update Tool Object from Configuration
#' @keywords internal
.create_tool_from_config <- function(config, version, base_dir, existing_tool = NULL) {
  tool_name <- config$tool_name

  # If we have an existing tool, update it; otherwise create new one
  if (is.null(existing_tool)) {
    # Create new tool object
    tool <- new("Tool",
      tool_name = tool_name,
      versions = character(0),
      default_version = character(0),
      description = config$description %||% "",
      citation = config$citation %||% "",
      environment = config$environment %||% list(),
      commands = config$commands %||% list(),
      install_dates = list()
    )
  } else {
    tool <- existing_tool
  }

  # Add this version if not already present
  if (!version %in% tool@versions) {
    tool@versions <- c(tool@versions, version)
  }

  # Set default version if not set
  if (length(tool@default_version) == 0) {
    tool@default_version <- version
  }

  # Check for installation - try both simple and complex naming schemes
  install_path <- file.path(base_dir, tool_name, version)

  is_installed <- isTRUE(dir.exists(install_path))

  # Update install date if version is installed
  if (is_installed) {
    # Get install date
    install_date <- file.info(install_path)$mtime
    tool@install_dates[[version]] <- install_date
  } else {
    # Remove install date if not actually installed
    tool@install_dates[[version]] <- NULL
  }

  return(tool)
}

# Tool Installation/Uninstallation Functions -----------------------------------

#' Install Tool Internal Function
#' @keywords internal
.install_tool <- function(toolbox, tool_name, version, config) {
  base_dir <- toolbox@base_dir

  # Determine installation method based on environment
  env <- config$environment
  has_pip <- FALSE

  # Check if environment contains pip dependencies
  if (!is.null(env$dependencies)) {
    for (dep in env$dependencies) {
      if (is.list(dep) && "pip" %in% names(dep)) {
        has_pip <- TRUE
        break
      }
    }
  }

  tryCatch(
    {
      if (has_pip) {
        # Method A: Use YAML file for complex environments with pip
        install_path <- .install_tool_from_yaml(toolbox, tool_name, version, config, base_dir)
      } else {
        # Method B: Use direct sn_install_tool for simple conda packages
        install_path <- .install_tool_direct(toolbox, tool_name, version, config, base_dir)
      }

      # Verify installation was successful by checking if the environment exists
      if (!is.null(install_path) && dir.exists(install_path)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    error = function(e) {
      cli_alert_danger("Installation failed: {e$message}")
      return(FALSE)
    }
  )
}

#' Install Tool Directly Using YAML with Proper Version Resolution
#' @keywords internal
.install_tool_direct <- function(toolbox, tool_name, version, config, base_dir) {
  cli_alert_info("Installing {tool_name} version {version} using conda...")

  # Use the same logic as .install_tool_with_yaml for consistency
  # Extract environment specification from the tool YAML
  env <- config$environment %||% list()
  channels <- env$channels %||% c("conda-forge", "bioconda")
  dependencies <- env$dependencies %||% list(paste0(tool_name, "=", version))

  # Resolve versions for dependencies that don't have explicit versions
  resolved_dependencies <- .resolve_dependency_versions(dependencies, channels, TRUE)

  # Get the actual version for the main tool for path naming
  actual_version <- .get_tool_version_from_dependencies(tool_name, resolved_dependencies, version)

  # Create the environment path in tool_name/version format
  env_path <- file.path(base_dir, tool_name, actual_version)

  # Check if already exists and is complete
  if (dir.exists(env_path)) {
    # Verify installation completeness by checking for essential conda environment files
    if (.is_environment_complete(env_path)) {
      cli_alert_success("Environment already exists at {env_path}")
      return(env_path)
    } else {
      cli_alert_warning("Incomplete installation detected at {env_path}, removing and reinstalling...")
      unlink(env_path, recursive = TRUE)
    }
  }

  # Create temporary YAML environment file
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml), add = TRUE)

  yaml_content <- list(
    name = basename(env_path), # Use simple name for YAML
    channels = channels,
    dependencies = .process_dependencies_for_yaml(resolved_dependencies)
  )
  write_yaml(yaml_content, temp_yaml)

  # Create environment from YAML file using low-level mamba call
  cli_alert_info("Creating environment '{tool_name}/{actual_version}' from YAML")
  result <- .mamba(
    subcommand = "env",
    options = c(
      "create",
      "-y",
      "-p", env_path,
      "-f", temp_yaml
    )
  )

  if (result$exit_code != 0) {
    cli_abort("Failed to create environment from YAML: {result$stderr}")
  }

  # Verify environment was created
  if (!dir.exists(env_path)) {
    cli_abort("Environment creation failed at {env_path}")
  }

  # Enhanced verification: check if tool is actually properly installed
  temp_tool <- tryCatch(
    {
      new("Tool",
        tool_name = tool_name,
        versions = actual_version,
        default_version = actual_version,
        description = config$description %||% "",
        citation = config$citation %||% "",
        environment = config$environment %||% list(),
        commands = config$commands %||% list(),
        install_dates = list()
      )
    },
    error = function(e) NULL
  )

  if (!is.null(temp_tool)) {
    diagnosis <- .diagnose_tool_installation(temp_tool, actual_version, base_dir, show_details = TRUE)

    if (!diagnosis$installed) {
      cli_alert_danger("Environment created but tool installation incomplete:")
      if (!is.null(diagnosis$issues)) {
        for (issue in diagnosis$issues) {
          cli_text("  • {issue}")
        }
      }

      # Show YAML content for debugging
      if (file.exists(temp_yaml)) {
        cli_alert_info("YAML content used for installation:")
        yaml_lines <- readLines(temp_yaml)
        for (line in head(yaml_lines, 15)) {
          cli_text("  {line}")
        }
        if (length(yaml_lines) > 15) {
          cli_text("  ... ({length(yaml_lines) - 15} more lines)")
        }
      }

      cli_alert_warning("Tool may not be available in specified channels or version may be incorrect")
    } else {
      cli_alert_success("Tool installation verified successfully")
    }
  }

  cli_alert_success("Environment created successfully at {env_path}")
  return(env_path)
}

#' Install Tool from YAML Environment
#' @keywords internal
.install_tool_from_yaml <- function(toolbox, tool_name, version, config, base_dir) {
  cli_alert_info("Installing {tool_name} version {version} using YAML environment...")

  # Create the environment path in tool_name/version format (consistent with direct method)
  env_path <- file.path(base_dir, tool_name, version)

  # Check if already exists and is complete
  if (dir.exists(env_path)) {
    # Verify installation completeness by checking for essential conda environment files
    if (.is_environment_complete(env_path)) {
      cli_alert_success("Environment already exists at {env_path}")
      return(env_path)
    } else {
      cli_alert_warning("Incomplete installation detected at {env_path}, removing and reinstalling...")
      unlink(env_path, recursive = TRUE)
    }
  }

  # Create temporary YAML file from environment config
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))

  # Convert environment config to YAML format
  yaml_content <- list(
    name = basename(env_path), # Use simple name for YAML
    channels = config$environment$channels %||% c("conda-forge", "bioconda"),
    dependencies = .process_dependencies_for_yaml(config$environment$dependencies %||% list())
  )

  # Write YAML file
  write_yaml(yaml_content, temp_yaml)

  # Create environment from YAML file using low-level mamba call
  cli_alert_info("Creating environment '{tool_name}/{version}' from YAML")
  result <- .mamba(
    subcommand = "env",
    options = c(
      "create",
      "-y",
      "-p", env_path,
      "-f", temp_yaml
    )
  )

  if (result$exit_code != 0) {
    cli_abort("Failed to create environment from YAML: {result$stderr}")
  }

  # Verify environment was created and is complete
  if (!dir.exists(env_path) || !.is_environment_complete(env_path)) {
    cli_abort("Environment creation failed or incomplete at {env_path}")
  }

  cli_alert_success("Environment created successfully at {env_path}")
  return(env_path)
}

#' Process Dependencies for YAML Writing
#' @keywords internal
.process_dependencies_for_yaml <- function(dependencies) {
  # Always return a list to ensure YAML array format
  if (is.null(dependencies) || length(dependencies) == 0) {
    return(list())
  }

  if (is.character(dependencies)) {
    # Convert character vector to list, ensuring array format in YAML
    return(as.list(dependencies))
  }

  if (is.list(dependencies)) {
    processed <- list()
    for (i in seq_along(dependencies)) {
      dep <- dependencies[[i]]
      if (is.character(dep) && length(dep) == 1) {
        # Single character dependency
        processed[[length(processed) + 1]] <- dep
      } else if (is.list(dep) && "pip" %in% names(dep)) {
        # Handle pip dependencies specially
        pip_deps <- dep[["pip"]]
        if (is.character(pip_deps)) {
          processed[[length(processed) + 1]] <- list(pip = as.list(pip_deps))
        } else {
          processed[[length(processed) + 1]] <- dep
        }
      } else {
        processed[[length(processed) + 1]] <- dep
      }
    }
    return(processed)
  }

  # Fallback: convert to list
  return(list(dependencies))
}

#' Check if Conda Environment is Complete
#' @keywords internal
.is_environment_complete <- function(env_path) {
  # Check for essential conda environment directories/files
  essential_items <- c("conda-meta", "bin", "lib")

  for (item in essential_items) {
    item_path <- file.path(env_path, item)
    if (!dir.exists(item_path)) {
      return(FALSE)
    }
  }

  # Additional check: conda-meta should contain package records
  conda_meta_path <- file.path(env_path, "conda-meta")
  meta_files <- list.files(conda_meta_path, pattern = "\\.json$")

  # Should have at least a few package records
  return(length(meta_files) >= 3)
}

#' Uninstall Tool Internal Function
#' @keywords internal
.uninstall_tool <- function(toolbox, tool_name, version) {
  base_dir <- toolbox@base_dir

  # Determine the environment path
  # First try the simple path (tool_name/version)
  env_path <- file.path(base_dir, tool_name, version)

  if (!dir.exists(env_path)) {
    # Try the complex path (tool_name_version)
    env_path <- file.path(base_dir, paste0(tool_name, "_", version))
  }

  if (!dir.exists(env_path)) {
    cli_alert_warning("Installation path not found for {tool_name} version {version}")
    return(FALSE)
  }

  cli_alert_info("Uninstalling {tool_name} version {version} from {env_path}")

  tryCatch(
    {
      unlink(env_path, recursive = TRUE)
      cli_alert_success("Successfully uninstalled {tool_name} version {version}")
      return(TRUE)
    },
    error = function(e) {
      cli_alert_danger("Failed to uninstall {tool_name}: {e$message}")
      return(FALSE)
    }
  )
}

# User Toolbox Management API ----------------------------------------

#' Load Toolbox from Specific Path
#'
#' Load a toolbox from a specific file path. If path is NULL, uses smart loading
#' with priority: user environment variable > user config > package default.
#'
#' @param path Character. Path to toolbox RDS file. If NULL, uses smart loading.
#'
#' @return Toolbox object.
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' # Load from specific path
#' toolbox <- sn_load_toolbox("/path/to/my_toolbox.rds")
#'
#' # Smart loading (recommended)
#' toolbox <- sn_load_toolbox()
#' }
sn_load_toolbox <- function(path = NULL) {
  if (!is.null(path)) {
    if (!file.exists(path)) {
      cli_abort("Toolbox file not found: {path}")
    }

    tryCatch(
      {
        toolbox <- readRDS(path)
        if (!inherits(toolbox, "Toolbox")) {
          cli_abort("File does not contain a valid Toolbox object: {path}")
        }

        # Update global toolbox
        .set_default_toolbox(toolbox)
        cli_alert_success("Loaded toolbox from {path}")
        return(toolbox)
      },
      error = function(e) {
        cli_abort("Failed to load toolbox from {path}: {e$message}")
      }
    )
  } else {
    # Smart loading
    toolbox <- .load_smart_toolbox()
    .set_default_toolbox(toolbox)
    return(toolbox)
  }
}

#' Save Toolbox to Specific Path
#'
#' Save the current toolbox to a specific file path. If path is NULL,
#' saves to user config directory.
#'
#' @param toolbox Toolbox object to save. If NULL, uses current default toolbox.
#' @param path Character. Path where to save the toolbox. If NULL, saves to user config.
#'
#' @return Logical. TRUE if successful, FALSE otherwise.
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' # Save to specific path
#' sn_save_toolbox(toolbox, "/path/to/my_toolbox.rds")
#'
#' # Save to user config (default)
#' sn_save_toolbox(toolbox)
#'
#' # Save current toolbox
#' sn_save_toolbox()
#' }
sn_save_toolbox <- function(toolbox = NULL, path = NULL) {
  if (is.null(toolbox)) {
    toolbox <- .get_default_toolbox()
  }

  if (!inherits(toolbox, "Toolbox")) {
    cli_abort("First argument must be a Toolbox object")
  }

  if (!is.null(path)) {
    # Save to specific path
    tryCatch(
      {
        saveRDS(toolbox, path)
        cli_alert_success("Toolbox saved to {path}")
        return(TRUE)
      },
      error = function(e) {
        cli_alert_danger("Failed to save toolbox to {path}: {e$message}")
        return(FALSE)
      }
    )
  } else {
    # Save to user config
    success <- .save_user_toolbox(toolbox)
    if (success) {
      user_config_dir <- R_user_dir("shennong-tools", "config")
      user_toolbox_path <- file.path(user_config_dir, "user_toolbox.rds")
      cli_alert_success("Toolbox saved to user config: {user_toolbox_path}")
    }
    return(success)
  }
}

#' Reset to Default Toolbox
#'
#' Reset the current toolbox to the package default (pre-loaded with all built-in tools).
#' This will remove any user customizations.
#'
#' @param remove_user_config Logical. Whether to remove the user config file.
#'
#' @return Toolbox object (the default toolbox).
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' # Reset to default, keep user config file
#' toolbox <- sn_reset_to_default()
#'
#' # Reset and remove user config file
#' toolbox <- sn_reset_to_default(remove_user_config = TRUE)
#' }
sn_reset_to_default <- function(remove_user_config = FALSE) {
  if (remove_user_config) {
    user_config_dir <- R_user_dir("shennong-tools", "config")
    user_toolbox_path <- file.path(user_config_dir, "user_toolbox.rds")
    if (file.exists(user_toolbox_path)) {
      unlink(user_toolbox_path)
      cli_alert_info("Removed user toolbox config: {user_toolbox_path}")
    }
  }

  # Clear current toolbox from memory
  if (exists("current_toolbox", envir = .toolbox_env)) {
    rm("current_toolbox", envir = .toolbox_env)
  }

  # Load fresh default toolbox
  toolbox <- .load_smart_toolbox()
  .set_default_toolbox(toolbox)

  cli_alert_success("Reset to default toolbox with {length(toolbox@tools)} built-in tools")
  return(toolbox)
}

#' Get Toolbox Information
#'
#' Display information about the current toolbox including source,
#' number of tools, and file paths.
#'
#' @param toolbox Toolbox object. If NULL, uses current default toolbox.
#'
#' @return List with toolbox information (invisibly).
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' # Show current toolbox info
#' sn_get_toolbox_info()
#'
#' # Show specific toolbox info
#' sn_get_toolbox_info(my_toolbox)
#' }
sn_get_toolbox_info <- function(toolbox = NULL) {
  if (is.null(toolbox)) {
    toolbox <- .get_default_toolbox()
  }

  if (!inherits(toolbox, "Toolbox")) {
    cli_abort("Argument must be a Toolbox object")
  }

  # Determine toolbox source
  user_config_dir <- R_user_dir("shennong-tools", "config")
  user_toolbox_path <- file.path(user_config_dir, "user_toolbox.rds")
  user_env_path <- Sys.getenv("SHENNONG_TOOLBOX_PATH", "")

  source <- "default"
  source_path <- "package data (data/default_toolbox.rda)"

  if (nzchar(user_env_path) && file.exists(user_env_path)) {
    source <- "environment variable"
    source_path <- user_env_path
  } else if (file.exists(user_toolbox_path)) {
    source <- "user config"
    source_path <- user_toolbox_path
  }

  # Display information
  cli_rule(left = paste0(symbol$info, " Toolbox Information"))

  cli_text("{col_blue('Source:')} {source}")
  cli_text("{col_blue('Path:')} {source_path}")
  cli_text("{col_blue('Base Directory:')} {toolbox@base_dir}")
  cli_text("{col_blue('Mamba Path:')} {toolbox@mamba_path}")
  cli_text("{col_blue('Tools:')} {length(toolbox@tools)} tools available")

  if (length(toolbox@tools) > 0) {
    cli_text()
    cli_text("{col_silver('Available tools:')}")
    for (tool_name in sort(names(toolbox@tools))) {
      tool <- toolbox@tools[[tool_name]]
      versions <- sn_get_tool_versions(tool)
      default_ver <- sn_get_tool_version(tool)

      if (length(versions) > 0) {
        version_text <- if (length(versions) == 1) {
          paste0("v", versions[1])
        } else {
          paste0("v", default_ver, " (", length(versions), " versions)")
        }
        cli_text("  • {tool_name} ({version_text})")
      } else {
        cli_text("  • {tool_name} (no versions)")
      }
    }
  }

  cli_rule()

  # Return information invisibly
  info <- list(
    source = source,
    source_path = source_path,
    base_dir = toolbox@base_dir,
    mamba_path = toolbox@mamba_path,
    n_tools = length(toolbox@tools),
    tool_names = names(toolbox@tools)
  )

  invisible(info)
}

#' Check User Toolbox Status
#'
#' Check if user has made modifications to the default toolbox and
#' provide information about the current toolbox state.
#'
#' @return List with toolbox status information.
#' @family tool management
#' @concept tool management
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if user has modified toolbox
#' status <- sn_check_user_toolbox()
#' print(status)
#' }
sn_check_user_toolbox <- function() {
  user_config_dir <- R_user_dir("shennong-tools", "config")
  user_toolbox_path <- file.path(user_config_dir, "user_toolbox.rds")
  user_env_path <- Sys.getenv("SHENNONG_TOOLBOX_PATH", "")

  # Check different sources
  has_env_toolbox <- nzchar(user_env_path) && file.exists(user_env_path)
  has_user_config <- file.exists(user_toolbox_path)

  # Get current toolbox
  current_toolbox <- .get_default_toolbox()

  # Determine source
  source <- if (has_env_toolbox) {
    "environment variable"
  } else if (has_user_config) {
    "user config"
  } else {
    "default package data"
  }

  # Create status info
  status <- list(
    source = source,
    has_user_modifications = has_env_toolbox || has_user_config,
    user_config_path = if (has_user_config) user_toolbox_path else NA,
    env_path = if (has_env_toolbox) user_env_path else NA,
    n_tools = length(current_toolbox@tools),
    tool_names = names(current_toolbox@tools)
  )

  # Display information
  cli_rule(left = paste0(symbol$info, " User Toolbox Status"))

  cli_text("{col_blue('Source:')} {source}")

  if (status$has_user_modifications) {
    cli_text("{col_green('Status:')} User has made modifications")
    if (has_env_toolbox) {
      cli_text("{col_blue('Environment Path:')} {user_env_path}")
    }
    if (has_user_config) {
      cli_text("{col_blue('User Config:')} {user_toolbox_path}")
    }
  } else {
    cli_text("{col_grey('Status:')} Using default package toolbox")
    cli_text("{col_silver('Note:')} No user modifications detected")
  }

  cli_text("{col_blue('Tools:')} {status$n_tools} tools configured")

  cli_rule()

  # Provide helpful commands
  if (status$has_user_modifications) {
    cli_text("{col_silver('Commands:')}")
    cli_text("  • {.code sn_reset_to_default()} - Reset to package default")
    cli_text("  • {.code sn_save_toolbox()} - Save current state")
    cli_text("  • {.code sn_get_toolbox_info()} - View detailed toolbox info")
  } else {
    cli_text("{col_silver('Commands:')}")
    cli_text("  • {.code sn_add_tool()} - Add tools (will create user config)")
    cli_text("  • {.code sn_remove_tool()} - Remove tools (will create user config)")
    cli_text("  • {.code sn_get_toolbox_info()} - View detailed toolbox info")
  }

  invisible(status)
}
