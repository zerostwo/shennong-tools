#' Initialize a Toolbox
#'
#' Creates a new Toolbox object with empty tool list and valid mamba path.
#' This is the starting point for managing bioinformatics tools.
#'
#' @param base_dir Character. Base directory for tool installations.
#'   Installation paths will be {base_dir}/{tool_name}/{version}.
#' @param mamba_path Character. Path to mamba/micromamba executable.
#'   If NULL, will try to find automatically.
#'
#' @return Toolbox object.
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
    install = FALSE, ...
  ) {
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
  if (!is.null(tool_name)) {
    # Load from built-in registry
    tool_config <- .load_builtin_tool(tool_name, version)
    version <- .resolve_latest_version(tool_config)
  } else {
    # Load from custom YAML
    tool_config <- .load_tool_from_yaml(yaml)
    tool_name <- tool_config$tool_name
    if (is.null(version)) {
      version <- .extract_version_from_config(tool_config)
    }
  }

  # Create Tool object
  tool <- .create_tool_from_config(tool_config, version, toolbox@base_dir)

  # Add to toolbox
  if (!tool_name %in% names(toolbox@tools)) {
    toolbox@tools[[tool_name]] <- list()
  }

  # Check if version already exists
  if (version %in% names(toolbox@tools[[tool_name]])) {
    cli_alert_warning("Tool {tool_name} version {version} already exists, replacing")
  }

  toolbox@tools[[tool_name]][[version]] <- tool

  cli_alert_success("Added {tool_name} version {version} to toolbox")

  # Install if requested
  if (install) {
    install_success <- .install_tool(toolbox, tool_name, version, tool_config)
    if (install_success) {
      # Update the tool's installed status
      toolbox@tools[[tool_name]][[version]]@installed <- TRUE
      toolbox@tools[[tool_name]][[version]]@install_date <- Sys.time()
    } else {
      cli_alert_warning("Installation failed for {tool_name} version {version}")
    }
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

  # Remove specific version or all versions
  if (!is.null(version)) {
    if (!version %in% names(toolbox@tools[[tool_name]])) {
      cli_alert_warning("Tool {tool_name} version {version} not found")
      return(toolbox)
    }

    # Uninstall if requested
    if (uninstall && toolbox@tools[[tool_name]][[version]]@installed) {
      success <- .uninstall_tool(toolbox, tool_name, version)
      if (success) {
        toolbox@tools[[tool_name]][[version]]@installed <- FALSE
      }
    }

    # Remove version
    toolbox@tools[[tool_name]][[version]] <- NULL

    # If no versions left, remove tool entirely
    if (length(toolbox@tools[[tool_name]]) == 0) {
      toolbox@tools[[tool_name]] <- NULL
    }

    cli_alert_success("Removed {tool_name} version {version}")
  } else {
    # Remove all versions
    versions <- names(toolbox@tools[[tool_name]])

    if (uninstall) {
      for (ver in versions) {
        if (toolbox@tools[[tool_name]][[ver]]@installed) {
          success <- .uninstall_tool(toolbox, tool_name, ver)
          if (success) {
            toolbox@tools[[tool_name]][[ver]]@installed <- FALSE
          }
        }
      }
    }

    toolbox@tools[[tool_name]] <- NULL
    cli_alert_success("Removed all versions of {tool_name}")
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

  config$environment$dependencies <- ifelse(
    config$environment$dependencies == tool_name,
    paste0(tool_name, "=", version),
    config$environment$dependencies
  )

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

#' Create Tool Object from Configuration
#' @keywords internal
.create_tool_from_config <- function(config, version, base_dir) {
  # Check for installation - try both simple and complex naming schemes
  install_path1 <- file.path(base_dir, config$tool_name, version)
  install_path2 <- file.path(base_dir, paste0(config$tool_name, "_", version))

  path1_exists <- isTRUE(dir.exists(install_path1))
  path2_exists <- isTRUE(dir.exists(install_path2))
  is_installed <- path1_exists || path2_exists

  # Only get install date if tool is actually installed
  install_date <- if (is_installed) {
    actual_install_path <- if (path1_exists) install_path1 else install_path2
    file.info(actual_install_path)$mtime
  } else {
    as.POSIXct(character(0))
  }

  new("Tool",
    tool_name = config$tool_name,
    version = version,
    description = config$description %||% "",
    citation = config$citation %||% "",
    environment = config$environment %||% list(),
    commands = config$commands %||% list(),
    installed = is_installed,
    install_date = install_date
  )
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

  # Check if already exists
  if (dir.exists(env_path)) {
    cli_alert_success("Environment already exists at {env_path}")
    return(env_path)
  }

  # Create temporary YAML environment file
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml), add = TRUE)

  yaml_content <- list(
    name = basename(env_path), # Use simple name for YAML
    channels = channels,
    dependencies = resolved_dependencies
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

  cli_alert_success("Environment created successfully at {env_path}")
  return(env_path)
}

#' Install Tool from YAML Environment
#' @keywords internal
.install_tool_from_yaml <- function(toolbox, tool_name, version, config, base_dir) {
  cli_alert_info("Installing {tool_name} version {version} using YAML environment...")

  # Create temporary YAML file from environment config
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml))

  # Convert environment config to YAML format
  yaml_content <- list(
    name = paste0(tool_name, "_", version),
    channels = config$environment$channels %||% c("conda-forge", "bioconda"),
    dependencies = config$environment$dependencies %||% list()
  )

  # Write YAML file
  write_yaml(yaml_content, temp_yaml)

  # Install using sn_install_tool with YAML
  install_path <- sn_install_tool(
    tool_name = paste0(tool_name, "_", version),
    yaml = temp_yaml,
    base_dir = base_dir,
    overwrite = FALSE
  )

  return(install_path)
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
