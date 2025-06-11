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
        mamba_path <- .check_mamba()
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
#' Formats file size in bytes to human-readable format.
#'
#' @param bytes Numeric. File size in bytes.
#'
#' @return Character. Formatted file size.
#' @keywords internal
.format_file_size <- function(bytes) {
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
#' Formats memory size in bytes to human-readable format.
#'
#' @param bytes Numeric. Memory size in bytes.
#'
#' @return Character. Formatted memory size.
#' @keywords internal
.format_memory <- function(bytes) {
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
#' Formats duration in seconds to human-readable format.
#'
#' @param seconds Numeric. Duration in seconds.
#'
#' @return Character. Formatted duration.
#' @keywords internal
.format_duration <- function(seconds) {
  if (is.na(seconds) || seconds == 0) {
    return("0s")
  }

  if (seconds < 60) {
    return(sprintf("%.1fs", seconds))
  } else if (seconds < 3600) {
    minutes <- floor(seconds / 60)
    remaining_seconds <- seconds %% 60
    return(sprintf("%dm %.1fs", minutes, remaining_seconds))
  } else {
    hours <- floor(seconds / 3600)
    remaining_seconds <- seconds %% 3600
    minutes <- floor(remaining_seconds / 60)
    seconds <- remaining_seconds %% 60
    return(sprintf("%dh %dm %.1fs", hours, minutes, seconds))
  }
}

#' Null Default Operator
#'
#' Returns right-hand side if left-hand side is NULL.
#'
#' @param x Left-hand side value.
#' @param y Right-hand side value.
#'
#' @return x if not NULL, otherwise y.
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Find Tool Environment Path
#'
#' Find the installation path for a tool environment.
#'
#' @param tool_name Character. Name of the tool.
#' @param tool_config List. Tool configuration.
#'
#' @return Character. Path to tool environment or NULL if not found.
#' @keywords internal
.sn_find_tool_env <- function(tool_name, tool_config) {
  # This is a placeholder function that should work with the existing toolbox system
  # In the actual implementation, this would integrate with the toolbox structure
  base_dir <- R_user_dir(package = "shennong-tools", which = "data")

  # Try both naming schemes
  env_path1 <- file.path(base_dir, tool_name)
  env_path2 <- file.path(base_dir, paste0(tool_name, "_latest"))

  if (dir.exists(env_path1)) {
    return(env_path1)
  } else if (dir.exists(env_path2)) {
    return(env_path2)
  } else {
    return(NULL)
  }
}

#' Normalize Log Level
#'
#' Convert log level to numeric value.
#'
#' @param log_level Character or integer. Log level.
#'
#' @return Integer. Numeric log level.
#' @keywords internal
.sn_normalize_log_level <- function(log_level) {
  if (is.character(log_level)) {
    switch(log_level,
      "silent" = 0,
      "quiet" = 0,
      "minimal" = 1,
      "normal" = 2,
      2 # default
    )
  } else {
    as.integer(log_level)
  }
}

#' Resolve versions for dependencies that don't have explicit versions
#' @keywords internal
.resolve_dependency_versions <- function(dependencies, channels, show_messages = TRUE) {
  resolved <- character(0)

  for (dep in dependencies) {
    if (is.character(dep)) {
      if (grepl("=", dep)) {
        # Already has version specified
        resolved <- c(resolved, dep)
      } else {
        # No version specified, get latest
        tool_name <- trimws(dep)
        tryCatch(
          {
            # Try bioconda first, then conda-forge
            latest_version <- NULL
            for (channel in c("bioconda", "conda-forge")) {
              if (channel %in% channels) {
                tryCatch(
                  {
                    latest_version <- .get_latest_version_from_conda(tool_name, channel = channel)
                    break
                  },
                  error = function(e) {
                    # Continue to next channel
                  }
                )
              }
            }

            if (!is.null(latest_version)) {
              resolved <- c(resolved, paste0(tool_name, "=", latest_version))
              if (show_messages) {
                cli_alert_info("Resolved {tool_name} to version {latest_version}")
              }
            } else {
              # Fallback: use without version (let conda resolve)
              resolved <- c(resolved, dep)
              if (show_messages) {
                cli_alert_warning("Could not resolve version for {tool_name}, using latest available")
              }
            }
          },
          error = function(e) {
            # If version resolution fails, use the original dependency
            resolved <- c(resolved, dep)
            if (show_messages) {
              cli_alert_warning("Version resolution failed for {tool_name}: {e$message}")
            }
          }
        )
      }
    } else {
      # Non-character dependency (e.g., pip dependencies), keep as-is
      resolved <- c(resolved, dep)
    }
  }

  return(resolved)
}

#' Extract the version for the main tool from resolved dependencies
#' @keywords internal
.get_tool_version_from_dependencies <- function(tool_name, resolved_dependencies, fallback_version) {
  for (dep in resolved_dependencies) {
    if (is.character(dep) && grepl("=", dep)) {
      parts <- strsplit(dep, "\\s*=\\s*")[[1]]
      if (length(parts) >= 2 && parts[1] == tool_name) {
        return(parts[2])
      }
    }
  }

  # If not found in dependencies, return fallback
  return(fallback_version)
}

#' Load and Manage Datatype Definitions
#' @description Load datatype definitions from YAML files with support for
#' global datatypes and tool-specific extensions
#' @param tool_name Optional tool name to load tool-specific datatypes
#' @return List containing file_types and value_types definitions
#' @keywords internal
.sn_load_datatypes <- function(tool_name = NULL) {
  # Load global datatypes
  global_datatypes_path <- system.file("config", "datatypes.yaml", package = "ShennongTools")

  if (!file.exists(global_datatypes_path)) {
    stop("Global datatypes.yaml not found at: ", global_datatypes_path)
  }

  global_datatypes <- yaml::read_yaml(global_datatypes_path)

  # Load tool-specific datatypes if specified
  if (!is.null(tool_name)) {
    tool_datatypes_path <- system.file("tools", tool_name, "datatypes.yaml", package = "ShennongTools")

    if (file.exists(tool_datatypes_path)) {
      tool_datatypes <- yaml::read_yaml(tool_datatypes_path)

      # Merge tool-specific datatypes with global ones (tool-specific takes precedence)
      if (!is.null(tool_datatypes$file_types)) {
        global_datatypes$file_types <- c(global_datatypes$file_types, tool_datatypes$file_types)
      }
      if (!is.null(tool_datatypes$value_types)) {
        global_datatypes$value_types <- c(global_datatypes$value_types, tool_datatypes$value_types)
      }
    }
  }

  return(global_datatypes)
}

#' Get Example Value for Datatype
#' @description Get the example value for a given datatype from the datatype registry
#' @param datatype The datatype name
#' @param tool_name Optional tool name for tool-specific datatypes
#' @param input_output Whether this is for "input" or "output" (affects path prefix)
#' @return Character string with example value
#' @keywords internal
.sn_get_example_value <- function(datatype, tool_name = NULL, input_output = "input") {
  # Handle datatype as vector (take first one)
  if (length(datatype) > 1) {
    datatype <- datatype[1]
  }

  datatypes <- .sn_load_datatypes(tool_name)

  # Check in file_types first
  if (!is.null(datatypes$file_types[[datatype]])) {
    example_val <- datatypes$file_types[[datatype]]$example_value
    if (!is.null(example_val)) {
      # Adjust path prefix for outputs
      if (input_output == "output" && grepl("^path/to/", example_val)) {
        example_val <- gsub("^path/to/", "output/", example_val)
      }
      return(paste0("\"", example_val, "\""))
    }
  }

  # Check in value_types
  if (!is.null(datatypes$value_types[[datatype]])) {
    example_val <- datatypes$value_types[[datatype]]$example_value
    if (!is.null(example_val)) {
      return(example_val)
    }
  }

  # Fallback for unknown datatypes
  return("\"path/to/file\"")
}

#' Validate Datatype
#' @description Validate if a datatype is defined in the datatype registry
#' @param datatype The datatype name
#' @param tool_name Optional tool name for tool-specific datatypes
#' @return TRUE if valid, FALSE otherwise
#' @keywords internal
.sn_validate_datatype <- function(datatype, tool_name = NULL) {
  # Handle datatype as vector
  if (length(datatype) > 1) {
    return(all(sapply(datatype, function(dt) .sn_validate_datatype(dt, tool_name))))
  }

  datatypes <- .sn_load_datatypes(tool_name)

  # Check if datatype exists in file_types or value_types
  exists_in_file_types <- !is.null(datatypes$file_types[[datatype]])
  exists_in_value_types <- !is.null(datatypes$value_types[[datatype]])

  return(exists_in_file_types || exists_in_value_types)
}

#' Generate Unified Usage Example
#' @description Generate usage example with actual parameters using datatype registry
#' @param cmd_config Command configuration
#' @param command_name Command name
#' @param tool_name Tool name
#' @return Character string with usage example
#' @keywords internal
.sn_generate_usage_example <- function(cmd_config, command_name, tool_name) {
  # Collect all parameters
  example_params <- list()

  # Add inputs with example values
  if (!is.null(cmd_config$inputs)) {
    for (input_name in names(cmd_config$inputs)) {
      input_def <- cmd_config$inputs[[input_name]]
      datatype <- input_def$datatype %||% "string"

      example_value <- .sn_get_example_value(datatype, tool_name, "input")
      example_params[[input_name]] <- example_value
    }
  }

  # Add outputs with example values
  if (!is.null(cmd_config$outputs)) {
    for (output_name in names(cmd_config$outputs)) {
      output_def <- cmd_config$outputs[[output_name]]
      datatype <- output_def$datatype %||% "string"

      example_value <- .sn_get_example_value(datatype, tool_name, "output")
      example_params[[output_name]] <- example_value
    }
  }

  # Add parameters with their defaults or example values
  if (!is.null(cmd_config$params)) {
    for (param_name in names(cmd_config$params)) {
      param_def <- cmd_config$params[[param_name]]

      if (!is.null(param_def$default)) {
        # Use the actual default value
        default_val <- param_def$default
        example_params[[param_name]] <- if (is.character(default_val)) {
          if (default_val == "") "\"\"" else paste0("\"", default_val, "\"")
        } else {
          as.character(default_val)
        }
      } else {
        # Generate example based on datatype
        datatype <- param_def$datatype %||% "string"
        example_value <- .sn_get_example_value(datatype, tool_name, "param")
        example_params[[param_name]] <- example_value
      }
    }
  }

  # Build the usage example
  if (length(example_params) == 0) {
    return(paste0("sn_run(\"", tool_name, "\", \"", command_name, "\")"))
  }

  # Format parameters nicely
  param_lines <- character(0)
  for (param_name in names(example_params)) {
    param_lines <- c(param_lines, paste0("  ", param_name, " = ", example_params[[param_name]]))
  }

  # Create multi-line example for better readability
  if (length(param_lines) <= 2) {
    # Single line for short examples
    params_str <- paste(param_lines, collapse = ", ")
    return(paste0("sn_run(\"", tool_name, "\", \"", command_name, "\", ", gsub("^  ", "", params_str), ")"))
  } else {
    # Multi-line for longer examples
    result <- paste0("sn_run(\"", tool_name, "\", \"", command_name, "\",\n")
    result <- paste0(result, paste(param_lines, collapse = ",\n"))
    result <- paste0(result, "\n)")
    return(result)
  }
}
