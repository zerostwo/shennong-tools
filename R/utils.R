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
#' @family core functions
#' @concept core functions
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
        .normalize_log_level(log_level)
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

#' Normalize Log Level
#' @keywords internal
.normalize_log_level <- function(log_level) {
  # Validate log_level parameter
  valid_levels <- c("silent", "quiet", "minimal", "normal", 0, 1, 2)

  if (!log_level %in% valid_levels) {
    cli_abort("Invalid log_level: {log_level}. Must be one of: 'silent'/'quiet'/0, 'minimal'/1, 'normal'/2")
  }

  if (is.character(log_level)) {
    switch(log_level,
      "silent" = 0,
      "quiet" = 0,
      "minimal" = 1,
      "normal" = 2,
      1 # This line should never be reached due to validation above
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
.load_datatypes <- function(tool_name = NULL) {
  # Load global datatypes
  global_datatypes_path <- system.file("config", "datatypes.yaml", package = "ShennongTools")

  if (!file.exists(global_datatypes_path)) {
    stop("Global datatypes.yaml not found at: ", global_datatypes_path)
  }

  global_datatypes <- read_yaml(global_datatypes_path)

  # Load tool-specific datatypes if specified
  if (!is.null(tool_name)) {
    tool_datatypes_path <- system.file("tools", tool_name, "datatypes.yaml", package = "ShennongTools")

    if (file.exists(tool_datatypes_path)) {
      tool_datatypes <- read_yaml(tool_datatypes_path)

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
#' @param param_name The parameter name (for context-aware examples)
#' @param tool_name Optional tool name for tool-specific datatypes
#' @param input_output Whether this is for "input" or "output" (affects path prefix)
#' @return Character string with example value
#' @keywords internal
.get_example_value <- function(datatype, param_name = NULL, tool_name = NULL, input_output = "input") {
  # Handle datatype as vector (take first one)
  if (length(datatype) > 1) {
    datatype <- datatype[1]
  }

  datatypes <- .load_datatypes(tool_name)

  # Check in file_types first
  if (!is.null(datatypes$file_types[[datatype]])) {
    example_val <- datatypes$file_types[[datatype]]$example_value
    if (!is.null(example_val)) {
      # Apply smart naming based on parameter name
      example_val <- .apply_smart_naming(example_val, param_name, datatype, input_output)

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
.validate_datatype <- function(datatype, tool_name = NULL) {
  # Handle datatype as vector
  if (length(datatype) > 1) {
    return(all(sapply(datatype, function(dt) .validate_datatype(dt, tool_name))))
  }

  datatypes <- .load_datatypes(tool_name)

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
.generate_usage_example <- function(cmd_config, command_name, tool_name) {
  # Collect all parameters
  example_params <- list()

  # Add inputs with example values
  if (!is.null(cmd_config$inputs)) {
    for (input_name in names(cmd_config$inputs)) {
      input_def <- cmd_config$inputs[[input_name]]
      datatype <- input_def$datatype %||% "string"

      example_value <- .get_example_value(datatype, input_name, tool_name, "input")
      example_params[[input_name]] <- example_value
    }
  }

  # Add outputs with example values
  if (!is.null(cmd_config$outputs)) {
    for (output_name in names(cmd_config$outputs)) {
      output_def <- cmd_config$outputs[[output_name]]
      datatype <- output_def$datatype %||% "string"

      example_value <- .get_example_value(datatype, output_name, tool_name, "output")
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
        example_value <- .get_example_value(datatype, param_name, tool_name, "param")
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

#' Apply Smart Naming to Example Values
#' @description Generate context-aware example file names based on parameter names
#' @param example_val Base example value from datatype config
#' @param param_name Parameter name (for context)
#' @param datatype File datatype
#' @param input_output Whether this is for "input" or "output"
#' @return Character string with context-aware example value
#' @keywords internal
.apply_smart_naming <- function(example_val, param_name, datatype, input_output) {
  if (is.null(param_name) || !is.character(param_name)) {
    return(example_val)
  }

  # Convert to lowercase for pattern matching
  param_lower <- tolower(param_name)

  # Pattern matching for FASTQ files
  if (datatype == "fastq") {
    # For paired-end sequencing files
    if (grepl("input1|read1|r1", param_lower)) {
      return(gsub("reads\\.fastq\\.gz", "read1.fastq.gz", example_val))
    } else if (grepl("input2|read2|r2", param_lower)) {
      return(gsub("reads\\.fastq\\.gz", "read2.fastq.gz", example_val))
    } else if (grepl("forward|fwd", param_lower)) {
      return(gsub("reads\\.fastq\\.gz", "forward.fastq.gz", example_val))
    } else if (grepl("reverse|rev", param_lower)) {
      return(gsub("reads\\.fastq\\.gz", "reverse.fastq.gz", example_val))
    }
  }

  # Pattern matching for BAM files
  if (datatype == "bam") {
    if (grepl("input1|primary", param_lower)) {
      return(gsub("alignment\\.bam", "input1.bam", example_val))
    } else if (grepl("input2|secondary", param_lower)) {
      return(gsub("alignment\\.bam", "input2.bam", example_val))
    } else if (grepl("sorted", param_lower)) {
      return(gsub("alignment\\.bam", "sorted.bam", example_val))
    }
  }

  # Pattern matching for output files
  if (input_output == "output") {
    if (grepl("output1|out1|result1", param_lower)) {
      base_name <- basename(example_val)
      dir_name <- dirname(example_val)
      base_name <- gsub("^([^.]+)", "\\1_1", base_name)
      return(file.path(dir_name, base_name))
    } else if (grepl("output2|out2|result2", param_lower)) {
      base_name <- basename(example_val)
      dir_name <- dirname(example_val)
      base_name <- gsub("^([^.]+)", "\\1_2", base_name)
      return(file.path(dir_name, base_name))
    } else if (grepl("log|report", param_lower) && datatype %in% c("txt", "html", "json")) {
      return(gsub("config\\.", "report.", example_val))
    }
  }

  # Pattern matching for FASTA files
  if (datatype == "fasta") {
    if (grepl("reference|ref|genome", param_lower)) {
      return(gsub("genome\\.fa", "reference.fa", example_val))
    } else if (grepl("query|input", param_lower)) {
      return(gsub("genome\\.fa", "query.fa", example_val))
    }
  }

  # Pattern matching for index files
  if (datatype %in% c("index", "prefix")) {
    if (grepl("index|idx", param_lower)) {
      return(gsub("prefix", "index", example_val))
    } else if (grepl("reference|ref", param_lower)) {
      return(gsub("prefix", "reference", example_val))
    }
  }

  # For numbered parameters (general case)
  if (grepl("1$", param_name)) {
    base_name <- basename(example_val)
    dir_name <- dirname(example_val)
    base_name <- gsub("^([^.]+)", "\\1_1", base_name)
    return(file.path(dir_name, base_name))
  } else if (grepl("2$", param_name)) {
    base_name <- basename(example_val)
    dir_name <- dirname(example_val)
    base_name <- gsub("^([^.]+)", "\\1_2", base_name)
    return(file.path(dir_name, base_name))
  }

  # Default: return original value
  return(example_val)
}
