#' Run Tool Command
#'
#' This is the main unified API function for executing bioinformatics tools
#' registered via YAML configurations. It handles parameter validation, template
#' rendering, and command execution.
#'
#' @param tool Character. Tool name from registry.
#' @param command Character. Command name within the tool.
#' @param ... Named parameters for the tool command.
#' @param dry_run Logical. If TRUE, only show the command without executing.
#' @param work_dir Character. Working directory for execution.
#' @param env_path Character. Path to conda environment (optional).
#' @param extra Character. Additional command-line arguments.
#' @param registry List. Tool registry (optional, will load if not provided).
#' @param log_level Character or Integer. Logging level: "silent"/"quiet"/0 (no
#'   output), "minimal"/1 (R messages only), "normal"/2 (R messages + tool
#'   output). Default can be set via options("sn.log_level").
#' @param log_dir Character. Directory to save log files. If NULL, uses output
#'   directory or work_dir. Default can be set via options("sn.log_dir").
#'
#' @return List containing execution results (exit_code, stdout, stderr,
#'   command).
#' @export
#'
#' @examples
#' \dontrun{
#' # Set global options
#' options(sn.log_level = "normal", sn.log_dir = "~/logs")
#'
#' # Build HISAT2 index (logs saved to output directory)
#' sn_run("hisat2", "build",
#'   fasta = "genome.fa",
#'   index = "genome_index",
#'   threads = 8
#' )
#'
#' # Align reads with HISAT2 (logs saved to custom directory)
#' sn_run("hisat2", "align",
#'   index = "genome_index",
#'   reads1 = "sample_R1.fastq.gz",
#'   reads2 = "sample_R2.fastq.gz",
#'   bam = "aligned.bam",
#'   threads = 4,
#'   log_dir = "~/analysis_logs"
#' )
#'
#' # Dry run to see command
#' sn_run("samtools", "index",
#'   input = "aligned.bam",
#'   output = "aligned.bam.bai",
#'   dry_run = TRUE
#' )
#'
#' # Silent mode (no output but still saves logs)
#' sn_run("samtools", "faidx",
#'   fasta = "genome.fa",
#'   region = "chr1",
#'   log_level = 0
#' ) # or "silent"
#'
#' # Use numeric log levels
#' sn_run("samtools", "index",
#'   input = "aligned.bam",
#'   log_level = 2
#' ) # normal
#' }
sn_run <- function(tool, command, ...,
                   dry_run = FALSE,
                   work_dir = ".",
                   env_path = NULL,
                   extra = NULL,
                   registry = NULL,
                   log_level = getOption("sn.log_level", "normal"),
                   log_dir = getOption("sn.log_dir", NULL)) {
  # Normalize log_level (support both string and numeric)
  log_level <- .sn_normalize_log_level(log_level)

  # Set verbosity based on log_level
  show_messages <- log_level >= 1 # minimal or normal
  show_tool_output <- log_level >= 2 # normal

  # Load registry if not provided
  if (is.null(registry)) {
    registry <- sn_load_registry()
  }

  # Get tool configuration
  tool_config <- sn_get_tool_config(tool, registry)

  # Ensure tool is available (auto-install if needed)
  if (!sn_ensure_tool(tool)) {
    cli_abort("Tool '{tool}' is not available and could not be installed")
  }

  # Validate command exists
  if (!command %in% names(tool_config$commands)) {
    available_commands <- paste(names(tool_config$commands), collapse = ", ")
    cli_abort("Command '{command}' not found for tool '{tool}'. Available commands: {available_commands}")
  }

  cmd_config <- tool_config$commands[[command]]

  # Collect user parameters
  user_params <- list(...)

  # Add extra if provided
  if (!is.null(extra) && extra != "") {
    user_params$extra <- extra
  }

  # Display modern workflow-style information
  if (show_messages) {
    .sn_display_workflow_info(tool, command, cmd_config, user_params)
  }

  # Render command template
  rendered_cmd <- sn_render_template(
    template = cmd_config$args_template,
    params = user_params,
    inputs = cmd_config$inputs,
    cmd_config = cmd_config
  )

  # Create output directories before execution
  .sn_create_output_directories(cmd_config, user_params, show_messages)

  # Prepare execution environment
  if (is.null(env_path)) {
    # Collect all environments (main tool + dependencies)
    env_paths <- .sn_collect_tool_environments(tool, tool_config, registry)
  } else {
    # User provided specific environment path
    env_paths <- list(env_path)
  }

  if (show_messages || dry_run) {
    # Show the environment information on separate line if available
    if (length(env_paths) > 0) {
      valid_paths <- Filter(function(x) !is.null(x) && dir.exists(x), env_paths)
      if (length(valid_paths) > 0) {
        if (length(valid_paths) == 1) {
          cli_bullets(c("i" = "Environment: {.path {valid_paths[[1]]}}"))
        } else {
          cli_bullets(c("i" = "Environments:"))
          for (i in seq_along(valid_paths)) {
            cli_bullets(c(" " = "  {i}. {.path {valid_paths[[i]]}}"))
          }
        }
      }
    }
    # Show the command on its own line for better readability
    cli_bullets(c("i" = "Command:"))
    cli_bullets(c(" " = "{.code {rendered_cmd}}"))
  }

  # Prepare log file (always save logs)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- sprintf("%s_%s_%s.log", timestamp, tool, command)

  # Determine log directory
  final_log_dir <- .sn_determine_log_dir(
    log_dir, user_params, cmd_config, work_dir
  )
  log_file <- file.path(final_log_dir, log_file)

  # Show logging message if displaying messages
  if (show_messages) {
    cli_bullets(c("i" = "Logging to: {.file {log_file}}"))
  }

  # Execute or dry run
  if (dry_run) {
    if (show_messages) {
      cli_bullets(c("!" = "Dry run mode - command not executed"))
    }

    # Create S3 object for dry run result
    result <- .sn_create_result_object(
      tool = tool,
      command = command,
      cmd_config = cmd_config,
      user_params = user_params,
      rendered_cmd = rendered_cmd,
      dry_run = TRUE,
      exit_code = NULL,
      stdout = NULL,
      stderr = NULL,
      log_file = log_file,
      env_paths = env_paths,
      resources = NULL
    )

    return(invisible(result))
  }

  # Execute command with resource monitoring
  if (show_messages) {
    cli_bullets(c(">" = "Executing..."))
  }

  # Execute command with resource monitoring
  start_time <- Sys.time()
  result <- .sn_execute_command_with_monitoring(rendered_cmd, work_dir, show_tool_output, log_file, start_time, env_paths)
  end_time <- Sys.time()

  # Create resources info
  resources <- list(
    duration = as.numeric(difftime(end_time, start_time, units = "secs")),
    peak_cpu = result$peak_cpu,
    peak_memory = result$peak_memory
  )

  # Update log file with resource information
  if (!is.null(log_file)) {
    .sn_append_resource_info(log_file, end_time, resources)
  }

  # Report results
  if (show_messages) {
    if (result$exit_code == 0) {
      duration_str <- .sn_format_duration(resources$duration)
      cli_bullets(c("v" = "Completed successfully in {.emph {duration_str}}"))
      if (!is.null(resources$peak_memory)) {
        memory_str <- .sn_format_memory(resources$peak_memory)
        cli_bullets(c("i" = "Peak memory: {.emph {memory_str}}"))
      }
      if (!is.null(resources$peak_cpu)) {
        cli_bullets(
          c(
            "i" = "Peak CPU: {.emph {sprintf('%.1f%%', resources$peak_cpu)}}"
          )
        )
      }
    } else {
      cli_bullets(c("x" = "Failed with exit code: {.emph {result$exit_code}}"))
      if (!show_tool_output && nzchar(result$stderr)) {
        cli_bullets(c("!" = "Error: {.emph {result$stderr}}"))
      }
    }
  }

  # Create S3 object for execution result
  result_obj <- .sn_create_result_object(
    tool = tool,
    command = command,
    cmd_config = cmd_config,
    user_params = user_params,
    rendered_cmd = rendered_cmd,
    dry_run = FALSE,
    exit_code = result$exit_code,
    stdout = result$stdout,
    stderr = result$stderr,
    log_file = log_file,
    env_paths = env_paths,
    resources = resources
  )

  invisible(result_obj)
}

#' Display Workflow Information
#'
#' Internal function to display modern workflow-style information about
#' inputs/outputs/parameters.
#'
#' @param tool Character. Tool name.
#' @param command Character. Command name.
#' @param cmd_config List. Command configuration.
#' @param user_params List. User provided parameters.
#'
#' @keywords internal
.sn_display_workflow_info <- function(tool, command, cmd_config, user_params) {
  cli_rule(left = paste0("\U1F527 ", tool, "::", command))

  # Display inputs (required parameters from inputs section)
  required_inputs <- .sn_extract_required_inputs(cmd_config$inputs, user_params)
  if (length(required_inputs) > 0) {
    cli_bullets(c(" " = "{.strong Inputs:}"))
    for (name in names(required_inputs)) {
      value <- required_inputs[[name]]
      param_def <- cmd_config$inputs[[name]]
      if (!is.null(param_def$type) && param_def$type == "file") {
        # Display file with size info
        if (file.exists(value)) {
          size <- .sn_format_file_size(file.size(value))
          cli_bullets(c("*" = "{.field {name}}: {.file {basename(value)}} ({.emph {size}}) - {.path {dirname(value)}}"))
        } else {
          cli_bullets(c("!" = "{.field {name}}: {.file {basename(value)}} {.emph (not found)} - {.path {dirname(value)}}"))
        }
      } else {
        cli_bullets(c("*" = "{.field {name}}: {.val {value}}"))
      }
    }
    cli_text() # Add spacing
  }

  # Display outputs
  if (!is.null(cmd_config$outputs) && length(cmd_config$outputs) > 0) {
    output_files <- .sn_extract_user_params(cmd_config$outputs, user_params)
    if (length(output_files) > 0) {
      cli_bullets(c(" " = "{.strong Outputs:}"))
      for (name in names(output_files)) {
        value <- output_files[[name]]
        cli_bullets(c("*" = "{.field {name}}: {.file {basename(value)}} - {.path {dirname(value)}}"))
      }
      cli_text() # Add spacing
    }
  }

  # Display parameters (optional inputs + other parameters)
  params <- .sn_extract_parameters(cmd_config, user_params)
  if (length(params) > 0) {
    cli_bullets(c(" " = "{.strong Parameters:}"))
    for (name in names(params)) {
      value <- params[[name]]
      cli_bullets(c("*" = "{.field {name}}: {.val {value}}"))
    }
  }

  cli_rule()
}

#' Extract User Parameters from Config Section
#'
#' @param config_section List. Input or output definitions.
#' @param user_params List. User parameters.
#'
#' @return List of parameters that match the config section.
#' @keywords internal
.sn_extract_user_params <- function(config_section, user_params) {
  result <- list()
  if (!is.null(config_section)) {
    for (param_name in names(config_section)) {
      if (param_name %in% names(user_params)) {
        result[[param_name]] <- user_params[[param_name]]
      }
    }
  }
  return(result)
}

#' Extract Required Inputs
#'
#' @param inputs List. Input definitions.
#' @param user_params List. User parameters.
#'
#' @return List of required input parameters.
#' @keywords internal
.sn_extract_required_inputs <- function(inputs, user_params) {
  result <- list()
  if (!is.null(inputs)) {
    for (param_name in names(inputs)) {
      param_def <- inputs[[param_name]]
      # Check if parameter is required and user provided it
      if ((param_def$required %||% FALSE) && param_name %in% names(user_params)) {
        result[[param_name]] <- user_params[[param_name]]
      }
    }
  }
  return(result)
}

#' Extract Parameters (optional inputs + others)
#'
#' @param cmd_config List. Command configuration.
#' @param user_params List. User parameters.
#'
#' @return List of parameter values.
#' @keywords internal
.sn_extract_parameters <- function(cmd_config, user_params) {
  result <- list()

  # Get optional inputs (non-required inputs)
  if (!is.null(cmd_config$inputs)) {
    for (param_name in names(cmd_config$inputs)) {
      param_def <- cmd_config$inputs[[param_name]]
      # Include if not required and user provided it
      if (!(param_def$required %||% FALSE) && param_name %in% names(user_params)) {
        result[[param_name]] <- user_params[[param_name]]
      }
    }
  }

  # Get other parameters not in inputs or outputs
  input_names <- if (!is.null(cmd_config$inputs)) names(cmd_config$inputs) else character(0)
  output_names <- if (!is.null(cmd_config$outputs)) names(cmd_config$outputs) else character(0)
  exclude_names <- c(input_names, output_names, "extra")

  for (param_name in names(user_params)) {
    if (!param_name %in% exclude_names) {
      result[[param_name]] <- user_params[[param_name]]
    }
  }

  return(result)
}

#' Find Tool Environment Path
#'
#' Internal function to locate the conda environment for an installed tool.
#'
#' @param tool Character. Tool name.
#' @param tool_config List. Tool configuration.
#'
#' @return Character. Path to environment or NULL if not found.
#' @keywords internal
.sn_find_tool_env <- function(tool, tool_config) {
  # Default base directory for tool installations
  base_dir <- tools::R_user_dir(package = "shennong-tools", which = "data")

  # Check if tool directory exists
  tool_dir <- file.path(base_dir, tool)
  if (!dir.exists(tool_dir)) {
    cli_warn("Tool '{tool}' not found in {base_dir}. Use sn_install_tool() first or provide env_path.")
    return(NULL)
  }

  # Look for version subdirectories
  versions <- list.dirs(tool_dir, full.names = FALSE, recursive = FALSE)
  if (length(versions) == 0) {
    return(NULL)
  }

  # Use the most recent version
  latest_version <- versions[order(versions, decreasing = TRUE)][1]
  env_path <- file.path(tool_dir, latest_version)

  # Verify environment is valid
  binary_name <- tool_config$commands[[1]]$binary %||% tool_config$package$name
  binary_path <- file.path(env_path, "bin", binary_name)

  if (!file.exists(binary_path)) {
    cli_warn("Binary '{binary_name}' not found in environment {env_path}")
    return(NULL)
  }

  return(env_path)
}

#' Collect Tool Environments
#'
#' Internal function to collect environment paths for a tool and all its
#' dependencies. This ensures that when running a tool that depends on other
#' tools (e.g., hisat2 + samtools), all necessary environments are available in
#' PATH.
#'
#' @param tool Character. Tool name.
#' @param tool_config List. Tool configuration.
#' @param registry List. Tool registry.
#'
#' @return List. Environment paths for tool and its dependencies.
#' @keywords internal
.sn_collect_tool_environments <- function(tool, tool_config, registry) {
  env_paths <- list()

  # Get environment for the main tool
  main_env <- .sn_find_tool_env(tool, tool_config)
  if (!is.null(main_env)) {
    env_paths <- append(env_paths, main_env)
  }

  # Process dependencies from requires field
  if (!is.null(tool_config$package$requires)) {
    for (dep in tool_config$package$requires) {
      dep_name <- dep$name

      # Get dependency tool config from registry
      if (dep_name %in% names(registry)) {
        dep_config <- registry[[dep_name]]
        dep_env <- .sn_find_tool_env(dep_name, dep_config)

        if (!is.null(dep_env)) {
          env_paths <- append(env_paths, dep_env)
        } else {
          cli_warn("Dependency '{dep_name}' environment not found. Tool '{tool}' may not work correctly.")
        }
      } else {
        cli_warn("Dependency '{dep_name}' not found in registry. Tool '{tool}' may not work correctly.")
      }
    }
  }

  # Remove duplicates while preserving order (main tool first)
  env_paths <- env_paths[!duplicated(env_paths)]

  return(env_paths)
}

#' Execute Command with Resource Monitoring
#'
#' Internal function to execute a shell command with CPU and memory monitoring
#' using the ps package.
#'
#' @param command Character. Command to execute.
#' @param work_dir Character. Working directory.
#' @param show_logs Logical. Whether to show stdout/stderr in real time.
#' @param log_file Character. Full path to log file (optional).
#' @param start_time POSIXct. Start time for logging.
#' @param env_paths List. List of environment paths.
#'
#' @return List with exit_code, stdout, stderr, peak_cpu, peak_memory.
#' @keywords internal
.sn_execute_command_with_monitoring <- function(
    command, work_dir, show_logs = TRUE,
    log_file = NULL, start_time = NULL, env_paths = NULL) {
  # Change to working directory if specified
  old_wd <- getwd()
  if (work_dir != ".") {
    if (!dir.exists(work_dir)) {
      dir.create(work_dir, recursive = TRUE)
    }
    setwd(work_dir)
  }

  on.exit({
    setwd(old_wd)
  })

  # Prepare log file if specified
  if (!is.null(log_file)) {
    # Ensure log directory exists
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }

    log_start_time <- if (is.null(start_time)) Sys.time() else start_time
    writeLines(c(
      paste("# Command execution log"),
      paste("# Started at:", log_start_time),
      paste("# Command:", command),
      paste("# Working directory:", work_dir),
      ""
    ), log_file)
  }

  # Check if ps package is available for resource monitoring
  # # if (!requireNamespace("ps", quietly = TRUE)) {
  # if (TRUE) {
  #   # Fall back to simple execution without monitoring
  #   return(.sn_execute_command_simple(command, work_dir, show_logs, log_file, start_time, env_paths))
  # }

  # Prepare environment variables for conda environment
  env_vars <- Sys.getenv()
  if (!is.null(env_paths) && length(env_paths) > 0) {
    for (env_path in env_paths) {
      if (dir.exists(env_path)) {
        # Add conda environment bin to PATH
        env_bin <- file.path(env_path, "bin")
        if (dir.exists(env_bin)) {
          current_path <- env_vars[["PATH"]] %||% ""
          env_vars[["PATH"]] <- paste(env_bin, current_path, sep = ":")
        }
      }
    }
  }

  # Execute command with resource monitoring using GNU time
  tryCatch(
    {
      # Create temporary file for time output
      time_output_file <- tempfile()

      # Use GNU time to monitor resources
      time_cmd <- sprintf(
        "/usr/bin/time -f 'TIMESTAT:%%e:%%U:%%S:%%M:%%P' -o %s %s",
        shQuote(time_output_file), command
      )

      # Execute command using processx
      result <- processx::run(
        command = "sh",
        args = c("-c", time_cmd),
        stdout = "|",
        stderr = "|",
        env = env_vars,
        echo_cmd = FALSE,
        echo = show_logs,
        error_on_status = FALSE
      )

      exit_code <- result$status
      stdout_result <- result$stdout
      stderr_result <- result$stderr

      # Parse time output for resource information
      peak_cpu <- NULL
      peak_memory <- NULL

      if (file.exists(time_output_file)) {
        tryCatch(
          {
            time_output <- readLines(time_output_file, warn = FALSE)
            for (line in time_output) {
              if (startsWith(line, "TIMESTAT:")) {
                parts <- strsplit(line, ":")[[1]]
                if (length(parts) >= 6) {
                  # Format: TIMESTAT:elapsed:user:system:maxrss:cpu_percent
                  user_time <- as.numeric(parts[3])
                  system_time <- as.numeric(parts[4])
                  max_rss_kb <- as.numeric(parts[5])
                  cpu_percent_str <- parts[6]

                  # Convert memory from KB to bytes
                  if (!is.na(max_rss_kb) && max_rss_kb > 0) {
                    peak_memory <- max_rss_kb * 1024
                  }

                  # Parse CPU percentage (remove % sign if present)
                  if (!is.na(cpu_percent_str) && nzchar(cpu_percent_str)) {
                    cpu_percent_clean <- gsub("%", "", cpu_percent_str)
                    cpu_percent_num <- as.numeric(cpu_percent_clean)
                    if (!is.na(cpu_percent_num) && cpu_percent_num > 0) {
                      peak_cpu <- cpu_percent_num
                    }
                  }
                }
              }
            }
            unlink(time_output_file)
          },
          error = function(e) {
            # Failed to parse time output, continue without resource info
          }
        )
      }

      # Log output if log file specified
      if (!is.null(log_file)) {
        log_lines <- c(
          paste("# Exit code:", exit_code),
          ""
        )

        if (nzchar(stdout_result)) {
          log_lines <- c(log_lines, strsplit(stdout_result, "\n")[[1]])
        }

        if (nzchar(stderr_result)) {
          log_lines <- c(log_lines, "", "# STDERR:", strsplit(stderr_result, "\n")[[1]])
        }

        cat(paste(log_lines, collapse = "\n"), "\n", file = log_file, append = TRUE)
      }

      return(list(
        exit_code = exit_code,
        stdout = stdout_result,
        stderr = stderr_result,
        peak_cpu = peak_cpu,
        peak_memory = peak_memory
      ))
    },
    error = function(e) {
      error_msg <- as.character(e)

      # Log error if log file is specified
      if (!is.null(log_file)) {
        cat(c(
          "",
          paste("# ERROR occurred at:", Sys.time()),
          paste("# Error message:", error_msg),
          ""
        ), sep = "\n", file = log_file, append = TRUE)
      }

      list(
        exit_code = 1,
        stdout = "",
        stderr = error_msg,
        peak_cpu = NULL,
        peak_memory = NULL
      )
    }
  )
}

#' Execute Command (Simple, No Monitoring)
#'
#' Internal function to execute a shell command without resource monitoring.
#' This is used as a fallback when ps package is not available.
#'
#' @param command Character. Command to execute.
#' @param work_dir Character. Working directory.
#' @param show_logs Logical. Whether to show stdout/stderr in real time.
#' @param log_file Character. Full path to log file (optional).
#' @param start_time POSIXct. Start time for logging.
#' @param env_paths List. List of environment paths.
#'
#' @return List with exit_code, stdout, stderr.
#' @keywords internal
.sn_execute_command_simple <- function(
    command, work_dir, show_logs = TRUE, log_file = NULL,
    start_time = NULL, env_paths = NULL) {
  # Change to working directory if specified
  old_wd <- getwd()
  if (work_dir != ".") {
    if (!dir.exists(work_dir)) {
      dir.create(work_dir, recursive = TRUE)
    }
    setwd(work_dir)
  }

  on.exit({
    setwd(old_wd)
  })

  # Prepare log file if specified
  if (!is.null(log_file)) {
    # Ensure log directory exists
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }

    log_start_time <- if (is.null(start_time)) Sys.time() else start_time
    writeLines(c(
      paste("# Command execution log"),
      paste("# Started at:", log_start_time),
      paste("# Command:", command),
      paste("# Working directory:", work_dir),
      ""
    ), log_file)
  }

  # Prepare environment variables for conda environment
  env_vars <- Sys.getenv()
  if (!is.null(env_paths) && length(env_paths) > 0) {
    for (env_path in env_paths) {
      if (dir.exists(env_path)) {
        # Add conda environment bin to PATH
        env_bin <- file.path(env_path, "bin")
        if (dir.exists(env_bin)) {
          current_path <- env_vars[["PATH"]] %||% ""
          env_vars[["PATH"]] <- paste(env_bin, current_path, sep = ":")
        }
      }
    }
  }

  # Execute command using processx
  tryCatch(
    {
      result <- processx::run(
        command = "sh",
        args = c("-c", command),
        stdout = "|",
        stderr = "|",
        env = env_vars,
        echo_cmd = FALSE,
        echo = show_logs,
        error_on_status = FALSE
      )

      exit_code <- result$status

      # Log output if log file specified
      if (!is.null(log_file)) {
        log_lines <- c(
          paste("# Exit code:", exit_code),
          "",
          if (nzchar(result$stdout)) strsplit(result$stdout, "\n")[[1]] else "",
          if (nzchar(result$stderr)) c("", "# STDERR:", strsplit(result$stderr, "\n")[[1]]) else ""
        )
        cat(paste(log_lines, collapse = "\n"), "\n", file = log_file, append = TRUE)
      }

      return(list(
        exit_code = exit_code,
        stdout = result$stdout,
        stderr = result$stderr,
        peak_cpu = NULL,
        peak_memory = NULL
      ))
    },
    error = function(e) {
      error_msg <- as.character(e)

      # Log error if log file is specified
      if (!is.null(log_file)) {
        cat(c(
          "",
          paste("# ERROR occurred at:", Sys.time()),
          paste("# Error message:", error_msg),
          ""
        ), sep = "\n", file = log_file, append = TRUE)
      }

      list(
        exit_code = 1,
        stdout = "",
        stderr = error_msg,
        peak_cpu = NULL,
        peak_memory = NULL
      )
    }
  )
}

#' Append Resource Information to Log
#'
#' @param log_file Character. Path to log file.
#' @param end_time POSIXct. End time.
#' @param resources List. Resource usage information.
#'
#' @keywords internal
.sn_append_resource_info <- function(log_file, end_time, resources) {
  tryCatch(
    {
      if (file.exists(log_file)) {
        additional_info <- c(
          "",
          paste("# Completed at:", end_time),
          paste("# Duration:", sprintf("%.2f seconds", resources$duration))
        )

        if (!is.null(resources$peak_cpu)) {
          additional_info <- c(
            additional_info,
            paste("# Peak CPU:", sprintf("%.1f%%", resources$peak_cpu))
          )
        }

        if (!is.null(resources$peak_memory)) {
          additional_info <- c(
            additional_info,
            paste("# Peak Memory:", .sn_format_memory(resources$peak_memory))
          )
        }

        cat(paste(additional_info, collapse = "\n"), "\n", file = log_file, append = TRUE)
      }
    },
    error = function(e) {
      # Silently fail if logging doesn't work
    }
  )
}

#' Normalize Log Level
#'
#' @param log_level Character or Integer. Logging level.
#'
#' @return Integer. Normalized log level.
#' @keywords internal
.sn_normalize_log_level <- function(log_level) {
  if (is.character(log_level)) {
    # Map string to numeric: silent/quiet=0, minimal=1, normal=2
    level_map <- c("silent" = 0, "quiet" = 0, "minimal" = 1, "normal" = 2)
    if (!log_level %in% names(level_map)) {
      valid_levels <- names(level_map)
      cli_abort("Invalid log_level. Must be one of: {paste(valid_levels, collapse = ', ')}")
    }
    return(level_map[[log_level]])
  } else if (is.numeric(log_level)) {
    if (log_level < 0 || log_level > 2) {
      cli_abort("Invalid log_level. Must be between 0 and 2 (0=silent, 1=minimal, 2=normal)")
    }
    return(as.integer(log_level))
  } else {
    cli_abort("Invalid log_level format. Must be character or numeric")
  }
}

#' Determine Log Directory
#'
#' @param log_dir Character. User specified log directory.
#' @param user_params List. User parameters.
#' @param cmd_config List. Command configuration.
#' @param work_dir Character. Working directory.
#'
#' @return Character. Final log directory path.
#' @keywords internal
.sn_determine_log_dir <- function(log_dir, user_params, cmd_config, work_dir) {
  # If user specified log_dir, use it
  if (!is.null(log_dir)) {
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }
    return(log_dir)
  }

  # Try to find output directory from first output file
  outputs <- cmd_config$outputs %||% list()
  for (output_name in names(outputs)) {
    if (output_name %in% names(user_params)) {
      output_path <- user_params[[output_name]]
      if (is.character(output_path) && length(output_path) == 1) {
        output_dir <- dirname(output_path)
        if (output_dir != "." && dir.exists(output_dir)) {
          return(output_dir)
        }
      }
    }
  }

  # Fall back to working directory
  return(work_dir)
}

#' Create Output Directories
#'
#' Internal function to create directories for all output files before execution.
#'
#' @param cmd_config List. Command configuration.
#' @param user_params List. User provided parameters.
#' @param show_messages Logical. Whether to show creation messages.
#'
#' @keywords internal
.sn_create_output_directories <- function(cmd_config, user_params, show_messages = TRUE) {
  if (is.null(cmd_config$outputs) || length(cmd_config$outputs) == 0) {
    return(invisible())
  }

  created_dirs <- character(0)

  # Check all output parameters
  for (output_name in names(cmd_config$outputs)) {
    if (output_name %in% names(user_params)) {
      output_path <- user_params[[output_name]]

      if (is.character(output_path) && length(output_path) == 1 && nzchar(output_path)) {
        output_dir <- dirname(output_path)

        # Skip if directory is current directory or already exists
        if (output_dir != "." && !dir.exists(output_dir)) {
          tryCatch(
            {
              dir.create(output_dir, recursive = TRUE)
              created_dirs <- c(created_dirs, output_dir)
            },
            error = function(e) {
              cli_warn("Failed to create output directory '{output_dir}': {e$message}")
            }
          )
        }
      }
    }
  }

  # Show creation message if directories were created
  if (length(created_dirs) > 0 && show_messages) {
    unique_dirs <- unique(created_dirs)
    if (length(unique_dirs) == 1) {
      cli_bullets(c("v" = "Created output directory: {.path {unique_dirs[1]}}"))
    } else {
      cli_bullets(c("v" = "Created output directories: {.path {unique_dirs}}"))
    }
  }

  invisible(created_dirs)
}

# S3 Object for Tool Execution Results ----------------------------------------

#' Create Tool Execution Result Object
#'
#' Internal function to create an S3 object representing the result of a tool execution.
#'
#' @param tool Character. Tool name.
#' @param command Character. Command name.
#' @param cmd_config List. Command configuration.
#' @param user_params List. User provided parameters.
#' @param rendered_cmd Character. The rendered command string.
#' @param dry_run Logical. Whether this was a dry run.
#' @param exit_code Numeric. Exit code from execution.
#' @param stdout Character. Standard output.
#' @param stderr Character. Standard error.
#' @param log_file Character. Path to log file.
#' @param env_paths List. Environment paths.
#' @param resources List. Resource usage information.
#'
#' @return S3 object of class "sn_result".
#' @keywords internal
.sn_create_result_object <- function(tool, command, cmd_config, user_params, rendered_cmd,
                                     dry_run, exit_code, stdout, stderr, log_file, env_paths, resources) {
  # Extract input, output, and parameter information
  inputs <- .sn_extract_required_inputs(cmd_config$inputs, user_params)
  outputs <- .sn_extract_user_params(cmd_config$outputs, user_params)
  parameters <- .sn_extract_parameters(cmd_config, user_params)

  result <- list(
    # Tool execution metadata
    tool = tool,
    command = command,
    command_line = rendered_cmd,
    dry_run = dry_run,

    # Execution results
    exit_code = exit_code,
    stdout = stdout,
    stderr = stderr,
    success = if (dry_run) NA else (exit_code == 0),

    # Environment and logging
    env_paths = env_paths,
    log_file = log_file,

    # Resource usage
    resources = resources,

    # User parameters organized by type
    inputs = inputs,
    outputs = outputs,
    parameters = parameters,

    # Full user parameters for reference
    user_params = user_params,

    # Timestamps
    timestamp = Sys.time()
  )

  class(result) <- "sn_result"
  return(result)
}

#' Print Method for Tool Execution Results
#'
#' @param x An sn_result object.
#' @param ... Additional arguments (ignored).
#'
#' @export
print.sn_result <- function(x, ...) {
  # Header with tool and command
  if (x$dry_run) {
    cli_rule(left = paste0("\U1F50D Dry Run: ", x$tool, "::", x$command))
  } else if (x$success) {
    cli_rule(left = paste0("\u2705 Success: ", x$tool, "::", x$command))
  } else {
    cli_rule(left = paste0("\u274C Failed: ", x$tool, "::", x$command))
  }

  # Display inputs if any
  if (length(x$inputs) > 0) {
    cli_bullets(c(" " = "{.strong Inputs:}"))
    for (name in names(x$inputs)) {
      value <- x$inputs[[name]]
      # Check if it's a file path and show file info
      if (is.character(value) && length(value) == 1 && file.exists(value)) {
        size <- .sn_format_file_size(file.size(value))
        cli_bullets(c("*" = "{.field {name}}: {.file {basename(value)}} ({.emph {size}}) - {.path {dirname(value)}}"))
      } else {
        cli_bullets(c("*" = "{.field {name}}: {.val {value}}"))
      }
    }
    cli_text()
  }

  # Display outputs if any
  if (length(x$outputs) > 0) {
    cli_bullets(c(" " = "{.strong Outputs:}"))
    for (name in names(x$outputs)) {
      value <- x$outputs[[name]]
      # Check if output file exists and show file info
      if (is.character(value) && length(value) == 1) {
        if (file.exists(value)) {
          size <- .sn_format_file_size(file.size(value))
          cli_bullets(c("*" = "{.field {name}}: {.file {basename(value)}} ({.emph {size}}) - {.path {dirname(value)}}"))
        } else {
          cli_bullets(c("!" = "{.field {name}}: {.file {basename(value)}} {.emph (not created)} - {.path {dirname(value)}}"))
        }
      } else {
        cli_bullets(c("*" = "{.field {name}}: {.val {value}}"))
      }
    }
    cli_text()
  }

  # Display parameters if any
  if (length(x$parameters) > 0) {
    cli_bullets(c(" " = "{.strong Parameters:}"))
    for (name in names(x$parameters)) {
      value <- x$parameters[[name]]
      cli_bullets(c("*" = "{.field {name}}: {.val {value}}"))
    }
    cli_text()
  }

  # Display execution details
  cli_bullets(c(" " = "{.strong Execution:}"))

  # Environment
  if (length(x$env_paths) > 0) {
    cli_bullets(c("*" = "Environments:"))
    for (i in seq_along(x$env_paths)) {
      cli_bullets(c(" " = "  {i}. {.path {x$env_paths[[i]]}}"))
    }
  }

  # Command
  cli_bullets(c("*" = "Command: {.code {x$command_line}}"))

  # Status
  if (x$dry_run) {
    cli_bullets(c("*" = "Status: {.emph Dry run (not executed)}"))
  } else if (x$success) {
    cli_bullets(c("*" = "Status: {.strong {.val Success}} (exit code {x$exit_code})"))
  } else {
    cli_bullets(c("*" = "Status: {.strong {.val Failed}} (exit code {x$exit_code})"))
  }

  # Resources (if execution completed)
  if (!x$dry_run && !is.null(x$resources)) {
    if (!is.null(x$resources$duration)) {
      duration_str <- .sn_format_duration(x$resources$duration)
      cli_bullets(c("*" = "Duration: {.emph {duration_str}}"))
    }
    if (!is.null(x$resources$peak_memory)) {
      memory_str <- .sn_format_memory(x$resources$peak_memory)
      cli_bullets(c("*" = "Peak Memory: {.emph {memory_str}}"))
    }
    if (!is.null(x$resources$peak_cpu)) {
      cli_bullets(c("*" = "Peak CPU: {.emph {sprintf('%.1f%%', x$resources$peak_cpu)}}"))
    }
  }

  # Log file
  if (!is.null(x$log_file) && nzchar(x$log_file)) {
    cli_bullets(c("*" = "Log: {.file {x$log_file}}"))
  }

  cli_rule()

  invisible(x)
}

#' Summary Method for Tool Execution Results
#'
#' @param object An sn_result object.
#' @param ... Additional arguments (ignored).
#'
#' @export
summary.sn_result <- function(object, ...) {
  cat("Tool Execution Summary\n")
  cat("======================\n")
  cat("Tool:", object$tool, "\n")
  cat("Command:", object$command, "\n")
  cat("Timestamp:", format(object$timestamp), "\n")

  if (object$dry_run) {
    cat("Status: Dry run\n")
  } else {
    cat("Status:", if (object$success) "Success" else "Failed", "\n")
    cat("Exit code:", object$exit_code, "\n")

    if (!is.null(object$resources$duration)) {
      cat("Duration:", .sn_format_duration(object$resources$duration), "\n")
    }
  }

  cat("Inputs:", length(object$inputs), "\n")
  cat("Outputs:", length(object$outputs), "\n")
  cat("Parameters:", length(object$parameters), "\n")

  if (!is.null(object$log_file) && nzchar(object$log_file)) {
    cat("Log file:", object$log_file, "\n")
  }

  invisible(object)
}
