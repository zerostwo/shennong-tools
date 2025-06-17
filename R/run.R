#' Run Tool Command
#'
#' Execute a bioinformatics tool command. This function automatically manages
#' the toolbox and installation of tools.
#'
#' @param tool_name Character. Name of the tool to run.
#' @param command Character. Command name within the tool.
#' @param version Character. Version of the tool to use. If NULL, uses first available version.
#' @param ... Named parameters for the tool command.
#' @param dry_run Logical. If TRUE, only show the command without executing.
#' @param work_dir Character. Working directory for execution.
#' @param log_level Character or Integer. Logging level: "silent"/"quiet"/0,
#'   "minimal"/1, "normal"/2. Default "minimal".
#' @param log_dir Character. Directory to save log files. If NULL, uses work_dir.
#'
#' @return ToolCall object containing execution results.
#' @family core functions
#' @concept core functions
#' @export
#'
#' @examples
#' \dontrun{
#' # Run tool command directly
#' result <- sn_run("samtools", "view",
#'   input = "test.bam",
#'   output = "filtered.bam",
#'   extra = "-q 20"
#' )
#'
#' # Dry run to see command
#' result <- sn_run("samtools", "index",
#'   input = "test.bam",
#'   dry_run = TRUE
#' )
#' }
sn_run <- function(tool_name, command, version = NULL, ...,
                   dry_run = FALSE,
                   work_dir = ".",
                   log_level = "minimal",
                   log_dir = NULL) {
  # Get or create default toolbox
  # TODO: change to sn_load_toolbox() ?
  toolbox <- .get_default_toolbox()

  # Get tool from toolbox
  if (!tool_name %in% sn_get_toolbox_tools(toolbox)) {
    cli_abort("Tool {tool_name} not found in toolbox. Available tools: {paste(sn_get_toolbox_tools(toolbox), collapse = ', ')}")
  }

  # Normalize work_dir to absolute path
  work_dir <- normalizePath(work_dir, mustWork = TRUE)

  # Normalize log level
  log_level <- .normalize_log_level(log_level)
  show_messages <- log_level >= 1
  show_tool_output <- log_level >= 2

  # Get tool object
  tool <- sn_get_tool(toolbox, tool_name)

  # Determine version to use
  available_versions <- sn_get_tool_versions(tool)
  if (is.null(version)) {
    version <- sn_get_tool_version(tool) # Use default version
    if (length(version) == 0 && length(available_versions) > 0) {
      version <- available_versions[1] # Fallback to first available
    }
    if (show_messages) {
      cli_alert_info("Using version {version} (specify version parameter to choose different version)")
    }
  }

  if (!version %in% available_versions) {
    cli_abort("Version {version} not found for {tool_name}. Available versions: {paste(available_versions, collapse = ', ')}")
  }

  # Check tool is installed
  if (!.is_installed(tool, version)) {
    # Try one more time to install the tool if it's not marked as installed
    if (show_messages) {
      cli_alert_info("Tool not marked as installed, attempting installation...")
    }

    temp_yaml <- tempfile(fileext = ".yaml")
    on.exit(unlink(temp_yaml), add = TRUE)
    tool@environment$dependencies <- as.list(tool@environment$dependencies)
    write_yaml(tool@environment, temp_yaml)
    env_path <- file.path(tool_name, version)
    .mamba_create_from_yaml(
      yaml_file = temp_yaml,
      env_name = env_path,
      base_dir = toolbox@base_dir,
      mamba = toolbox@mamba_path,
      overwrite = TRUE
    )
    if (show_messages) {
      cli_alert_success("Tool {tool_name} installed successfully!")
    }
  }

  # Validate command exists
  if (!command %in% sn_get_tool_commands(tool)) {
    available_commands <- paste(sn_get_tool_commands(tool), collapse = ", ")
    cli_abort("Command '{command}' not found for tool '{tool_name}'. Available commands: {available_commands}")
  }

  cmd_config <- tool@commands[[command]]

  # Collect user parameters
  user_params <- list(...)

  # Display workflow information
  if (show_messages) {
    .display_workflow_info(tool, command, cmd_config, user_params)
  }

  # Determine template type and render command
  if (!is.null(cmd_config$shell)) {
    # Shell command template
    rendered_cmd <- sn_render_template(
      template = cmd_config$shell,
      params = user_params,
      inputs = cmd_config$inputs %||% list(),
      outputs = cmd_config$outputs %||% list(),
      command_config = cmd_config,
      show_messages = show_messages
    )
    execution_type <- "shell"
  } else if (!is.null(cmd_config$python)) {
    # Python code template
    rendered_cmd <- sn_render_python_template(
      python_template = cmd_config$python,
      params = user_params,
      inputs = cmd_config$inputs %||% list(),
      outputs = cmd_config$outputs %||% list(),
      command_config = cmd_config,
      show_messages = show_messages
    )
    execution_type <- "python"
  } else {
    cli_abort("Command {command} has no shell or python template defined")
  }

  # Create output directories
  .create_output_directories(cmd_config, user_params, show_messages)

  # Show execution information - only at log_level >= 1
  if (show_messages || dry_run) {
    env_path <- .get_tool_install_path(toolbox, tool_name, version)
    cli_bullets(c("i" = "Environment: {.path {env_path}}"))
    if (execution_type == "shell") {
      cli_bullets(c("i" = "Command:"))
      cli_bullets(c(" " = "{.code {rendered_cmd}}"))
    } else {
      cli_bullets(c("i" = "Python code:"))
      cli_bullets(c(" " = "{.code {substring(rendered_cmd, 1, 100)}}..."))
    }
  }

  # Prepare log file
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file_name <- sprintf("%s_%s_%s_%s.log", timestamp, tool_name, version, command)

  # Determine log directory with smart logic
  final_log_dir <- .determine_log_directory(log_dir, work_dir, user_params, cmd_config$outputs %||% list())
  if (!dir.exists(final_log_dir)) {
    dir.create(final_log_dir, recursive = TRUE)
  }
  log_file <- file.path(final_log_dir, log_file_name)

  # Log file path will be shown after execution

  # Create ToolCall object
  tool_call <- new("ToolCall",
    tool = tool,
    command = command,
    rendered_command = rendered_cmd,
    inputs = .extract_inputs_from_params(user_params, cmd_config$inputs %||% list()),
    outputs = .extract_outputs_from_params(user_params, cmd_config$outputs %||% list()),
    params = user_params,
    work_dir = work_dir,
    log_file = log_file,
    status = "pending",
    resources = list(
      runtime_seconds = NA,
      memory_mb = NA,
      cpu_percent = NA
    )
  )

  # Execute or dry run
  if (dry_run) {
    if (show_messages) {
      cli_bullets(c("!" = "Dry run mode - command not executed"))
    }
    tool_call@status <- "pending"
    return(tool_call)
  }

  # Execute command
  if (show_messages) {
    cli_bullets(c(">" = "Executing..."))
  }

  tool_call@status <- "running"

  # Record start time
  start_time <- Sys.time()

  if (execution_type == "shell") {
    env_path <- .get_tool_install_path(toolbox, tool_name, version)
    result <- .execute_shell_command(rendered_cmd, env_path, work_dir, log_file, show_tool_output)
  } else {
    env_path <- .get_tool_install_path(toolbox, tool_name, version)
    result <- .execute_python_command(rendered_cmd, env_path, work_dir, log_file, show_tool_output)
  }

  # Update tool call with results
  tool_call@return_code <- result$exit_code
  tool_call@stdout <- result$stdout
  tool_call@stderr <- result$stderr
  tool_call@status <- if (result$exit_code == 0) "success" else "failed"

  # Add resource information
  tool_call@resources <- list(
    runtime_seconds = result$runtime_seconds %||% NA,
    start_time = start_time,
    end_time = start_time + (result$runtime_seconds %||% 0),
    memory_mb = result$memory_mb %||% NA,
    cpu_percent = result$cpu_percent %||% NA
  )

  # Show completion message
  if (show_messages) {
    if (result$exit_code == 0) {
      cli_alert_success("Command completed successfully")
    } else {
      cli_alert_danger("Command failed with exit code {result$exit_code}")
    }
    cli_bullets(c("i" = "Log saved to: {.file {log_file}}"))
  }

  invisible(tool_call)
}

# Internal helper functions ---------------------------------------------------

#' Determine Log Directory
#' @keywords internal
.determine_log_directory <- function(log_dir, work_dir, user_params, output_defs) {
  # Priority 1: Explicit log_dir parameter
  if (!is.null(log_dir)) {
    return(log_dir)
  }

  # Priority 2: Global sn_options log_dir setting
  global_log_dir <- getOption("sn.log_dir", NULL)
  if (!is.null(global_log_dir)) {
    return(global_log_dir)
  }

  # Priority 3: Output base directory (from first output)
  if (length(output_defs) > 0) {
    # Find the first output in user_params
    for (output_name in names(output_defs)) {
      if (output_name %in% names(user_params)) {
        output_path <- user_params[[output_name]]
        if (is.character(output_path) && length(output_path) == 1) {
          output_dir <- dirname(output_path)
          if (output_dir != ".") {
            return(output_dir)
          }
        }
      }
    }
  }

  # Priority 4: work_dir (fallback)
  return(work_dir)
}

#' Display Workflow Information
#' @keywords internal
.display_workflow_info <- function(tool, command, cmd_config, user_params) {
  cli_rule("Running {sn_get_tool_name(tool)}::{command}")

  if (!is.null(cmd_config$description)) {
    cli_bullets(c("i" = cmd_config$description))
  }

  # Helper to collapse vector values for display
  format_value <- function(x) {
    if (is.null(x)) {
      return("NULL")
    }
    if (is.atomic(x) && length(x) > 1) {
      x <- paste(x, collapse = " ")
    }
    if (is.character(x) && nchar(x) > 50) {
      x <- paste0(substr(x, 1, 47), "...")
    }
    x
  }

  # Extract inputs, outputs, and parameters separately
  inputs <- .extract_inputs_from_params(user_params, cmd_config$inputs %||% list())
  outputs <- .extract_outputs_from_params(user_params, cmd_config$outputs %||% list())

  # Get parameter definitions for filtering
  param_defs <- cmd_config$params %||% list()
  params <- list()
  for (param_name in names(user_params)) {
    if (!param_name %in% names(inputs) &&
      !param_name %in% names(outputs)) {
      params[[param_name]] <- user_params[[param_name]]
    }
  }

  # Display inputs
  if (length(inputs) > 0) {
    cli_h3(paste0(symbol$file, " Inputs"))
    for (input_name in names(inputs)) {
      input_value <- format_value(inputs[[input_name]])
      cli_text("  {col_cyan(input_name)}: {.path {input_value}}")
    }
  }

  # Display outputs
  if (length(outputs) > 0) {
    cli_h3(paste0(symbol$arrow_right, " Outputs"))
    for (output_name in names(outputs)) {
      output_value <- format_value(outputs[[output_name]])
      cli_text("  {col_green(output_name)}: {.path {output_value}}")
    }
  }

  # Display parameters
  if (length(params) > 0) {
    cli_h3(paste0(symbol$gear, " Parameters"))
    for (param_name in names(params)) {
      param_value <- format_value(params[[param_name]])
      cli_text("  {col_silver(param_name)}: {.val {param_value}}")
    }
  }
}

#' Create Output Directories
#' @keywords internal
.create_output_directories <- function(cmd_config, user_params, show_messages) {
  if (is.null(cmd_config$outputs)) {
    return()
  }

  dirs_created <- character(0)

  for (output_name in names(cmd_config$outputs)) {
    if (output_name %in% names(user_params)) {
      output_path <- user_params[[output_name]]
      if (is.character(output_path) && length(output_path) == 1) {
        # Determine whether this output is a directory or a file based on datatype
        output_def <- cmd_config$outputs[[output_name]]
        datatype <- output_def$datatype %||% "file"
        # YAML may specify datatype as a character vector or list; coerce to character vector
        if (is.list(datatype)) datatype <- unlist(datatype, recursive = TRUE, use.names = FALSE)
        is_directory <- any(datatype == "directory")

        target_dir <- if (is_directory) {
          # Use the path itself for directory outputs
          output_path
        } else {
          # Use the parent directory for file outputs
          dirname(output_path)
        }

        if (target_dir != "." && !dir.exists(target_dir)) {
          dir.create(target_dir, recursive = TRUE)
          dirs_created <- c(dirs_created, target_dir)
        }
      }
    }
  }

  if (length(dirs_created) > 0 && show_messages) {
    cli_bullets(c("v" = "Created output directories: {.path {unique(dirs_created)}}"))
  }
}

#' Extract Inputs from Parameters
#' @keywords internal
.extract_inputs_from_params <- function(params, input_defs) {
  inputs <- list()
  for (input_name in names(input_defs)) {
    if (input_name %in% names(params)) {
      inputs[[input_name]] <- params[[input_name]]
    }
  }
  inputs
}

#' Extract Outputs from Parameters
#' @keywords internal
.extract_outputs_from_params <- function(params, output_defs) {
  outputs <- list()
  for (output_name in names(output_defs)) {
    if (output_name %in% names(params)) {
      outputs[[output_name]] <- params[[output_name]]
    }
  }
  outputs
}

#' Execute Shell Command
#' @keywords internal
.execute_shell_command <- function(command, env_path, work_dir, log_file, show_output) {
  # Prepare environment
  env_vars <- character(0)
  if (!is.null(env_path) && dir.exists(env_path)) {
    # Add conda environment to PATH
    env_bin <- file.path(env_path, "bin")
    if (dir.exists(env_bin)) {
      current_path <- Sys.getenv("PATH")
      new_path <- paste(env_bin, current_path, sep = .Platform$path.sep)

      # Extract environment name from path for CONDA_DEFAULT_ENV
      # env_path format: /path/to/base_dir/tool_name/version
      path_parts <- strsplit(env_path, .Platform$file.sep)[[1]]
      env_name <- paste(tail(path_parts, 2), collapse = "/") # tool_name/version

      # Set conda environment variables
      env_vars <- c(
        "PATH" = new_path,
        "CONDA_PREFIX" = env_path,
        "CONDA_DEFAULT_ENV" = env_name,
        "CONDA_PROMPT_MODIFIER" = paste0("(", env_name, ") ")
      )

      # Unset potentially conflicting variables
      env_vars <- c(env_vars,
        "CONDA_PREFIX_1" = "",
        "CONDA_STACKED_2" = ""
      )
    }
  }

  # Execute command with resource monitoring
  tryCatch(
    {
      # Start timing
      start_time <- Sys.time()

      # Wrap command with /usr/bin/time for resource monitoring if available
      time_cmd <- Sys.which("time")
      if (nzchar(time_cmd) && file.exists("/usr/bin/time")) {
        # Use GNU time if available for detailed resource info
        monitored_command <- sprintf("/usr/bin/time -f 'RESOURCE_INFO: %%e %%M %%P' sh -c %s", shQuote(command))
      } else {
        # Fallback to original command without resource monitoring
        monitored_command <- command
      }

      # Execute command
      result <- processx::run(
        command = "/bin/bash",
        args = c("-c", monitored_command),
        wd = work_dir,
        env = env_vars,
        echo = show_output,
        error_on_status = FALSE,
        stdout_callback = if (show_output) NULL else function(x, ...) {},
        stderr_callback = if (show_output) NULL else function(x, ...) {}
      )

      # End timing
      end_time <- Sys.time()
      runtime_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Extract resource information from stderr if using /usr/bin/time
      max_memory_mb <- NA
      max_cpu_percent <- NA

      if (nzchar(time_cmd) && file.exists("/usr/bin/time")) {
        # Look for RESOURCE_INFO in stderr
        resource_match <- regexpr("RESOURCE_INFO: ([0-9.]+) ([0-9]+) ([0-9.%]+)", result$stderr)
        if (resource_match > 0) {
          resource_str <- regmatches(result$stderr, resource_match)
          # Extract values: time(s) memory(KB) cpu(%)
          parts <- strsplit(gsub("RESOURCE_INFO: ", "", resource_str), " ")[[1]]
          if (length(parts) >= 3) {
            # Memory is in KB, convert to MB
            max_memory_mb <- round(as.numeric(parts[2]) / 1024, 2)
            # CPU percentage - remove % sign if present
            cpu_str <- gsub("%", "", parts[3])
            max_cpu_percent <- as.numeric(cpu_str)
          }
          # Clean up the resource info from stderr
          result$stderr <- gsub("RESOURCE_INFO: [^\n]*\n?", "", result$stderr)
        }
      }

      # If we couldn't get resource info from time command, try ps as fallback
      if (is.na(max_memory_mb) && requireNamespace("ps", quietly = TRUE)) {
        tryCatch(
          {
            # This is still just an approximation using current R process
            proc_info <- ps::ps_memory_info()
            max_memory_mb <- round(proc_info$rss / 1024 / 1024, 2) # Convert bytes to MB
          },
          error = function(e) {
            max_memory_mb <- NA
          }
        )
      }

      # Write log file
      .write_log_file(log_file, command, result)

      list(
        exit_code = result$status,
        stdout = result$stdout,
        stderr = result$stderr,
        runtime_seconds = runtime_seconds,
        memory_mb = max_memory_mb,
        cpu_percent = max_cpu_percent
      )
    },
    error = function(e) {
      cli_alert_danger("Command execution failed: {e$message}")
      list(
        exit_code = 1,
        stdout = "",
        stderr = e$message,
        runtime_seconds = NA,
        memory_mb = NA,
        cpu_percent = NA
      )
    }
  )
}

#' Execute Python Command
#' @keywords internal
.execute_python_command <- function(python_code, env_path, work_dir, log_file, show_output) {
  # Write Python code to temporary file
  temp_py <- tempfile(fileext = ".py")
  writeLines(python_code, temp_py)

  # Find Python executable in environment
  python_path <- file.path(env_path, "bin", "python")
  if (!file.exists(python_path)) {
    cli_abort("Python not found in environment: {python_path}")
  }

  # Execute Python script with resource monitoring
  tryCatch(
    {
      # Start resource monitoring
      start_time <- Sys.time()
      max_memory_mb <- 0
      max_cpu_percent <- 0

      # Execute Python script
      result <- processx::run(
        command = python_path,
        args = temp_py,
        wd = work_dir,
        echo = show_output,
        error_on_status = FALSE,
        stdout_callback = if (show_output) NULL else function(x, ...) {},
        stderr_callback = if (show_output) NULL else function(x, ...) {}
      )

      # End timing
      end_time <- Sys.time()
      runtime_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Get resource usage if ps package is available
      if (requireNamespace("ps", quietly = TRUE)) {
        tryCatch(
          {
            # Try to get memory usage from current R process as approximation
            proc_info <- ps::ps_memory_info()
            max_memory_mb <- round(proc_info$rss / 1024 / 1024, 2) # Convert bytes to MB
            max_cpu_percent <- NA
          },
          error = function(e) {
            max_memory_mb <- NA
            max_cpu_percent <- NA
          }
        )
      } else {
        max_memory_mb <- NA
        max_cpu_percent <- NA
      }

      # Write log file
      .write_log_file(log_file, python_code, result)

      # Clean up temp file
      unlink(temp_py)

      list(
        exit_code = result$status,
        stdout = result$stdout,
        stderr = result$stderr,
        runtime_seconds = runtime_seconds,
        memory_mb = max_memory_mb,
        cpu_percent = max_cpu_percent
      )
    },
    error = function(e) {
      cli_alert_danger("Python execution failed: {e$message}")
      # Clean up temp file on error too
      unlink(temp_py)
      list(
        exit_code = 1,
        stdout = "",
        stderr = e$message,
        runtime_seconds = NA,
        memory_mb = NA,
        cpu_percent = NA
      )
    }
  )
}

#' Write Log File
#' @keywords internal
.write_log_file <- function(log_file, command, result) {
  tryCatch(
    {
      log_content <- c(
        paste("# ShennongTools Log -", Sys.time()),
        "",
        "## Command/Code:",
        command,
        "",
        "## Exit Code:",
        as.character(result$status),
        "",
        "## Standard Output:",
        result$stdout,
        "",
        "## Standard Error:",
        result$stderr,
        ""
      )

      writeLines(log_content, log_file)
    },
    error = function(e) {
      cli_alert_warning("Failed to write log file: {e$message}")
    }
  )
}

# Tool Installation Helper ----------------------------------------

#' Install Tool with YAML Configuration
#' Always use YAML-based installation to ensure all dependencies are included
#' @keywords internal
.install_tool_with_yaml <- function(tool_name,
                                    version,
                                    config,
                                    base_dir,
                                    show_messages) {
  # Extract environment specification from the tool YAML
  env <- config$environment
  channels <- env$channels
  dependencies <- env$dependencies

  # Resolve versions for dependencies that don't have explicit versions
  resolved_dependencies <- .resolve_dependency_versions(dependencies, channels, show_messages)

  # Get the actual version for the main tool for path naming
  actual_version <- .get_tool_version_from_dependencies(tool_name, resolved_dependencies, version)

  # Create temporary YAML environment file
  temp_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_yaml), add = TRUE)

  # Create the environment path in tool_name/version format
  env_path <- file.path(base_dir, tool_name, actual_version)

  # Check if already exists
  if (.is_installed(tool_name, actual_version, base_dir)) {
    if (show_messages) {
      cli_alert_success("Environment already exists at {env_path}")
    }
    return(env_path)
  }

  yaml_content <- list(
    name = basename(env_path), # This will be tool_name/version format for the yaml name
    channels = channels,
    dependencies = resolved_dependencies
  )
  write_yaml(yaml_content, temp_yaml)

  # Create environment from YAML file using low-level mamba call
  if (show_messages) {
    cli_alert_info("Creating environment '{tool_name}/{actual_version}' from YAML")
  }
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
    if (show_messages) {
      cli_abort("Failed to create environment from YAML: {result$stderr}")
    } else {
      stop("Failed to create environment from YAML: ", result$stderr, call. = FALSE)
    }
  }

  # Verify environment was created
  if (!dir.exists(env_path)) {
    if (show_messages) {
      cli_abort("Environment creation failed at {env_path}")
    } else {
      stop("Environment creation failed at ", env_path, call. = FALSE)
    }
  }

  # Enhanced verification: check if tool is actually properly installed
  tool_config <- config
  tool <- tryCatch(
    {
      # Create a temporary tool object for verification
      temp_tool <- new("Tool",
        tool_name = tool_name,
        versions = actual_version,
        default_version = actual_version,
        description = tool_config$description %||% "",
        citation = tool_config$citation %||% "",
        environment = tool_config$environment %||% list(),
        commands = tool_config$commands %||% list(),
        install_dates = list()
      )
      temp_tool
    },
    error = function(e) NULL
  )

  if (!is.null(tool)) {
    diagnosis <- .diagnose_tool_installation(tool, actual_version, base_dir, show_details = show_messages)

    if (!diagnosis$installed) {
      if (show_messages) {
        cli_alert_danger("Environment created but tool installation incomplete:")
        if (!is.null(diagnosis$issues)) {
          for (issue in diagnosis$issues) {
            cli_text("  • {issue}")
          }
        }

        # Show some debugging information
        if (file.exists(temp_yaml)) {
          cli_alert_info("YAML content used for installation:")
          yaml_lines <- readLines(temp_yaml)
          for (line in head(yaml_lines, 20)) {
            cli_text("  {line}")
          }
          if (length(yaml_lines) > 20) {
            cli_text("  ... ({length(yaml_lines) - 20} more lines)")
          }
        }

        cli_alert_warning("Tool may not be available or YAML configuration may need adjustment")
      }
    } else if (show_messages) {
      cli_alert_success("Tool installation verified successfully")
    }
  }

  if (show_messages) {
    cli_alert_success("Environment created successfully at {env_path}")
  }
  return(env_path)
}

# Global toolbox management ----------------------------------------

# Store default toolbox in package environment
.toolbox_env <- new.env(parent = emptyenv())

#' Get Default Toolbox with Smart Loading
#' @keywords internal
.get_default_toolbox <- function() {
  if (!exists("current_toolbox", envir = .toolbox_env)) {
    toolbox <- .load_smart_toolbox()
    assign("current_toolbox", toolbox, envir = .toolbox_env)
  }
  return(get("current_toolbox", envir = .toolbox_env))
}

#' Set Default Toolbox
#' @keywords internal
.set_default_toolbox <- function(toolbox) {
  assign("current_toolbox", toolbox, envir = .toolbox_env)
}

#' Smart Toolbox Loading with Priority
#' @keywords internal
.load_smart_toolbox <- function() {
  # 1. Check for user-specified path via environment variable
  user_path <- Sys.getenv("SHENNONG_TOOLBOX_PATH", "")
  if (nzchar(user_path) && file.exists(user_path)) {
    tryCatch(
      {
        toolbox <- readRDS(user_path)
        if (inherits(toolbox, "Toolbox")) {
          return(toolbox)
        }
      },
      error = function(e) {
        cli_alert_warning("Failed to load toolbox from {user_path}: {e$message}")
      }
    )
  }

  # 2. Check for user toolbox in user config directory
  user_config_dir <- R_user_dir("shennong-tools", "config")
  user_toolbox_path <- file.path(user_config_dir, "user_toolbox.rds")
  if (file.exists(user_toolbox_path)) {
    tryCatch(
      {
        toolbox <- readRDS(user_toolbox_path)
        if (inherits(toolbox, "Toolbox")) {
          return(toolbox)
        }
      },
      error = function(e) {
        cli_alert_warning("Failed to load user toolbox: {e$message}")
      }
    )
  }

  # 3. Use package default toolbox (pre-loaded with all built-in tools)
  tryCatch(
    {
      # Try to load from package data
      data("default_toolbox", package = "ShennongTools", envir = environment())
      if (exists("default_toolbox", envir = environment())) {
        base_toolbox <- get("default_toolbox", envir = environment())
      } else {
        # Fallback: create empty toolbox
        base_toolbox <- sn_initialize_toolbox()
      }

      # Clone the toolbox to avoid modifying the original
      toolbox <- new("Toolbox",
        base_dir = base_toolbox@base_dir,
        mamba_path = base_toolbox@mamba_path,
        tools = base_toolbox@tools
      )

      # Set runtime paths if not set
      if (length(toolbox@base_dir) == 0) {
        toolbox@base_dir <- R_user_dir("shennong-tools", "data")
      }
      if (length(toolbox@mamba_path) == 0) {
        toolbox@mamba_path <- .check_mamba()
      }

      return(toolbox)
    },
    error = function(e) {
      cli_alert_warning("Failed to load default toolbox: {e$message}")
      # Fallback: create empty toolbox
      return(sn_initialize_toolbox())
    }
  )

  # 4. Final fallback: create empty toolbox
  return(sn_initialize_toolbox())
}

#' Save User Toolbox
#' @keywords internal
.save_user_toolbox <- function(toolbox) {
  # Only save if toolbox has been modified (has tools added by user)
  user_config_dir <- R_user_dir("shennong-tools", "config")

  # Create config directory if it doesn't exist
  if (!dir.exists(user_config_dir)) {
    dir.create(user_config_dir, recursive = TRUE, showWarnings = FALSE)
  }

  user_toolbox_path <- file.path(user_config_dir, "user_toolbox.rds")

  tryCatch(
    {
      saveRDS(toolbox, user_toolbox_path)
      return(TRUE)
    },
    error = function(e) {
      cli_alert_warning("Failed to save user toolbox: {e$message}")
      return(FALSE)
    }
  )
}

#' Check if Toolbox Should be Saved
#' @keywords internal
.should_save_toolbox <- function(toolbox) {
  # Always save user toolbox when user makes modifications
  # This ensures user changes are persisted regardless of tool count
  return(TRUE)
}

#' Check if Toolbox is User Modified
#' @keywords internal
.is_user_modified_toolbox <- function() {
  user_config_dir <- R_user_dir("shennong-tools", "config")
  user_toolbox_path <- file.path(user_config_dir, "user_toolbox.rds")
  return(file.exists(user_toolbox_path))
}

#' Reset Default Toolbox
#' @family tool management
#' @concept tool management
#' @export
sn_reset_toolbox <- function() {
  if (exists("default_toolbox", envir = .toolbox_env)) {
    rm("default_toolbox", envir = .toolbox_env)
  }
  cli_alert_info("Default toolbox reset")
}

#' Get Tool Installation Path
#' @keywords internal
.get_tool_install_path <- function(toolbox, tool_name, version) {
  base_dir <- toolbox@base_dir

  # Try both naming schemes
  install_path1 <- file.path(base_dir, tool_name, version)
  install_path2 <- file.path(base_dir, paste0(tool_name, "_", version))

  if (dir.exists(install_path1)) {
    return(install_path1)
  } else if (dir.exists(install_path2)) {
    return(install_path2)
  } else {
    cli_abort("Installation path not found for {tool_name} version {version}")
  }
}
