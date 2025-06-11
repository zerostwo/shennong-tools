#' Get Help for Tool or Command
#'
#' Displays help information for a tool or specific command. This function automatically
#' manages the toolbox and can be called directly with tool name.
#'
#' @param tool_name Character. Name of the tool.
#' @param command Character. Specific command (optional).
#' @param version Character. Tool version (optional).
#' @param raw Logical. If TRUE, shows raw tool help output using the tool's help flag.
#'   If FALSE (default), shows formatted help from YAML configuration.
#'
#' @return Invisible. Help information is printed to console.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get help for tool
#' sn_help("samtools")
#'
#' # Get help for specific command
#' sn_help("samtools", "view")
#'
#' # Get raw tool help output
#' sn_help("samtools", raw = TRUE)
#' }
sn_help <- function(tool_name, command = NULL, version = NULL, raw = FALSE) {
  # Get or create default toolbox
  toolbox <- .get_default_toolbox()

  # Check if tool is in toolbox, if not try to add it
  if (!tool_name %in% names(toolbox@tools)) {
    cli_alert_info("Tool {tool_name} not found in toolbox, adding it...")
    toolbox <- sn_add_tool(toolbox, tool_name, version = version, install = FALSE)
    # Update the global toolbox
    .set_default_toolbox(toolbox)
  }

  # Check tool exists
  if (!tool_name %in% names(toolbox@tools)) {
    cli_abort("Tool {tool_name} not found in toolbox. Available tools: {paste(names(toolbox@tools), collapse = ', ')}")
  }

  # Determine version
  available_versions <- names(toolbox@tools[[tool_name]])
  if (is.null(version)) {
    version <- available_versions[1]
  }

  if (!version %in% available_versions) {
    cli_abort("Version {version} not found for {tool_name}. Available versions: {paste(available_versions, collapse = ', ')}")
  }

  tool <- toolbox@tools[[tool_name]][[version]]

  # Display tool information
  if (raw) {
    .display_raw_tool_help(toolbox, tool, command)
  } else {
    .display_tool_help(tool, command)
  }

  invisible()
}

#' Get Help for Built-in Tool
#'
#' Displays help for tools in the built-in registry without requiring a toolbox.
#'
#' @param tool_name Character. Name of the tool.
#' @param command Character. Specific command (optional).
#'
#' @return Invisible. Help information is printed to console.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get help for built-in tool
#' sn_help_builtin("samtools")
#'
#' # Get help for specific command
#' sn_help_builtin("samtools", "view")
#' }
sn_help_builtin <- function(tool_name, command = NULL) {
  # Use the existing show tool function
  sn_show_tool(tool_name, command)

  invisible()
}

#' Validate Tool Configuration
#'
#' Validates that a tool is properly configured and installed. This function
#' automatically manages the toolbox.
#'
#' @param tool_name Character. Name of the tool to validate.
#' @param version Character. Tool version (optional).
#'
#' @return Logical. TRUE if valid, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate tool
#' is_valid <- sn_validate_tool("samtools")
#' }
sn_validate_tool <- function(tool_name, version = NULL) {
  # Get or create default toolbox
  toolbox <- .get_default_toolbox()

  # Check tool exists
  if (!tool_name %in% names(toolbox@tools)) {
    cli_alert_danger("Tool {tool_name} not found in toolbox")
    return(FALSE)
  }

  # Determine version
  available_versions <- names(toolbox@tools[[tool_name]])
  if (is.null(version)) {
    version <- available_versions[1]
  }

  if (!version %in% available_versions) {
    cli_alert_danger("Version {version} not found for {tool_name}")
    return(FALSE)
  }

  tool <- toolbox@tools[[tool_name]][[version]]

  # Check installation
  if (!tool@installed) {
    cli_alert_warning("Tool {tool_name} version {version} is not installed")
    return(FALSE)
  }

  # Check installation path
  install_path1 <- file.path(toolbox@base_dir, tool_name, version)
  install_path2 <- file.path(toolbox@base_dir, paste0(tool_name, "_", version))

  actual_install_path <- if (dir.exists(install_path1)) install_path1 else install_path2

  if (!dir.exists(actual_install_path)) {
    cli_alert_danger("Installation path does not exist: {actual_install_path}")
    return(FALSE)
  }

  # Check environment has binaries
  bin_dir <- file.path(actual_install_path, "bin")
  if (!dir.exists(bin_dir)) {
    cli_alert_warning("Binary directory not found: {bin_dir}")
    return(FALSE)
  }

  # Tool is valid
  cli_alert_success("Tool {tool_name} version {version} is valid and ready to use")
  return(TRUE)
}

# Internal helper functions ---------------------------------------------------

#' Display Tool Help
#' @keywords internal
.display_tool_help <- function(tool, command = NULL) {
  cli_rule(
    left = paste0(symbol$gear, " Tool Help: ", tool@tool_name),
    right = paste0("v", tool@version)
  )

  # Basic information with modern formatting
  cli_text("{col_grey('Description:')} {tool@description}")
  cli_text("{col_grey('Version:')} {tool@version}")

  # Installation status with colored indicators
  status_icon <- if (tool@installed) col_green(symbol$tick) else col_red(symbol$cross)
  status_text <- if (tool@installed) col_green("Yes") else col_red("No")
  cli_text("{col_grey('Installed:')} {status_icon} {status_text}")

  if (nzchar(tool@citation)) {
    cli_text("{col_grey('Citation:')} {col_cyan(tool@citation)}")
  }

  # Environment information with better formatting
  if (length(tool@environment) > 0) {
    cli_text() # Single line break
    cli_h3(paste0(symbol$package, " Environment"))

    if (!is.null(tool@environment$channels)) {
      cli_text("{col_grey('Channels:')} {paste(tool@environment$channels, collapse = ', ')}")
    }

    if (!is.null(tool@environment$dependencies)) {
      cli_text("{col_grey('Dependencies:')}")
      for (dep in tool@environment$dependencies) {
        if (is.character(dep)) {
          cli_text("{col_silver(symbol$bullet)} {dep}")
        }
      }
    }
  }

  # Commands with improved layout
  if (!is.null(command)) {
    # Show specific command
    if (command %in% names(tool@commands)) {
      cli_text() # Single line break
      cli_h3(paste0(symbol$arrow_right, " Command: ", command))
      .show_command_help(tool@commands[[command]], command, tool@tool_name)
    } else {
      available_commands <- paste(names(tool@commands), collapse = ", ")
      cli_alert_warning("Command '{command}' not found. Available commands: {available_commands}")
    }
  } else {
    # Show all commands
    cli_text() # Single line break
    cli_h3(paste0(symbol$menu, " Available Commands"))
    for (cmd_name in names(tool@commands)) {
      cmd_config <- tool@commands[[cmd_name]]
      desc <- cmd_config$description %||% ""
      cli_text("{col_cyan(cmd_name)}: {desc}")
    }

    cli_text() # Single line break
    cli_text("{col_silver('Use')} {col_yellow('sn_help(\"{tool@tool_name}\", \"command_name\")')} {col_silver('for detailed command help')}")
  }

  cli_rule()
}

#' Show Command Help
#' @keywords internal
.show_command_help <- function(cmd_config, command_name, tool_name = "tool_name") {
  if (!is.null(cmd_config$description)) {
    cli_text("{col_grey('Description:')} {cmd_config$description}")
  }

  # Inputs with modern formatting
  if (!is.null(cmd_config$inputs) && length(cmd_config$inputs) > 0) {
    cli_h3(paste0(symbol$file, " Inputs"))
    for (input_name in names(cmd_config$inputs)) {
      input_def <- cmd_config$inputs[[input_name]]
      required <- input_def$required %||% FALSE
      required_badge <- if (required) col_red("required") else col_green("optional")
      datatype <- input_def$datatype %||% "string"

      # Handle datatype as vector
      if (length(datatype) > 1) {
        datatype_str <- paste0("(", paste(datatype, collapse = "), ("), ")")
      } else {
        datatype_str <- paste0("(", datatype, ")")
      }

      desc <- input_def$description %||% ""

      cli_text("{col_cyan(input_name)} {col_grey(datatype_str)} {required_badge}")
      if (nzchar(desc)) {
        cli_text("  {desc}")
      }

      if (!is.null(input_def$default)) {
        cli_text("  {col_grey('Default:')} {col_silver(input_def$default)}")
      }
    }
  }

  # Outputs with modern formatting
  if (!is.null(cmd_config$outputs) && length(cmd_config$outputs) > 0) {
    cli_h3(paste0(symbol$arrow_right, " Outputs"))
    for (output_name in names(cmd_config$outputs)) {
      output_def <- cmd_config$outputs[[output_name]]
      required <- output_def$required %||% FALSE
      required_badge <- if (required) col_red("required") else col_green("optional")
      datatype <- output_def$datatype %||% "string"

      # Handle datatype as vector
      if (length(datatype) > 1) {
        datatype_str <- paste0("(", paste(datatype, collapse = "), ("), ")")
      } else {
        datatype_str <- paste0("(", datatype, ")")
      }

      desc <- output_def$description %||% ""

      cli_text("{col_cyan(output_name)} {col_grey(datatype_str)} {required_badge}")
      if (nzchar(desc)) {
        cli_text("  {desc}")
      }
    }
  }

  # Parameters with modern formatting
  if (!is.null(cmd_config$params) && length(cmd_config$params) > 0) {
    cli_h3(paste0(symbol$gear, " Parameters"))
    for (param_name in names(cmd_config$params)) {
      param_def <- cmd_config$params[[param_name]]
      datatype <- param_def$datatype %||% "string"
      desc <- param_def$description %||% ""

      cli_text("{col_cyan(param_name)} {col_grey(paste0('(', datatype, ')'))}")
      if (nzchar(desc)) {
        cli_text("  {desc}")
      }

      if (!is.null(param_def$default)) {
        cli_text("  {col_grey('Default:')} {col_silver(param_def$default)}")
      }
    }
  }

  # Enhanced Usage example with actual parameters
  cli_h3(paste0(symbol$play, " Usage Example"))
  usage_example <- .generate_usage_example(cmd_config, command_name, tool_name)

  cli_code(usage_example)
}

#' Generate Usage Example with Actual Parameters
#' @keywords internal
.generate_usage_example <- function(cmd_config, command_name, tool_name) {
  # Use the unified datatype-based example generation function
  return(.sn_generate_usage_example(cmd_config, command_name, tool_name))
}

#' Display Raw Tool Help
#' @keywords internal
.display_raw_tool_help <- function(toolbox, tool, command = NULL) {
  if (!tool@installed) {
    cli_abort("Tool {tool@tool_name} is not installed. Install it first with sn_install_tool().")
  }

  # Get tool installation path
  env_path <- .get_tool_install_path(toolbox, tool@tool_name, tool@version)

  if (is.null(command)) {
    # Show general tool help
    if (length(tool@commands) > 0) {
      # Use the first command's help flag as general help
      first_cmd <- tool@commands[[1]]
      binary <- first_cmd$binary %||% tool@tool_name
      help_flag <- first_cmd$help_flag %||% "--help"
    } else {
      binary <- tool@tool_name
      help_flag <- "--help"
    }
  } else {
    # Show specific command help
    if (!command %in% names(tool@commands)) {
      cli_abort("Command '{command}' not found. Available commands: {paste(names(tool@commands), collapse = ', ')}")
    }

    cmd_config <- tool@commands[[command]]
    binary <- cmd_config$binary %||% tool@tool_name
    help_flag <- cmd_config$help_flag %||% "--help"
  }

  # Execute help command
  tryCatch(
    {
      env_vars <- character(0)
      if (!is.null(env_path) && dir.exists(env_path)) {
        env_bin <- file.path(env_path, "bin")
        if (dir.exists(env_bin)) {
          current_path <- Sys.getenv("PATH")
          new_path <- paste(env_bin, current_path, sep = .Platform$path.sep)
          env_vars <- c("PATH" = new_path)
        }
      }

      result <- processx::run(
        command = binary,
        args = help_flag,
        env = env_vars,
        error_on_status = FALSE,
        timeout = 30 # 30 seconds timeout
      )

      # Display raw output
      cli_rule(left = paste0("Raw Help: ", tool@tool_name, if (!is.null(command)) paste0(" ", command) else ""))
      cat(result$stdout)
      if (nzchar(result$stderr)) {
        cat("\n--- STDERR ---\n")
        cat(result$stderr)
      }
      cli_rule()
    },
    error = function(e) {
      cli_alert_danger("Failed to get raw help: {e$message}")
    }
  )
}
