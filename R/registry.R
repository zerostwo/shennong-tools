#' List Available Built-in Tools
#'
#' Lists all available tools in the built-in registry with detailed information including
#' description, available commands, and installation status.
#'
#' @param tools_dir Character. Directory containing YAML tool definitions.
#'   Defaults to package inst/tools directory.
#' @param simple Logical. If TRUE, returns only tool names. If FALSE (default),
#'   displays detailed information with modern formatting.
#'
#' @return Character vector of available tool names (if simple=TRUE) or
#'   invisible output with formatted display.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get simple list
#' available_tools <- sn_list_tools(simple = TRUE)
#'
#' # Display detailed information
#' sn_list_tools()
#' }
sn_list_tools <- function(tools_dir = NULL, simple = FALSE) {
  if (is.null(tools_dir)) {
    tools_dir <- system.file("tools", package = "ShennongTools")
    if (tools_dir == "") {
      tools_dir <- file.path("inst", "tools")
    }
  }

  if (!dir.exists(tools_dir)) {
    cli_warn("Tools directory not found: {tools_dir}")
    return(character(0))
  }

  # Find all YAML files
  yaml_files <- list.files(
    path = tools_dir,
    pattern = "\\.ya?ml$",
    full.names = FALSE,
    ignore.case = TRUE
  )

  # Extract tool names (remove .yaml extension)
  tool_names <- sub("\\.(yaml|yml)$", "", yaml_files, ignore.case = TRUE)

  if (simple) {
    return(tool_names)
  }

  # Get default toolbox to check installation status
  toolbox <- tryCatch(
    {
      .get_default_toolbox()
    },
    error = function(e) NULL
  )

  # Display detailed information
  cli_rule(
    left = paste0(symbol$package, " Available Tools"),
    right = paste0(length(tool_names), " tools")
  )

  for (tool_name in sort(tool_names)) {
    info <- sn_get_tool_info(tool_name, tools_dir)
    if (is.null(info)) next

    # Check installation status
    installed <- FALSE
    version_info <- ""
    if (!is.null(toolbox) && tool_name %in% names(toolbox@tools)) {
      versions <- names(toolbox@tools[[tool_name]])
      installed_versions <- c()
      for (ver in versions) {
        if (toolbox@tools[[tool_name]][[ver]]@installed) {
          installed_versions <- c(installed_versions, ver)
        }
      }
      if (length(installed_versions) > 0) {
        installed <- TRUE
        version_info <- paste0(" (v", paste(installed_versions, collapse = ", v"), ")")
      }
    }

    # Status indicator
    status_icon <- if (installed) col_green(symbol$tick) else col_grey(symbol$circle)
    status_text <- if (installed) col_green("installed") else col_grey("available")

    # Tool info
    cli_div(theme = list(span.tool = list(color = "cyan", "font-weight" = "bold")))
    cli_text("{status_icon} {.tool {tool_name}}{version_info}")
    cli_end()

    # Description
    if (nzchar(info$description)) {
      cli_text("  {col_grey(info$description)}")
    }

    # Commands
    if (length(info$commands) > 0) {
      commands_text <- paste(info$commands, collapse = ", ")
      cli_text("  {col_silver('Commands:')} {commands_text}")
    }

    cli_text() # Add spacing
  }

  # Footer with usage information
  cli_div(theme = list(span.code = list(color = "blue")))
  cli_text(col_grey("Use {.code sn_help('tool_name')} for detailed help"))
  cli_text(col_grey("Use {.code sn_run('tool_name', 'command', ...)} to execute"))
  cli_end()

  cli_rule()

  invisible(tool_names)
}

#' Get Tool Information
#'
#' Retrieves basic information about a built-in tool.
#'
#' @param tool_name Character. Name of the tool.
#' @param tools_dir Character. Directory containing YAML tool definitions.
#'
#' @return List with tool information or NULL if not found.
#' @export
#'
#' @examples
#' \dontrun{
#' info <- sn_get_tool_info("samtools")
#' print(info$description)
#' print(names(info$commands))
#' }
sn_get_tool_info <- function(tool_name, tools_dir = NULL) {
  if (is.null(tools_dir)) {
    tools_dir <- system.file("tools", package = "ShennongTools")
    if (tools_dir == "") {
      tools_dir <- file.path("inst", "tools")
    }
  }

  yaml_file <- file.path(tools_dir, paste0(tool_name, ".yaml"))

  if (!file.exists(yaml_file)) {
    cli_warn("Tool {tool_name} not found")
    return(NULL)
  }

  tryCatch(
    {
      config <- yaml.load_file(yaml_file)

      # Return basic information
      list(
        tool_name = config$tool_name,
        description = config$description %||% "",
        citation = config$citation %||% "",
        commands = names(config$commands %||% list()),
        environment = config$environment %||% list()
      )
    },
    error = function(e) {
      cli_warn("Failed to load tool {tool_name}: {e$message}")
      return(NULL)
    }
  )
}

#' Show Tool Details
#'
#' Displays detailed information about a built-in tool including commands and parameters.
#'
#' @param tool_name Character. Name of the tool.
#' @param command Character. Specific command to show (optional).
#'
#' @return Invisible. Information is printed to console.
#' @export
#'
#' @examples
#' \dontrun{
#' # Show all tool information
#' sn_show_tool("samtools")
#'
#' # Show specific command
#' sn_show_tool("samtools", "view")
#' }
sn_show_tool <- function(tool_name, command = NULL) {
  info <- sn_get_tool_info(tool_name)
  if (is.null(info)) {
    return(invisible())
  }

  # Load full configuration for detailed display
  tools_dir <- system.file("tools", package = "ShennongTools")
  if (tools_dir == "") {
    tools_dir <- file.path("inst", "tools")
  }

  yaml_file <- file.path(tools_dir, paste0(tool_name, ".yaml"))
  config <- yaml.load_file(yaml_file)

  # Display header
  cli_rule("Tool: {tool_name}")

  # Basic information
  cli_bullets(c("i" = "Description: {info$description}"))
  if (nzchar(info$citation)) {
    cli_bullets(c("i" = "Citation: {info$citation}"))
  }

  # Environment information
  if (length(info$environment) > 0) {
    cli_bullets(c("i" = "Environment:"))

    if (!is.null(info$environment$channels)) {
      cli_bullets(c(" " = "  Channels: {paste(info$environment$channels, collapse = ', ')}"))
    }

    if (!is.null(info$environment$dependencies)) {
      cli_bullets(c(" " = "  Dependencies:"))
      for (dep in info$environment$dependencies) {
        if (is.character(dep)) {
          cli_bullets(c(" " = "    - {dep}"))
        } else if (is.list(dep) && "pip" %in% names(dep)) {
          cli_bullets(c(" " = "    - pip packages:"))
          for (pip_dep in dep$pip) {
            cli_bullets(c(" " = "      - {pip_dep}"))
          }
        }
      }
    }
  }

  # Commands
  if (!is.null(command)) {
    # Show specific command
    if (command %in% names(config$commands)) {
      .show_command_details(config$commands[[command]], command)
    } else {
      cli_alert_warning("Command '{command}' not found. Available commands: {paste(info$commands, collapse = ', ')}")
    }
  } else {
    # Show all commands
    cli_bullets(c("i" = "Commands:"))
    for (cmd_name in info$commands) {
      cmd_config <- config$commands[[cmd_name]]
      desc <- cmd_config$description %||% ""
      cli_bullets(c(" " = "  {cmd_name}: {desc}"))
    }

    cli_bullets(c("i" = "Use sn_show_tool('{tool_name}', 'command_name') for detailed command information"))
  }

  cli_rule()
  invisible()
}

#' Show Command Details
#' @keywords internal
.show_command_details <- function(cmd_config, command_name) {
  if (!is.null(cmd_config$description)) {
    cli_text("{col_grey('Description:')} {cmd_config$description}")
    cli_text()
  }

  # Inputs with modern formatting
  if (!is.null(cmd_config$inputs) && length(cmd_config$inputs) > 0) {
    cli_h3(paste0(symbol$file, " Inputs"))
    for (input_name in names(cmd_config$inputs)) {
      input_def <- cmd_config$inputs[[input_name]]
      required <- input_def$required %||% FALSE
      required_badge <- if (required) col_red("required") else col_green("optional")
      datatype <- input_def$datatype %||% "string"
      desc <- input_def$description %||% ""

      cli_text("  {col_cyan(input_name)} {col_grey(paste0('(', datatype, ')'))} {required_badge}")
      cli_text("    {desc}")

      if (!is.null(input_def$default)) {
        cli_text("    {col_grey('Default:')} {col_silver(input_def$default)}")
      }
    }
    cli_text()
  }

  # Outputs with modern formatting
  if (!is.null(cmd_config$outputs) && length(cmd_config$outputs) > 0) {
    cli_h3(paste0(symbol$arrow_right, " Outputs"))
    for (output_name in names(cmd_config$outputs)) {
      output_def <- cmd_config$outputs[[output_name]]
      required <- output_def$required %||% FALSE
      required_badge <- if (required) col_red("required") else col_green("optional")
      datatype <- output_def$datatype %||% "string"
      desc <- output_def$description %||% ""

      cli_text("  {col_cyan(output_name)} {col_grey(paste0('(', datatype, ')'))} {required_badge}")
      cli_text("    {desc}")
    }
    cli_text()
  }

  # Parameters with modern formatting
  if (!is.null(cmd_config$params) && length(cmd_config$params) > 0) {
    cli_h3(paste0(symbol$gear, " Parameters"))
    for (param_name in names(cmd_config$params)) {
      param_def <- cmd_config$params[[param_name]]
      datatype <- param_def$datatype %||% "string"
      desc <- param_def$description %||% ""

      cli_text("  {col_cyan(param_name)} {col_grey(paste0('(', datatype, ')'))}")
      cli_text("    {desc}")

      if (!is.null(param_def$default)) {
        cli_text("    {col_grey('Default:')} {col_silver(param_def$default)}")
      }
    }
    cli_text()
  }

  # Enhanced Usage example with actual parameters
  cli_h3(paste0(symbol$play, " Usage Example"))
  # Generate usage example with actual parameters
  usage_example <- .generate_usage_example_registry(cmd_config, command_name, "tool_name")

  cli_code(usage_example)

  # Template information
  if (!is.null(cmd_config$shell)) {
    cli_h3(paste0(symbol$terminal, " Shell Template"))
    cli_code(cmd_config$shell)
  } else if (!is.null(cmd_config$python)) {
    cli_h3(paste0(symbol$code, " Python Template"))
    # Show first few lines of Python code
    python_lines <- strsplit(cmd_config$python, "\n")[[1]]
    show_lines <- head(python_lines, 5)
    preview_code <- paste(show_lines, collapse = "\n")
    if (length(python_lines) > 5) {
      preview_code <- paste0(preview_code, "\n# ... (", length(python_lines) - 5, " more lines)")
    }
    cli_code(preview_code)
  }
}

#' Generate Usage Example with Actual Parameters (Registry Version)
#' @keywords internal
.generate_usage_example_registry <- function(cmd_config, command_name, tool_name) {
  # Use the unified datatype-based example generation function
  return(.sn_generate_usage_example(cmd_config, command_name, tool_name))
}

#' Load Tool Registry
#'
#' Loads the built-in tool registry from YAML files.
#'
#' @param tools_dir Character. Directory containing YAML tool definitions.
#'   Defaults to package inst/tools directory.
#'
#' @return List of tool configurations.
#' @export
sn_load_registry <- function(tools_dir = NULL) {
  if (is.null(tools_dir)) {
    tools_dir <- system.file("tools", package = "ShennongTools")
    if (tools_dir == "") {
      tools_dir <- file.path("inst", "tools")
    }
  }

  if (!dir.exists(tools_dir)) {
    cli_warn("Tools directory not found: {tools_dir}")
    return(list())
  }

  # Find all YAML files
  yaml_files <- list.files(
    path = tools_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  registry <- list()

  for (yaml_file in yaml_files) {
    tryCatch(
      {
        config <- yaml.load_file(yaml_file)
        if (!is.null(config$tool_name)) {
          registry[[config$tool_name]] <- config
        }
      },
      error = function(e) {
        cli_warn("Failed to load {basename(yaml_file)}: {e$message}")
      }
    )
  }

  return(registry)
}

#' Get Tool Configuration
#'
#' Get configuration for a specific tool from the registry.
#'
#' @param tool_name Character. Name of the tool.
#' @param registry List. Tool registry (optional, will load if not provided).
#'
#' @return List with tool configuration or NULL if not found.
#' @export
sn_get_tool_config <- function(tool_name, registry = NULL) {
  if (is.null(registry)) {
    registry <- sn_load_registry()
  }

  if (tool_name %in% names(registry)) {
    return(registry[[tool_name]])
  } else {
    return(NULL)
  }
}
