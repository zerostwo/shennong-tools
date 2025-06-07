# S4 Class Definitions --------------------------------------------------------
# This file contains all S4 class definitions for the Shennong Tool System

#' Tool S4 Class
#'
#' Represents a bioinformatics tool with its configuration and metadata.
#'
#' @slot name Character. Tool name (e.g., "star", "hisat2").
#' @slot version Character. Tool version.
#' @slot package_name Character. Package name for installation (may differ from tool name).
#' @slot binary_name Character. Binary/executable name (may differ from tool name).
#' @slot env_path Character. Path to conda/mamba environment.
#' @slot channel Character. Conda channel (e.g., "bioconda").
#' @slot description Character. Tool description.
#' @slot commands List. Available commands/subcommands for this tool.
#' @slot citation List. Citation information for the tool.
#' @slot installed Logical. Whether the tool is currently installed.
#' @slot install_date POSIXct. When the tool was installed.
#'
#' @export
setClass(
  "Tool",
  slots = c(
    name = "character",
    version = "character",
    package_name = "character",
    binary_name = "character",
    env_path = "character",
    channel = "character",
    description = "character",
    commands = "list",
    citation = "list",
    installed = "logical",
    install_date = "POSIXct"
  ),
  prototype = list(
    name = character(0),
    version = character(0),
    package_name = character(0),
    binary_name = character(0),
    env_path = character(0),
    channel = "bioconda",
    description = character(0),
    commands = list(),
    citation = list(),
    installed = FALSE,
    install_date = as.POSIXct(character(0))
  )
)

#' Toolbox S4 Class
#'
#' Manages a collection of bioinformatics tools.
#'
#' @slot tools List. Named list of Tool objects.
#' @slot base_dir Character. Base directory for tool installations.
#' @slot mamba_path Character. Path to mamba/micromamba executable.
#' @slot config List. Configuration options for the toolbox.
#' @slot registry List. Registry of available tools and their metadata.
#'
#' @export
setClass(
  "Toolbox",
  slots = c(
    tools = "list",
    base_dir = "character",
    mamba_path = "character",
    config = "list",
    registry = "list"
  ),
  prototype = list(
    tools = list(),
    base_dir = character(0),
    mamba_path = character(0),
    config = list(),
    registry = list()
  )
)

#' ToolCall S4 Class
#'
#' Represents a single tool invocation with inputs, outputs, and parameters.
#'
#' @slot tool Tool. The Tool object being called.
#' @slot command Character. The specific command/subcommand.
#' @slot args List. Command arguments and parameters.
#' @slot inputs List. Input files/objects with their roles.
#' @slot outputs List. Expected output files/objects.
#' @slot work_dir Character. Working directory for execution.
#' @slot log_file Character. Path to log file.
#' @slot status Character. Execution status ("pending", "running", "success", "failed").
#' @slot start_time POSIXct. When execution started.
#' @slot end_time POSIXct. When execution ended.
#' @slot return_code Numeric. Process return code.
#' @slot stdout Character. Captured stdout.
#' @slot stderr Character. Captured stderr.
#'
#' @export
setClass(
  "ToolCall",
  slots = c(
    tool = "Tool",
    command = "character",
    args = "list",
    inputs = "list",
    outputs = "list",
    work_dir = "character",
    log_file = "character",
    status = "character",
    start_time = "POSIXct",
    end_time = "POSIXct",
    return_code = "numeric",
    stdout = "character",
    stderr = "character"
  ),
  prototype = list(
    tool = new("Tool"),
    command = character(0),
    args = list(),
    inputs = list(),
    outputs = list(),
    work_dir = ".",
    log_file = character(0),
    status = "pending",
    start_time = as.POSIXct(character(0)),
    end_time = as.POSIXct(character(0)),
    return_code = numeric(0),
    stdout = character(0),
    stderr = character(0)
  )
)

# Validation Methods -----------------------------------------------------------

setValidity("Tool", function(object) {
  errors <- character()

  if (length(object@name) == 0) {
    errors <- c(errors, "Tool name cannot be empty")
  }

  if (length(object@installed) > 0 && object@installed) {
    if (length(object@env_path) == 0 || !dir.exists(object@env_path)) {
      errors <- c(errors, "Installed tool must have valid env_path")
    }
  }

  if (length(errors) == 0) TRUE else errors
})

setValidity("Toolbox", function(object) {
  errors <- character()

  if (length(object@base_dir) > 0 && !dir.exists(object@base_dir)) {
    errors <- c(errors, "Toolbox base_dir must exist")
  }

  # Check that all tools are Tool objects
  if (length(object@tools) > 0) {
    tool_classes <- sapply(object@tools, class)
    if (!all(tool_classes == "Tool")) {
      errors <- c(errors, "All tools must be Tool objects")
    }
  }

  if (length(errors) == 0) TRUE else errors
})

setValidity("ToolCall", function(object) {
  errors <- character()

  if (!inherits(object@tool, "Tool")) {
    errors <- c(errors, "tool slot must contain a Tool object")
  }

  if (length(object@status) > 0) {
    valid_statuses <- c("pending", "running", "success", "failed", "cancelled")
    if (!object@status %in% valid_statuses) {
      errors <- c(errors, paste(
        "status must be one of:",
        paste(valid_statuses, collapse = ", ")
      ))
    }
  }

  if (length(errors) == 0) TRUE else errors
})

# Show Methods -----------------------------------------------------------------

#' Show Method for Tool
#' @param object A Tool object
#' @export
setMethod("show", "Tool", function(object) {
  # Header with status indicator
  status_icon <- if (object@installed) symbol$tick else symbol$cross
  status_color <- if (object@installed) "green" else "red"

  cli_rule("{status_icon} Tool: {.strong {object@name}}")

  # Basic information
  cli_dl(c(
    "Package" = object@package_name,
    "Version" = if (length(object@version) > 0) object@version else "unknown",
    "Channel" = object@channel,
    "Status" = if (object@installed) "{.val installed}" else "{.val not installed}"
  ))

  # Installation details
  if (object@installed && length(object@env_path) > 0) {
    cli_text("")
    cli_h3("Installation Details")
    cli_dl(c(
      "Environment" = "{.file {object@env_path}}",
      "Binary" = object@binary_name,
      "Installed" = if (length(object@install_date) > 0) format(object@install_date, "%Y-%m-%d %H:%M:%S") else "unknown"
    ))
  }

  # Available commands
  if (length(object@commands) > 0) {
    cli_text("")
    cli_h3("Available Commands")
    cmd_names <- names(object@commands)
    cmd_desc <- sapply(object@commands, function(x) x$description %||% "")

    for (i in seq_along(cmd_names)) {
      if (nzchar(cmd_desc[i])) {
        cli_li("{.code {cmd_names[i]}}: {cmd_desc[i]}")
      } else {
        cli_li("{.code {cmd_names[i]}}")
      }
    }
  }

  # Description and citation
  if (nzchar(object@description)) {
    cli_text("")
    cli_h3("Description")
    cli_text(object@description)
  }

  if (length(object@citation) > 0 && !is.null(object@citation)) {
    cli_text("")
    cli_h3("Citation")
    if (is.character(object@citation)) {
      cli_text("{.emph DOI: {object@citation}}")
    } else if (is.list(object@citation)) {
      if (!is.null(object@citation$doi)) {
        cli_text("{.emph DOI: {object@citation$doi}}")
      }
      if (!is.null(object@citation$title)) {
        cli_text("{.emph Title: {object@citation$title}}")
      }
    }
  }

  cli_text("")
  invisible(object)
})

#' Show Method for Toolbox
#' @param object A Toolbox object
#' @export
setMethod("show", "Toolbox", function(object) {
  # Header
  cli_rule("{symbol$tools} ShennongTools Toolbox")

  # Configuration
  cli_h3("Configuration")
  cli_dl(c(
    "Base directory" = "{.file {object@base_dir}}",
    "Package manager" = if (nzchar(object@mamba_path)) "{.file {object@mamba_path}}" else "{.emph not found}",
    "Auto-install" = if (object@config$auto_install) "{.val enabled}" else "{.val disabled}"
  ))

  # Statistics
  installed_count <- length(object@tools)
  registry_count <- length(object@registry)
  installed_working <- sum(sapply(object@tools, function(x) x@installed))

  cli_text("")
  cli_h3("Statistics")
  cli_dl(c(
    "Available tools" = "{.val {registry_count}}",
    "Installed tools" = "{.val {installed_count}}",
    "Working tools" = "{.val {installed_working}}"
  ))

  # Installed tools summary
  if (installed_count > 0) {
    cli_text("")
    cli_h3("Installed Tools")

    for (tool_name in names(object@tools)) {
      tool <- object@tools[[tool_name]]
      status_icon <- if (tool@installed) symbol$tick else symbol$cross
      status_color <- if (tool@installed) "green" else "red"

      version_text <- if (length(tool@version) > 0) tool@version else "unknown"
      cmd_count <- length(tool@commands)

      cli_li("{status_icon} {.strong {tool@name}} {.dim v{version_text}} {.dim ({cmd_count} commands)}")
    }
  } else {
    cli_text("")
    cli_alert_info("No tools installed yet. Use {.code sn_install_tool()} to install tools.")
  }

  # Available tools (if different from installed)
  available_not_installed <- setdiff(names(object@registry), names(object@tools))
  if (length(available_not_installed) > 0) {
    cli_text("")
    cli_h3("Available for Installation")

    # Show first few available tools
    show_count <- min(5, length(available_not_installed))
    for (i in 1:show_count) {
      tool_name <- available_not_installed[i]
      tool_config <- object@registry[[tool_name]]
      desc <- if (!is.null(tool_config$description)) {
        paste0(" - ", strtrim(tool_config$description, 50))
      } else {
        ""
      }
      cli_li("{.code {tool_name}}{desc}")
    }

    if (length(available_not_installed) > show_count) {
      remaining <- length(available_not_installed) - show_count
      cli_li("{.dim ... and {remaining} more}")
    }

    cli_text("")
    cli_alert_info("Use {.code sn_list_available_tools()} to see all available tools.")
  }

  cli_text("")
  invisible(object)
})

#' Show Method for ToolCall
#' @param object A ToolCall object
#' @export
setMethod("show", "ToolCall", function(object) {
  # Status-based styling
  status_icons <- list(
    "pending" = symbol$circle,
    "running" = symbol$arrow_right,
    "success" = symbol$tick,
    "failed" = symbol$cross,
    "cancelled" = symbol$times
  )

  status_colors <- list(
    "pending" = "yellow",
    "running" = "blue",
    "success" = "green",
    "failed" = "red",
    "cancelled" = "grey"
  )

  icon <- status_icons[[object@status]] %||% symbol$circle
  color <- status_colors[[object@status]] %||% "grey"

  # Header
  cli_rule("{icon} Tool Call: {.strong {object@tool@name}}::{.code {object@command}}")

  # Basic information
  cli_h3("Call Details")
  cli_dl(c(
    "Tool" = object@tool@name,
    "Command" = object@command,
    "Status" = paste0("{.val ", object@status, "}"),
    "Working directory" = "{.file {object@work_dir}}"
  ))

  # Arguments
  if (length(object@args) > 0) {
    cli_text("")
    cli_h3("Arguments")
    for (arg_name in names(object@args)) {
      arg_value <- object@args[[arg_name]]
      if (is.character(arg_value) && length(arg_value) == 1) {
        cli_li("{.strong {arg_name}}: {.val {arg_value}}")
      } else {
        cli_li("{.strong {arg_name}}: {.val {paste(arg_value, collapse = ', ')}}")
      }
    }
  }

  # Timing information
  if (length(object@start_time) > 0 || length(object@end_time) > 0) {
    cli_text("")
    cli_h3("Timing")
    timing_info <- c()

    if (length(object@start_time) > 0) {
      timing_info["Started"] <- format(object@start_time, "%Y-%m-%d %H:%M:%S")
    }

    if (length(object@end_time) > 0) {
      timing_info["Finished"] <- format(object@end_time, "%Y-%m-%d %H:%M:%S")

      if (length(object@start_time) > 0) {
        duration <- difftime(object@end_time, object@start_time, units = "secs")
        timing_info["Duration"] <- paste(round(as.numeric(duration), 2), "seconds")
      }
    }

    cli_dl(timing_info)
  }

  # Results
  if (object@status %in% c("success", "failed")) {
    cli_text("")
    cli_h3("Results")

    result_info <- c()
    if (length(object@return_code) > 0) {
      result_info["Return code"] <- as.character(object@return_code)
    }

    if (length(object@log_file) > 0 && nzchar(object@log_file)) {
      result_info["Log file"] <- "{.file {object@log_file}}"
    }

    if (length(result_info) > 0) {
      cli_dl(result_info)
    }

    # Show output/error snippets
    if (length(object@stdout) > 0 && nzchar(object@stdout)) {
      cli_text("")
      cli_h3("Output (first 200 chars)")
      output_snippet <- strtrim(object@stdout, 200)
      cli_code(output_snippet)
    }

    if (length(object@stderr) > 0 && nzchar(object@stderr)) {
      cli_text("")
      cli_h3("Error Output")
      error_snippet <- strtrim(object@stderr, 200)
      cli_code(error_snippet)
    }
  }

  cli_text("")
  invisible(object)
})
