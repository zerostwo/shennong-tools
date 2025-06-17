# S4 Class Definitions --------------------------------------------------------
# This file contains all S4 class definitions for the Shennong Tool System

#' Tool S4 Class
#'
#' Represents a bioinformatics tool with its configuration and metadata.
#' Based on YAML configuration files that define tools with their environments,
#' commands, and parameters. Now supports multiple versions within a single Tool object.
#'
#' @slot tool_name Character. Tool name from YAML (e.g., "samtools", "hisat2").
#' @slot versions Character. All available versions (e.g., c("1.19.2", "1.22", "dev")).
#' @slot default_version Character. Default version to use if not specified.
#' @slot description Character. Tool description.
#' @slot citation Character. Citation information for the tool.
#' @slot environment List. Environment specification including channels and dependencies.
#' @slot commands List. Available commands/subcommands for this tool.
#' @slot install_dates List. Installation dates for each version (named list).
#'
#' @export
setClass(
  "Tool",
  slots = c(
    tool_name = "character",
    versions = "character",
    default_version = "character",
    description = "character",
    citation = "character",
    environment = "list",
    commands = "list",
    install_dates = "list"
  ),
  prototype = list(
    tool_name = character(0),
    versions = character(0),
    default_version = character(0),
    description = character(0),
    citation = character(0),
    environment = list(),
    commands = list(),
    install_dates = list()
  )
)

#' Toolbox S4 Class
#'
#' Manages a collection of bioinformatics tools with version control.
#' The tools slot contains a simplified structure where each tool manages its own versions:
#' tools = list(
#'   tool1 = Tool_object,  # Tool object manages multiple versions internally
#'   tool2 = Tool_object
#' )
#'
#' @slot tools List. List of Tool objects organized by tool name.
#' @slot base_dir Character. Base directory for tool installations.
#' @slot mamba_path Character. Path to mamba/micromamba executable.
#'
#' @export
setClass(
  "Toolbox",
  slots = c(
    tools = "list",
    base_dir = "character",
    mamba_path = "character"
  ),
  prototype = list(
    tools = list(),
    base_dir = character(0),
    mamba_path = character(0)
  )
)

#' ToolCall S4 Class
#'
#' Represents a single tool invocation with inputs, outputs, and parameters.
#'
#' @slot tool Tool. The Tool object being called.
#' @slot command Character. The specific command/subcommand.
#' @slot rendered_command Character. The fully rendered command string.
#' @slot inputs List. Input files/objects with their roles.
#' @slot outputs List. Expected output files/objects.
#' @slot params List. Command parameters.
#' @slot work_dir Character. Working directory for execution.
#' @slot log_file Character. Path to log file.
#' @slot status Character. Execution status ("pending", "running", "success", "failed").
#' @slot return_code Numeric. Process return code.
#' @slot stdout Character. Captured stdout.
#' @slot stderr Character. Captured stderr.
#' @slot resources List. Resource usage including runtime, memory, and CPU.
#'
#' @export
setClass(
  "ToolCall",
  slots = c(
    tool = "Tool",
    command = "character",
    rendered_command = "character",
    inputs = "list",
    outputs = "list",
    params = "list",
    work_dir = "character",
    log_file = "character",
    status = "character",
    return_code = "numeric",
    stdout = "character",
    stderr = "character",
    resources = "list"
  ),
  prototype = list(
    tool = new("Tool"),
    command = character(0),
    rendered_command = character(0),
    inputs = list(),
    outputs = list(),
    params = list(),
    work_dir = getwd(),
    log_file = character(0),
    status = "pending",
    return_code = numeric(0),
    stdout = character(0),
    stderr = character(0),
    resources = list()
  )
)

# Validation Methods -----------------------------------------------------------

setValidity("Tool", function(object) {
  errors <- character()

  if (length(object@tool_name) == 0) {
    errors <- c(errors, "Tool name cannot be empty")
  }

  # Validate versions and default_version
  if (length(object@versions) > 0 && length(object@default_version) > 0) {
    if (!object@default_version %in% object@versions) {
      errors <- c(errors, "Default version must be one of the available versions")
    }
  }



  if (length(errors) == 0) TRUE else errors
})

setValidity("Toolbox", function(object) {
  errors <- character()

  if (length(object@base_dir) > 0 && !dir.exists(object@base_dir)) {
    errors <- c(errors, "Toolbox base_dir must exist")
  }

  if (length(object@mamba_path) > 0 && !file.exists(object@mamba_path)) {
    errors <- c(errors, "mamba_path must point to valid executable")
  }

  # Check tools structure: tools[[tool_name]] should be Tool objects
  if (length(object@tools) > 0) {
    for (tool_name in names(object@tools)) {
      tool_obj <- object@tools[[tool_name]]
      if (!inherits(tool_obj, "Tool")) {
        errors <- c(errors, paste("Tool", tool_name, "must be a Tool object"))
      }
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
  # Modern header with tool name and default version
  tool_title <- paste0(
    object@tool_name,
    if (length(object@default_version) > 0) paste0(" (default: v", object@default_version, ")") else ""
  )

  # Header rule with tool name
  cli_rule(left = paste0(symbol$package, " Tool: ", tool_title))

  # Versions information
  if (length(object@versions) > 0) {
    all_versions <- object@versions

    # Get installed versions dynamically using default toolbox
    tryCatch(
      {
        toolbox <- .get_default_toolbox()
        installed_versions <- .get_tool_installed_versions(object, toolbox@base_dir)
      },
      error = function(e) {
        # Fallback if no toolbox available (e.g., during testing)
        installed_versions <- character(0)
      }
    )

    version_display <- character()
    for (ver in all_versions) {
      if (ver %in% installed_versions) {
        version_display <- c(version_display, col_green(paste0(ver, " [installed]")))
      } else {
        version_display <- c(version_display, col_grey(paste0(ver, " [available]")))
      }
    }

    cli_text("{col_grey('Versions:')} {paste(version_display, collapse = ' ')}")

    # Installation summary
    if (length(installed_versions) > 0) {
      cli_text("{col_grey('Installed:')} {col_green(length(installed_versions))} of {length(all_versions)} versions")
    } else {
      cli_text("{col_grey('Status:')} {col_grey('No versions installed')}")
    }
  }

  # Description
  if (length(object@description) > 0 && nzchar(object@description)) {
    cli_text("{col_grey('Description:')} {object@description}")
  }

  # Commands section
  if (length(object@commands) > 0) {
    commands <- names(object@commands)
    if (length(commands) <= 5) {
      # Show all commands if few
      commands_text <- paste(commands, collapse = ", ")
      cli_text("{col_grey('Commands:')} {col_cyan(commands_text)}")
    } else {
      # Show first few + count if many
      shown_commands <- paste(commands[1:3], collapse = ", ")
      remaining <- length(commands) - 3
      cli_text("{col_grey('Commands:')} {col_cyan(shown_commands)} {col_silver(paste0('(+', remaining, ' more)'))}")
    }
  }

  # Citation if available
  if (length(object@citation) > 0 && nzchar(object@citation)) {
    citation_preview <- if (nchar(object@citation) > 80) {
      paste0(substr(object@citation, 1, 77), "...")
    } else {
      object@citation
    }
    cli_text("{col_grey('Citation:')} {col_silver(citation_preview)}")
  }

  # Environment info (brief)
  if (length(object@environment) > 0) {
    # Show conda channels if available
    if (!is.null(object@environment$channels)) {
      channels <- paste(object@environment$channels, collapse = ", ")
      cli_text("{col_grey('Channels:')} {col_blue(channels)}")
    }

    # Show number of dependencies
    if (!is.null(object@environment$dependencies)) {
      dep_count <- length(object@environment$dependencies)
      cli_text("{col_grey('Dependencies:')} {col_silver(dep_count)} packages")
    }
  }

  cli_rule()
})

#' Show Method for Toolbox
#' @param object A Toolbox object
#' @export
setMethod("show", "Toolbox", function(object) {
  # Header with modern styling
  cli_rule(
    left = paste0(symbol$package, " ShennongTools Toolbox"),
    right = paste0(length(object@tools), " tools")
  )

  # Configuration section with icons
  cli_h2("Configuration")
  cli_text("  {symbol$folder} Base Directory: {.path {object@base_dir}}")
  cli_text("  {symbol$gear} Mamba Path: {.path {object@mamba_path}}")

  # Tools summary with statistics
  if (length(object@tools) > 0) {
    total_tools <- length(object@tools)
    total_versions <- 0
    installed_count <- 0

    # Count versions and installed versions
    for (tool_name in names(object@tools)) {
      tool_obj <- object@tools[[tool_name]]
      total_versions <- total_versions + length(tool_obj@versions)
      installed_versions <- .get_tool_installed_versions(tool_obj, object@base_dir)
      installed_count <- installed_count + length(installed_versions)
    }

    cli_h2("Tools Overview")
    cli_text("  {symbol$info} Total: {col_cyan(total_tools)} tools with {col_blue(total_versions)} versions")
    cli_text("  {symbol$tick} Installed: {col_green(installed_count)} versions ready to use")
    cli_text("  {symbol$circle} Available: {col_grey(total_versions - installed_count)} versions can be installed")

    cli_h2("Available Tools")

    # Group tools by installation status for better organization
    installed_tools <- character()
    available_tools <- character()

    sorted_tools <- names(object@tools)[order(names(object@tools))]

    for (tool_name in sorted_tools) {
      tool_obj <- object@tools[[tool_name]]
      all_versions <- tool_obj@versions
      installed_versions <- .get_tool_installed_versions(tool_obj, object@base_dir)
      default_version <- if (length(tool_obj@default_version) > 0) tool_obj@default_version else all_versions[1]

      # Create clean version display
      if (length(installed_versions) > 0) {
        # Tool has installed versions
        version_display <- if (length(all_versions) == 1) {
          col_green(paste0("v", all_versions[1]))
        } else {
          paste0(
            col_green(paste0("v", default_version)),
            col_grey(paste0(" (+", length(all_versions) - 1, " more)"))
          )
        }
        status_icon <- col_green(symbol$tick)
        installed_tools <- c(
          installed_tools,
          paste0("  ", status_icon, " ", col_cyan(sprintf("%-12s", tool_name)), " ", version_display)
        )
      } else {
        # Tool not installed
        version_display <- if (length(all_versions) == 1) {
          col_grey(paste0("v", all_versions[1]))
        } else {
          paste0(
            col_grey(paste0("v", default_version)),
            col_grey(paste0(" (+", length(all_versions) - 1, " more)"))
          )
        }
        status_icon <- col_grey(symbol$circle)
        available_tools <- c(
          available_tools,
          paste0("  ", status_icon, " ", col_silver(sprintf("%-12s", tool_name)), " ", version_display)
        )
      }
    }

    # Display installed tools first
    if (length(installed_tools) > 0) {
      cli_text(col_green("  ✓ Installed ({length(installed_tools)} tools):"))
      for (tool_line in installed_tools) {
        cli_text(tool_line)
      }
      cli_text()
    }

    # Then display available tools
    if (length(available_tools) > 0) {
      cli_text(col_grey("  ○ Available ({length(available_tools)} tools):"))
      for (tool_line in available_tools) {
        cli_text(tool_line)
      }
    }
  } else {
    cli_h2("Tools")
    cli_text("  {col_grey('No tools configured')}")
    cli_text("  {col_silver('Use')} {.code sn_add_tool()} {col_silver('to add tools')}")
  }

  # Footer with helpful commands
  cli_rule()
  cli_text("{col_silver('Use')} {.code sn_get_toolbox_info()} {col_silver('for detailed information')}")
  cli_text("{col_silver('Use')} {.code sn_run('tool_name', 'command', ...)} {col_silver('to execute tools')}")
  cli_rule()
})

#' Show Method for ToolCall
#' @param object A ToolCall object
#' @export
setMethod("show", "ToolCall", function(object) {
  # Header with modern formatting
  cli_rule(left = paste0(symbol$play, " ToolCall: ", object@tool@tool_name, "::", object@command))

  # Status with colored indicators
  status_icon <- switch(object@status,
    "success" = col_green(symbol$tick),
    "failed" = col_red(symbol$cross),
    "running" = col_yellow(symbol$continue),
    "pending" = col_grey(symbol$circle),
    "cancelled" = col_red(symbol$cross),
    col_grey(symbol$circle)
  )

  status_color <- switch(object@status,
    "success" = col_green,
    "failed" = col_red,
    "running" = col_yellow,
    "pending" = col_grey,
    "cancelled" = col_red,
    col_grey
  )

  cli_text("{col_grey('Status:')} {status_icon} {status_color(toupper(object@status))}")

  if (length(object@return_code) > 0) {
    code_color <- if (object@return_code == 0) col_green else col_red
    cli_text("{col_grey('Exit Code:')} {code_color(object@return_code)}")
  }

  # Resource information with better formatting
  if (length(object@resources) > 0) {
    resource_parts <- character(0)

    if (!is.null(object@resources$runtime_seconds) && !is.na(object@resources$runtime_seconds)) {
      runtime <- round(object@resources$runtime_seconds, 2)
      resource_parts <- c(resource_parts, paste0(col_cyan("Runtime: "), runtime, "s"))
    }

    if (!is.null(object@resources$memory_mb) && !is.na(object@resources$memory_mb)) {
      memory <- round(object@resources$memory_mb, 1)
      resource_parts <- c(resource_parts, paste0(col_blue("Memory: "), memory, "MB"))
    }

    if (!is.null(object@resources$cpu_percent) && !is.na(object@resources$cpu_percent)) {
      resource_parts <- c(resource_parts, paste0(col_yellow("CPU: "), object@resources$cpu_percent, "%"))
    }

    if (length(resource_parts) > 0) {
      cli_text("{col_grey('Resources:')} {paste(resource_parts, collapse = ' | ')}")
    }
  }

  # Timing information
  if (!is.null(object@resources$start_time) && !is.null(object@resources$end_time)) {
    start_time <- format(object@resources$start_time, "%Y-%m-%d %H:%M:%S")
    end_time <- format(object@resources$end_time, "%Y-%m-%d %H:%M:%S")
    cli_text("{col_grey('Start Time:')} {start_time}")
    cli_text("{col_grey('End Time:')} {end_time}")
  }

  # Working directory
  cli_text("{col_grey('Work Dir:')} {.path {object@work_dir}}")

  # Command information
  if (length(object@rendered_command) > 0 && nzchar(object@rendered_command)) {
    cli_text("{col_grey('Command:')} {.code {object@rendered_command}}")
  }

  # Log file if available
  if (length(object@log_file) > 0 && nzchar(object@log_file)) {
    cli_text("{col_grey('Log File:')} {.path {object@log_file}}")
  }

  # Inputs section
  if (length(object@inputs) > 0) {
    cli_h3(paste0(symbol$file, " Inputs"))
    for (name in names(object@inputs)) {
      value <- object@inputs[[name]]
      if (nchar(value) > 50) {
        value <- paste0(substr(value, 1, 47), "...")
      }
      cli_text("{col_cyan(name)}: {.path {value}}")
    }
  }

  # Outputs section
  if (length(object@outputs) > 0) {
    cli_h3(paste0(symbol$arrow_right, " Outputs"))
    for (name in names(object@outputs)) {
      value <- object@outputs[[name]]
      if (nchar(value) > 50) {
        value <- paste0(substr(value, 1, 47), "...")
      }
      # Check if output file exists
      file_status <- if (file.exists(value)) {
        file_size <- file.size(value)
        size_str <- if (file_size > 1024^3) {
          paste0(round(file_size / 1024^3, 1), "GB")
        } else if (file_size > 1024^2) {
          paste0(round(file_size / 1024^2, 1), "MB")
        } else if (file_size > 1024) {
          paste0(round(file_size / 1024, 1), "KB")
        } else {
          paste0(file_size, "B")
        }
        col_green(paste0(" (", size_str, ")"))
      } else {
        col_red(" (missing)")
      }
      cli_text("{col_green(name)}: {.path {value}}{file_status}")
    }
  }

  # Parameters section (excluding inputs and outputs)
  param_names <- names(object@params)
  input_names <- names(object@inputs)
  output_names <- names(object@outputs)
  pure_params <- param_names[!param_names %in% c(input_names, output_names)]

  if (length(pure_params) > 0) {
    cli_h3(paste0(symbol$gear, " Parameters"))
    for (name in pure_params) {
      value <- object@params[[name]]
      if (is.character(value) && nchar(value) > 30) {
        value <- paste0(substr(value, 1, 27), "...")
      }
      cli_text("{col_silver(name)}: {.val {value}}")
    }
  }

  cli_rule()
})

# Helper Functions for Dynamic Installation Checking -------------------------

#' Check if Tool Version is Installed (Dynamic)
#' @keywords internal
.is_installed <- function(tool, version = NULL, base_dir = NULL) {
  if (is.null(base_dir)) {
    toolbox <- .get_default_toolbox()
    base_dir <- toolbox@base_dir
  }

  version <- version %||% tool@default_version

  bin_dir <- file.path(base_dir, tool@tool_name, version, "bin")

  if (!dir.exists(bin_dir)) {
    return(FALSE)
  }

  binaries <- vapply(tool@commands, function(cmd) cmd$binary, character(1))
  binary_paths <- file.path(bin_dir, binaries)
  exists_vec <- file.exists(binary_paths)

  if (!any(exists_vec)) {
    return(FALSE)
  }

  return(TRUE)
}


#' Diagnose Tool Installation Issues (Detailed)
#' @keywords internal
.diagnose_tool_installation <- function(tool, version, base_dir, show_details = TRUE) {
  tool_name <- tool@tool_name

  # Try both naming schemes
  install_path <- file.path(base_dir, tool_name, version)

  if (!dir.exists(install_path)) {
    if (show_details) {
      cli_alert_danger("Installation directory not found:")
      cli_text("  - Tried: {install_path}")
    }
    return(list(installed = FALSE, path = NULL, issues = "Directory not found"))
  }

  issues <- character(0)

  # Check essential conda environment structure
  essential_dirs <- c("conda-meta", "bin", "lib")
  missing_dirs <- character(0)
  for (dir_name in essential_dirs) {
    dir_path <- file.path(install_path, dir_name)
    if (!dir.exists(dir_path)) {
      missing_dirs <- c(missing_dirs, dir_name)
    }
  }

  if (length(missing_dirs) > 0) {
    issues <- c(issues, paste("Missing directories:", paste(missing_dirs, collapse = ", ")))
    if (show_details) {
      cli_alert_warning("Missing essential directories: {paste(missing_dirs, collapse = ', ')}")
    }
  }

  # Check for actual binaries from tool commands
  missing_binaries <- character(0)
  found_binaries <- character(0)

  if (length(tool@commands) > 0) {
    bin_dir <- file.path(install_path, "bin")

    if (dir.exists(bin_dir)) {
      for (cmd_name in names(tool@commands)) {
        cmd_config <- tool@commands[[cmd_name]]
        binary_name <- cmd_config$binary %||% tool_name
        binary_path <- file.path(bin_dir, binary_name)

        if (file.exists(binary_path)) {
          found_binaries <- c(found_binaries, binary_name)
        } else {
          missing_binaries <- c(missing_binaries, binary_name)
        }
      }

      if (show_details) {
        if (length(found_binaries) > 0) {
          cli_alert_success("Found binaries: {paste(found_binaries, collapse = ', ')}")
        }
        if (length(missing_binaries) > 0) {
          cli_alert_warning("Missing binaries: {paste(missing_binaries, collapse = ', ')}")
          # List all available binaries in bin directory
          all_bins <- list.files(bin_dir)
          if (length(all_bins) > 0) {
            cli_text("Available binaries: {paste(head(all_bins, 10), collapse = ', ')}{if(length(all_bins) > 10) '...' else ''}")
          } else {
            cli_text("No binaries found in bin directory")
          }
        }
      }
    } else {
      issues <- c(issues, "bin directory not found")
      if (show_details) {
        cli_alert_danger("bin directory not found at {bin_dir}")
      }
    }

    if (length(missing_binaries) > 0) {
      issues <- c(issues, paste("Missing binaries:", paste(missing_binaries, collapse = ", ")))
    }
  }

  # Check conda-meta for package records
  if (dir.exists(file.path(install_path, "conda-meta"))) {
    meta_files <- list.files(file.path(install_path, "conda-meta"), pattern = "\\.json$")
    if (show_details) {
      cli_text("Conda packages installed: {length(meta_files)}")
      if (length(meta_files) > 0) {
        # Show first few package names
        package_names <- sub("-[^-]+-[^-]+\\.json$", "", meta_files)
        cli_text("Sample packages: {paste(head(package_names, 5), collapse = ', ')}{if(length(package_names) > 5) '...' else ''}")
      }
    }
    if (length(meta_files) < 3) {
      issues <- c(issues, "Very few conda packages installed")
    }
  }

  is_installed <- length(issues) == 0 && length(found_binaries) > 0

  return(list(
    installed = is_installed,
    path = actual_path,
    issues = if (length(issues) > 0) issues else NULL,
    found_binaries = found_binaries,
    missing_binaries = missing_binaries
  ))
}

#' Get Installed Versions (Dynamic)
#' @keywords internal
.get_tool_installed_versions <- function(tool, base_dir) {
  tool_name <- tool@tool_name
  versions <- tool@versions

  installed_versions <- character(0)

  for (version in versions) {
    if (.is_installed(tool, version, base_dir)) {
      installed_versions <- c(installed_versions, version)
    }
  }

  return(installed_versions)
}

# Getter/Setter Functions -----------------------------------------------------

#' Get Tool Name
#'
#' Safely get tool name instead of using direct @ access.
#'
#' @param tool Tool object
#' @return Character. Tool name
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: tool@tool_name
#' tool_name <- sn_get_tool_name(tool)
#' }
sn_get_tool_name <- function(tool) {
  if (!inherits(tool, "Tool")) {
    cli_abort("Object must be a Tool")
  }
  return(tool@tool_name)
}

#' Get Tool Default Version
#'
#' Safely get tool default version instead of using direct @ access.
#'
#' @param tool Tool object
#' @return Character. Default tool version
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Get the default version for a tool
#' version <- sn_get_tool_version(tool)
#' }
sn_get_tool_version <- function(tool) {
  if (!inherits(tool, "Tool")) {
    cli_abort("Object must be a Tool")
  }
  return(tool@default_version)
}

#' Get All Tool Versions
#'
#' Get all available versions for a tool.
#'
#' @param tool Tool object
#' @return Character. All available versions
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Get all available versions
#' versions <- sn_get_tool_versions(tool)
#' }
sn_get_tool_versions <- function(tool) {
  if (!inherits(tool, "Tool")) {
    cli_abort("Object must be a Tool")
  }
  return(tool@versions)
}

#' Get Installed Versions
#'
#' Get all installed versions for a tool.
#'
#' @param tool Tool object
#' @param base_dir Character. Base directory for installations (optional, will use default toolbox if not provided)
#' @return Character. Installed versions
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Get all installed versions
#' installed <- sn_get_installed_versions(tool)
#' }
sn_get_installed_versions <- function(tool, base_dir = NULL) {
  if (!inherits(tool, "Tool")) {
    cli_abort("Object must be a Tool")
  }

  # Get base_dir from default toolbox if not provided
  if (is.null(base_dir)) {
    toolbox <- .get_default_toolbox()
    base_dir <- toolbox@base_dir
  }

  return(.get_tool_installed_versions(tool, base_dir))
}

#' Get Tool Commands
#'
#' Safely get tool command names instead of using direct @ access.
#'
#' @param tool Tool object
#' @return Character. Available command names
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: names(tool@commands)
#' commands <- sn_get_tool_commands(tool)
#' if ("align" %in% commands) {
#'   # Tool supports align command
#' }
#' }
sn_get_tool_commands <- function(tool) {
  if (!inherits(tool, "Tool")) {
    cli_abort("Object must be a Tool")
  }
  return(names(tool@commands))
}

#' Get Toolbox Tools
#'
#' Safely get toolbox tool names instead of using direct @ access.
#'
#' @param toolbox Toolbox object
#' @return Character. Available tool names
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: names(toolbox@tools)
#' tools <- sn_get_toolbox_tools(toolbox)
#' if ("samtools" %in% tools) {
#'   # Samtools is available in toolbox
#' }
#' }
sn_get_toolbox_tools <- function(toolbox) {
  if (!inherits(toolbox, "Toolbox")) {
    cli_abort("Object must be a Toolbox")
  }
  return(names(toolbox@tools))
}

#' Get Tool from Toolbox
#'
#' Safely get tool object from toolbox instead of using direct @ access.
#'
#' @param toolbox Toolbox object
#' @param tool_name Character. Tool name
#' @return Tool object
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: toolbox@tools[[tool_name]]
#' tool <- sn_get_tool(toolbox, "samtools")
#' }
sn_get_tool <- function(toolbox, tool_name) {
  if (!inherits(toolbox, "Toolbox")) {
    cli_abort("Object must be a Toolbox")
  }

  if (!tool_name %in% names(toolbox@tools)) {
    cli_abort("Tool {tool_name} not found in toolbox")
  }

  return(toolbox@tools[[tool_name]])
}

#' Get ToolCall Status
#'
#' Safely get toolcall execution status instead of using direct @ access.
#'
#' @param tool_call ToolCall object
#' @return Character. Execution status
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: tool_call@status
#' status <- sn_get_toolcall_status(tool_call)
#' if (status == "success") {
#'   # Processing completed successfully
#' }
#' }
sn_get_toolcall_status <- function(tool_call) {
  if (!inherits(tool_call, "ToolCall")) {
    cli_abort("Object must be a ToolCall")
  }
  return(tool_call@status)
}

#' Get ToolCall Output Files
#'
#' Safely get toolcall output files instead of using direct @ access.
#'
#' @param tool_call ToolCall object
#' @return List. Output files with their roles
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: tool_call@outputs
#' outputs <- sn_get_toolcall_outputs(tool_call)
#' output_file <- outputs$alignment
#' }
sn_get_toolcall_outputs <- function(tool_call) {
  if (!inherits(tool_call, "ToolCall")) {
    cli_abort("Object must be a ToolCall")
  }
  return(tool_call@outputs)
}

#' Get ToolCall Runtime
#'
#' Safely get toolcall runtime instead of using direct @ access.
#'
#' @param tool_call ToolCall object
#' @return Numeric. Runtime in seconds (NA if not available)
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: tool_call@resources$runtime_seconds
#' runtime <- sn_get_toolcall_runtime(tool_call)
#' if (!is.na(runtime)) {
#'   cat("Command took", runtime, "seconds to execute\n")
#' }
#' }
sn_get_toolcall_runtime <- function(tool_call) {
  if (!inherits(tool_call, "ToolCall")) {
    cli_abort("Object must be a ToolCall")
  }
  return(tool_call@resources$runtime_seconds %||% NA)
}

#' Check if ToolCall was Successful
#'
#' Safely check if toolcall completed successfully instead of using direct @ access.
#'
#' @param tool_call ToolCall object
#' @return Logical. TRUE if successful
#' @family accessor functions
#' @concept accessor functions
#' @export
#' @examples
#' \dontrun{
#' # Instead of: tool_call@status == "success"
#' if (sn_is_toolcall_success(tool_call)) {
#'   outputs <- sn_get_toolcall_outputs(tool_call)
#'   # Process the output files
#' }
#' }
sn_is_toolcall_success <- function(tool_call) {
  if (!inherits(tool_call, "ToolCall")) {
    cli_abort("Object must be a ToolCall")
  }
  return(tool_call@status == "success")
}

#' Diagnose Tool Installation
#'
#' Provides detailed diagnostic information about tool installation status,
#' including missing directories, binaries, and potential issues.
#'
#' @param tool_name Character. Name of the tool to diagnose.
#' @param version Character. Specific version to diagnose (optional).
#' @param base_dir Character. Base directory for installations (optional).
#' @return List with diagnostic information.
#' @family tool management
#' @concept tool management
#' @export
#' @examples
#' \dontrun{
#' # Diagnose a specific tool
#' sn_diagnose_tool("fastp")
#'
#' # Diagnose specific version
#' sn_diagnose_tool("fastp", "0.26.0")
#' }
sn_diagnose_tool <- function(tool_name, version = NULL, base_dir = NULL) {
  if (!is.character(tool_name) || length(tool_name) != 1 || nchar(tool_name) == 0) {
    cli_abort("tool_name must be a non-empty character string")
  }

  # Get or create default toolbox
  toolbox <- .get_default_toolbox()

  # Get base_dir from default toolbox if not provided
  if (is.null(base_dir)) {
    base_dir <- toolbox@base_dir
  }

  # Check if tool is in toolbox
  if (!tool_name %in% sn_get_toolbox_tools(toolbox)) {
    cli_alert_info("Tool {tool_name} not found in toolbox, adding it...")
    toolbox <- sn_add_tool(toolbox, tool_name, version = version, install = FALSE)
    .set_default_toolbox(toolbox)
  }

  # Get tool object
  tool <- sn_get_tool(toolbox, tool_name)
  available_versions <- sn_get_tool_versions(tool)

  # Determine version to diagnose
  if (is.null(version)) {
    if (length(available_versions) > 0) {
      version <- available_versions[1]
    } else {
      version <- sn_get_tool_version(tool)
    }
  }

  if (!version %in% available_versions) {
    cli_abort("Version {version} not found for {tool_name}. Available versions: {paste(available_versions, collapse = ', ')}")
  }

  # Display header
  cli_rule(
    left = paste0("Diagnosing Tool: ", tool_name),
    right = paste0("v", version)
  )

  # Run detailed diagnosis
  diagnosis <- .diagnose_tool_installation(tool, version, base_dir, show_details = TRUE)

  # Summary
  if (diagnosis$installed) {
    cli_alert_success("Tool {tool_name} v{version} is properly installed")
  } else {
    cli_alert_danger("Tool {tool_name} v{version} has installation issues")
    if (!is.null(diagnosis$issues)) {
      cli_text("Issues found:")
      for (issue in diagnosis$issues) {
        cli_text("  - {issue}")
      }
    }
  }

  cli_rule()

  invisible(diagnosis)
}
