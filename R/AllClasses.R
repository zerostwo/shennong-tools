# S4 Class Definitions --------------------------------------------------------
# This file contains all S4 class definitions for the Shennong Tool System

#' Tool S4 Class
#'
#' Represents a bioinformatics tool with its configuration and metadata.
#' Based on YAML configuration files that define tools with their environments,
#' commands, and parameters.
#'
#' @slot tool_name Character. Tool name from YAML (e.g., "samtools", "hisat2").
#' @slot version Character. Tool version (e.g., "1.22", "dev").
#' @slot description Character. Tool description.
#' @slot citation Character. Citation information for the tool.
#' @slot environment List. Environment specification including channels and dependencies.
#' @slot commands List. Available commands/subcommands for this tool.
#' @slot installed Logical. Whether the tool is currently installed.

#' @slot install_date POSIXct. When the tool was installed.
#'
#' @export
setClass(
  "Tool",
  slots = c(
    tool_name = "character",
    version = "character",
    description = "character",
    citation = "character",
    environment = "list",
    commands = "list",
    installed = "logical",
    install_date = "POSIXct"
  ),
  prototype = list(
    tool_name = character(0),
    version = character(0),
    description = character(0),
    citation = character(0),
    environment = list(),
    commands = list(),
    installed = FALSE,
    install_date = as.POSIXct(character(0))
  )
)

#' Toolbox S4 Class
#'
#' Manages a collection of bioinformatics tools with version control.
#' The tools slot contains a nested list structure:
#' tools = list(
#'   tool1 = list(
#'     version1 = Tool_object,
#'     version2 = Tool_object
#'   ),
#'   tool2 = list(
#'     version1 = Tool_object
#'   )
#' )
#'
#' @slot tools List. Nested list of Tool objects organized by tool name and version.
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
    work_dir = ".",
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

  # Validation for installed tools can be done when needed

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

  # Check tools structure: tools[[tool_name]][[version]] should be Tool objects
  if (length(object@tools) > 0) {
    for (tool_name in names(object@tools)) {
      tool_versions <- object@tools[[tool_name]]
      if (!is.list(tool_versions)) {
        errors <- c(errors, paste("Tool", tool_name, "must contain a list of versions"))
        next
      }

      for (version in names(tool_versions)) {
        if (!inherits(tool_versions[[version]], "Tool")) {
          errors <- c(errors, paste("Tool", tool_name, "version", version, "must be a Tool object"))
        }
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
  # Modern header with tool name and version
  tool_title <- paste0(object@tool_name, 
                      if (length(object@version) > 0) paste0(" v", object@version) else "")
  
  # Status icon and color
  if (object@installed) {
    status_icon <- col_green(symbol$tick)
    status_text <- col_green("INSTALLED")
  } else {
    status_icon <- col_grey(symbol$circle)
    status_text <- col_grey("NOT INSTALLED")
  }
  
  # Header rule with tool name
  cli_rule(left = paste0(symbol$package, " Tool: ", tool_title))
  
  # Status line
  cli_text("{col_grey('Status:')} {status_icon} {status_text}")
  
  # Description
  if (length(object@description) > 0 && nzchar(object@description)) {
    cli_text("{col_grey('Description:')} {object@description}")
  }
  
  # Installation date
  if (object@installed && length(object@install_date) > 0) {
    install_date <- format(object@install_date, "%Y-%m-%d %H:%M:%S")
    cli_text("{col_grey('Installed:')} {install_date}")
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
  # Header with toolbox symbol
  cli_rule(left = paste0(symbol$box, " Toolbox"))
  
  # Basic configuration
  cli_text("{col_grey('Mamba Path:')} {.path {object@mamba_path}}")
  cli_text("{col_grey('Base Directory:')} {.path {object@base_dir}}")
  
  # Tools summary
  if (length(object@tools) > 0) {
    total_tools <- length(object@tools)
    total_versions <- sum(sapply(object@tools, length))
    installed_count <- 0
    
    # Count installed versions
    for (tool_name in names(object@tools)) {
      for (version in names(object@tools[[tool_name]])) {
        if (object@tools[[tool_name]][[version]]@installed) {
          installed_count <- installed_count + 1
        }
      }
    }
    
    # Summary statistics
    cli_text("{col_cyan('Tools:')} {total_tools} tools, {total_versions} versions ({col_green(installed_count)} installed)")
    
    # Tools listing - more compact
    sorted_tools <- names(object@tools)[order(names(object@tools))]
    
    for (tool_name in sorted_tools) {
      versions <- names(object@tools[[tool_name]])
      
      # Create version info with status indicators
      version_infos <- character()
      for (version in versions) {
        tool_obj <- object@tools[[tool_name]][[version]]
        if (tool_obj@installed) {
          version_info <- col_green(paste0(version, " ✓"))
        } else {
          version_info <- col_grey(paste0(version, " ○"))
        }
        version_infos <- c(version_infos, version_info)
      }
      
      # Tool line with versions - more compact format
      versions_text <- paste(version_infos, collapse = " ")
      cli_text("  {col_cyan(tool_name)} ({versions_text})")
    }
    
  } else {
    cli_text("{col_grey('No tools installed')}")
  }
  
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
