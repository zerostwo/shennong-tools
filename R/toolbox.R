#' Create Toolbox
#'
#' Create a new Toolbox object with registry loaded and tools discovered.
#'
#' @param base_dir Character. Base directory for tool installations.
#' @param tools_dir Character. Directory containing YAML tool configurations.
#' @param mamba_path Character. Path to mamba/micromamba executable.
#' @param refresh Logical. Whether to refresh the registry cache.
#'
#' @return Toolbox object.
#' @export
#'
#' @examples
#' \dontrun{
#' toolbox <- sn_create_toolbox()
#' toolbox
#' }
sn_create_toolbox <- function(base_dir = NULL,
                              tools_dir = NULL,
                              mamba_path = NULL,
                              refresh = FALSE) {
  # Set default base directory
  if (is.null(base_dir)) {
    base_dir <- tools::R_user_dir(package = "shennong-tools", which = "data")
  }

  # Ensure base directory exists
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }

  # Find mamba if not provided
  mamba_path <- mamba_path %||% sn_check_mamba()

  # Load registry
  registry <- sn_load_registry(tools_dir = tools_dir, refresh = refresh)

  # Create toolbox object
  toolbox <- new("Toolbox",
    base_dir = base_dir,
    mamba_path = mamba_path,
    registry = registry,
    tools = list(),
    config = list(
      auto_install = TRUE,
      default_channel = "bioconda"
    )
  )

  # Discover installed tools
  toolbox <- .sn_discover_installed_tools(toolbox)

  return(toolbox)
}

#' Get Global Toolbox
#'
#' Get the global cached toolbox instance.
#'
#' @param refresh Logical. Whether to refresh the toolbox.
#'
#' @return Toolbox object.
#' @export
sn_get_toolbox <- function(refresh = FALSE) {
  if (refresh || !exists("toolbox", envir = .sn_global_toolbox)) {
    toolbox <- sn_create_toolbox(refresh = refresh)
    assign("toolbox", toolbox, envir = .sn_global_toolbox)
  }

  get("toolbox", envir = .sn_global_toolbox)
}

#' Check Tool Availability and Auto-install
#'
#' Check if a tool is available and install it if needed.
#'
#' @param tool_name Character. Name of the tool.
#' @param toolbox Toolbox. Toolbox object (optional).
#' @param force_install Logical. Whether to force reinstallation.
#'
#' @return Logical. TRUE if tool is available after check/install.
#' @export
sn_ensure_tool <- function(tool_name, toolbox = NULL, force_install = FALSE) {
  if (is.null(toolbox)) {
    toolbox <- sn_get_toolbox()
  }

  # Check if tool is already installed
  if (!force_install && tool_name %in% names(toolbox@tools)) {
    tool <- toolbox@tools[[tool_name]]
    if (tool@installed) {
      return(TRUE)
    }
  }

  # Check if tool is in registry
  if (!tool_name %in% names(toolbox@registry)) {
    cli_alert_danger("Tool '{tool_name}' not found in registry")
    return(FALSE)
  }

  # Auto-install if enabled
  if (toolbox@config$auto_install) {
    cli_alert_info("Tool '{tool_name}' not installed. Installing automatically...")

    tryCatch(
      {
        result <- sn_install_tool(tool_name, force = force_install)
        if (result$success) {
          # Refresh toolbox to detect newly installed tool
          assign("toolbox", sn_create_toolbox(refresh = TRUE), envir = .sn_global_toolbox)
          cli_alert_success("Tool '{tool_name}' installed successfully")
          return(TRUE)
        } else {
          cli_alert_danger("Failed to install tool '{tool_name}': {result$message}")
          return(FALSE)
        }
      },
      error = function(e) {
        cli_alert_danger("Error installing tool '{tool_name}': {e$message}")
        return(FALSE)
      }
    )
  } else {
    cli_alert_warning("Tool '{tool_name}' not installed. Set auto_install = TRUE or run sn_install_tool('{tool_name}')")
    return(FALSE)
  }
}

#' Discover Installed Tools
#'
#' Scan the base directory for installed tools and update toolbox.
#'
#' @param toolbox Toolbox object.
#'
#' @return Updated Toolbox object.
#' @keywords internal
.sn_discover_installed_tools <- function(toolbox) {
  if (!dir.exists(toolbox@base_dir)) {
    return(toolbox)
  }

  # Scan for tool directories
  tool_dirs <- list.dirs(toolbox@base_dir, full.names = FALSE, recursive = FALSE)

  for (tool_name in tool_dirs) {
    if (tool_name %in% names(toolbox@registry)) {
      tool_config <- toolbox@registry[[tool_name]]

      # Look for version subdirectories
      tool_path <- file.path(toolbox@base_dir, tool_name)
      versions <- list.dirs(tool_path, full.names = FALSE, recursive = FALSE)

      if (length(versions) > 0) {
        # Use latest version
        latest_version <- versions[order(versions, decreasing = TRUE)][1]
        env_path <- file.path(tool_path, latest_version)

        # Verify installation
        binary_name <- tool_config$package$name %||% tool_name
        binary_path <- file.path(env_path, "bin", binary_name)

        is_installed <- file.exists(binary_path)

        # Create Tool object
        citation_list <- if (is.character(tool_config$citation)) {
          list(doi = tool_config$citation)
        } else {
          tool_config$citation %||% list()
        }

        tool <- new("Tool",
          name = tool_name,
          version = latest_version,
          package_name = tool_config$package$name %||% tool_name,
          binary_name = binary_name,
          env_path = env_path,
          channel = tool_config$package$channel %||% "bioconda",
          description = tool_config$description %||% "",
          commands = tool_config$commands %||% list(),
          citation = citation_list,
          installed = is_installed,
          install_date = as.POSIXct(file.mtime(env_path))
        )

        toolbox@tools[[tool_name]] <- tool
      }
    }
  }

  return(toolbox)
}
