#' Load Tool Registry from YAML Files
#'
#' This function loads all YAML tool definitions from the inst/tools directory
#' and builds a registry of available tools.
#'
#' @param tools_dir Character. Directory containing YAML tool definitions.
#'                 Defaults to package inst/tools directory.
#' @param refresh Logical. Whether to refresh the registry cache.
#'
#' @return Named list of tool configurations.
#' @export
sn_load_registry <- function(tools_dir = NULL, refresh = FALSE) {
  if (is.null(tools_dir)) {
    tools_dir <- system.file("tools", package = "ShennongTools")
    if (tools_dir == "") {
      tools_dir <- file.path("inst", "tools")
    }
  }

  if (!dir.exists(tools_dir)) {
    cli_abort("Tools directory not found: {tools_dir}")
  }

  # Find all YAML files
  yaml_files <- list.files(
    path = tools_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(yaml_files) == 0) {
    cli_warn("No YAML files found in {tools_dir}")
    return(list())
  }

  registry <- list()

  for (yaml_file in yaml_files) {
    tryCatch(
      {
        tool_config <- yaml::yaml.load_file(yaml_file)
        tool_name <- tool_config$tool_name

        if (is.null(tool_name)) {
          cli_warn("Tool config missing 'tool_name' in {basename(yaml_file)}")
          next
        }

        # Validate required fields
        .sn_validate_tool_config(tool_config, yaml_file)

        registry[[tool_name]] <- tool_config
        cli_alert_success("Loaded tool: {tool_name}")
      },
      error = function(e) {
        cli_alert_danger("Failed to load {basename(yaml_file)}: {e$message}")
      }
    )
  }

  cli_alert_info("Loaded {length(registry)} tools from registry")
  return(registry)
}

#' Validate Tool Configuration
#'
#' Internal function to validate a tool configuration loaded from YAML.
#'
#' @param config List. Tool configuration from YAML.
#' @param yaml_file Character. Path to YAML file for error reporting.
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
.sn_validate_tool_config <- function(config, yaml_file) {
  required_fields <- c("tool_name", "description", "package", "commands")

  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    cli_abort(
      "Missing required fields in {basename(yaml_file)}: {paste(missing_fields, collapse = ', ')}"
    )
  }

  # Validate package structure
  if (is.null(config$package$name)) {
    cli_abort("Missing package.name in {basename(yaml_file)}")
  }

  # Validate commands structure
  if (length(config$commands) == 0) {
    cli_abort("No commands defined in {basename(yaml_file)}")
  }

  for (cmd_name in names(config$commands)) {
    cmd <- config$commands[[cmd_name]]
    cmd_required <- c("binary", "args_template", "inputs", "outputs")
    cmd_missing <- setdiff(cmd_required, names(cmd))

    if (length(cmd_missing) > 0) {
      cli_abort(
        "Command '{cmd_name}' missing required fields in {basename(yaml_file)}: {paste(cmd_missing, collapse = ', ')}"
      )
    }

    # Validate that template variables are defined in inputs
    .sn_validate_template_vars(cmd, cmd_name, yaml_file)

    # Validate input definitions
    .sn_validate_input_definitions(cmd$inputs, cmd_name, yaml_file)
  }
}

#' Validate Template Variables
#'
#' Internal function to validate that all variables used in args_template
#' are defined in the inputs or outputs section.
#'
#' @param cmd List. Command configuration.
#' @param cmd_name Character. Command name for error reporting.
#' @param yaml_file Character. YAML file path for error reporting.
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
.sn_validate_template_vars <- function(cmd, cmd_name, yaml_file) {
  if (is.null(cmd$args_template)) {
    return(NULL)
  }

  # Extract variables from template
  template_vars <- sn_get_template_vars(cmd$args_template)
  input_names <- names(cmd$inputs %||% list())
  output_names <- names(cmd$outputs %||% list())

  # Combine available variable names
  available_vars <- c(input_names, output_names)

  # Check for undefined variables
  undefined_vars <- setdiff(template_vars, available_vars)
  if (length(undefined_vars) > 0) {
    cli_abort(
      "Command '{cmd_name}' in {basename(yaml_file)} uses undefined variables in template: {paste(undefined_vars, collapse = ', ')}. Available variables from inputs: {paste(input_names, collapse = ', ')}. Available variables from outputs: {paste(output_names, collapse = ', ')}"
    )
  }
}

#' Validate Input Definitions
#'
#' Internal function to validate input parameter definitions.
#'
#' @param inputs List. Input definitions.
#' @param cmd_name Character. Command name for error reporting.
#' @param yaml_file Character. YAML file path for error reporting.
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
.sn_validate_input_definitions <- function(inputs, cmd_name, yaml_file) {
  valid_types <- c("file", "string", "integer", "flag", "list")

  for (input_name in names(inputs)) {
    input_def <- inputs[[input_name]]

    # Check required type field
    if (is.null(input_def$type)) {
      cli_abort(
        "Input '{input_name}' in command '{cmd_name}' in {basename(yaml_file)} missing 'type' field"
      )
    }

    # Check valid type
    if (!input_def$type %in% valid_types) {
      cli_abort(
        "Input '{input_name}' in command '{cmd_name}' in {basename(yaml_file)} has invalid type '{input_def$type}'. Valid types: {paste(valid_types, collapse = ', ')}"
      )
    }

    # Check required field is logical if present
    if (!is.null(input_def$required) && !is.logical(input_def$required)) {
      cli_abort(
        "Input '{input_name}' in command '{cmd_name}' in {basename(yaml_file)} 'required' field must be TRUE or FALSE"
      )
    }
  }
}

#' Get Tool Configuration
#'
#' Retrieve configuration for a specific tool from the registry.
#'
#' @param tool_name Character. Name of the tool.
#' @param registry List. Tool registry (optional, will load if not provided).
#'
#' @return List. Tool configuration.
#' @export
sn_get_tool_config <- function(tool_name, registry = NULL) {
  if (is.null(registry)) {
    registry <- sn_load_registry()
  }

  if (!tool_name %in% names(registry)) {
    available_tools <- paste(names(registry), collapse = ", ")
    cli_abort("Tool '{tool_name}' not found in registry. Available tools: {available_tools}")
  }

  return(registry[[tool_name]])
}

#' List Available Tools
#'
#' List all tools available in the registry with their descriptions.
#'
#' @param registry List. Tool registry (optional, will load if not provided).
#' @param detailed Logical. Whether to show detailed information.
#'
#' @return Data frame with tool information.
#' @export
sn_list_available_tools <- function(registry = NULL, detailed = FALSE) {
  if (is.null(registry)) {
    registry <- sn_load_registry()
  }

  if (length(registry) == 0) {
    cli_alert_info("No tools found in registry")
    return(data.frame())
  }

  tool_info <- data.frame(
    tool = names(registry),
    description = sapply(registry, function(x) x$description %||% ""),
    commands = sapply(registry, function(x) paste(names(x$commands), collapse = ", ")),
    stringsAsFactors = FALSE
  )

  if (detailed) {
    tool_info$citation <- sapply(registry, function(x) x$citation %||% "")
    tool_info$package_name <- sapply(registry, function(x) x$package$name %||% "")
    tool_info$channel <- sapply(registry, function(x) x$package$channel %||% "")
  }

  return(tool_info)
}
