#' Help Function for Tool Commands
#'
#' Display help information for a specific tool command.
#'
#' @param tool Character. Tool name.
#' @param command Character. Command name (optional).
#' @param registry List. Tool registry (optional).
#'
#' @return Invisible NULL (prints help to console).
#' @export
sn_help <- function(tool, command = NULL, registry = NULL) {
  if (is.null(registry)) {
    registry <- sn_load_registry()
  }

  tool_config <- sn_get_tool_config(tool, registry)

  cli_h1("Tool: {tool}")
  cli_alert_info("Description: {tool_config$description}")

  if (!is.null(tool_config$citation)) {
    cli_alert_info("Citation: {tool_config$citation}")
  }

  if (is.null(command)) {
    # Show all commands
    cli_h2("Available commands:")
    for (cmd_name in names(tool_config$commands)) {
      cmd_config <- tool_config$commands[[cmd_name]]
      cli_alert("* {.strong {cmd_name}}: {cmd_config$description}")
    }
    cli_alert_info("Use sn_help('{tool}', 'command_name') for detailed command help")
  } else {
    # Show specific command
    if (!command %in% names(tool_config$commands)) {
      available_commands <- paste(names(tool_config$commands), collapse = ", ")
      cli_abort("Command '{command}' not found. Available: {available_commands}")
    }

    cmd_config <- tool_config$commands[[command]]
    cli_h2("Command: {command}")
    cli_alert_info("Description: {cmd_config$description}")
    cli_alert_info("Binary: {cmd_config$binary}")

    cli_h3("Inputs:")
    for (input_name in names(cmd_config$inputs)) {
      input_def <- cmd_config$inputs[[input_name]]
      required_text <- if (input_def$required %||% FALSE) " (required)" else " (optional)"
      default_text <- if (!is.null(input_def$default)) paste0(" [default: ", input_def$default, "]") else ""

      cli_alert("* {.strong {input_name}} ({input_def$type}){required_text}{default_text}")
      cli_alert("  {input_def$description}")
    }

    cli_h3("Template:")
    cli_code(cmd_config$args_template)

    # Add R usage example
    cli_h3("R Usage Example:")
    r_example <- .sn_generate_r_example(tool, command, cmd_config)
    cli_code(r_example)
  }

  invisible(NULL)
}


#' Generate R Usage Example
#'
#' Internal function to generate an R usage example for a command.
#'
#' @param tool Character. Tool name.
#' @param command Character. Command name.
#' @param cmd_config List. Command configuration.
#'
#' @return Character. R code example.
#' @keywords internal
.sn_generate_r_example <- function(tool, command, cmd_config) {
  # Collect all parameters with their defaults/examples
  all_params <- list()

  # Add input parameters
  if (!is.null(cmd_config$inputs)) {
    for (input_name in names(cmd_config$inputs)) {
      input_def <- cmd_config$inputs[[input_name]]
      if (!is.null(input_def$default)) {
        all_params[[input_name]] <- input_def$default
      } else if (!is.null(input_def$example)) {
        all_params[[input_name]] <- input_def$example
      } else {
        # Generate example based on type
        if (!is.null(input_def$type) && input_def$type == "file") {
          all_params[[input_name]] <- paste0("\"path/to/", input_name, ".file\"")
        } else {
          all_params[[input_name]] <- paste0("\"", input_name, "_value\"")
        }
      }
    }
  }

  # Add output parameters
  if (!is.null(cmd_config$outputs)) {
    for (output_name in names(cmd_config$outputs)) {
      output_def <- cmd_config$outputs[[output_name]]
      if (!is.null(output_def$default)) {
        all_params[[output_name]] <- output_def$default
      } else if (!is.null(output_def$example)) {
        all_params[[output_name]] <- output_def$example
      } else {
        all_params[[output_name]] <- paste0("\"path/to/", output_name, ".output\"")
      }
    }
  }

  # Add other parameters if they have defaults
  if (!is.null(cmd_config$parameters)) {
    for (param_name in names(cmd_config$parameters)) {
      param_def <- cmd_config$parameters[[param_name]]
      if (!is.null(param_def$default)) {
        all_params[[param_name]] <- param_def$default
      } else if (!is.null(param_def$example)) {
        all_params[[param_name]] <- param_def$example
      }
    }
  }

  # Build the function call
  func_call <- sprintf("sn_run(\"%s\", \"%s\"", tool, command)

  if (length(all_params) > 0) {
    params_str <- sapply(names(all_params), function(name) {
      value <- all_params[[name]]
      if (is.character(value) && !startsWith(value, "\"")) {
        value <- sprintf("\"%s\"", value)
      }
      sprintf("       %s = %s", name, value)
    })

    func_call <- paste0(func_call, ",\n", paste(params_str, collapse = ",\n"))
  }

  func_call <- paste0(func_call, ")")

  return(func_call)
}
