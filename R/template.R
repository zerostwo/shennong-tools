#' Render Template with Parameters using Jinjar
#'
#' Uses the jinjar package to render command templates with user-provided
#' parameters. Supports Jinja2 syntax for conditional logic and variable substitution.
#'
#' @param template Character. Template string from tool configuration (shell or python field).
#' @param params List. Named list of parameters to substitute in template.
#' @param inputs List. Input definitions from tool configuration.
#' @param outputs List. Output definitions from tool configuration.
#' @param command_config List. Complete command configuration from tool YAML.
#' @param show_messages Logical. Whether to show template rendering messages.
#'
#' @return Character. Rendered command string.
#' @family template system
#' @concept template system
#' @export
#'
#' @examples
#' \dontrun{
#' template <- "samtools view -@ {{ threads }} {{ input }} -o {{ output }}"
#' params <- list(threads = 4, input = "test.bam", output = "filtered.bam")
#' rendered <- sn_render_template(template, params, inputs, outputs, cmd_config)
#' }
sn_render_template <- function(template, params, inputs = list(), outputs = list(), command_config = list(), show_messages = TRUE) {
  # Prepare parameters with defaults
  final_params <- .prepare_template_params(params, inputs, outputs, command_config)

  # Validate required parameters
  .validate_template_params(final_params, inputs, outputs)

  # Handle special parameter types
  final_params <- .handle_special_param_types(final_params, inputs, command_config)

  # Render using jinjar
  tryCatch(
    {
      # Create arguments list preserving NULL values
      args <- list(.x = template) # First argument is .x (the template)
      # Add all parameters from final_params
      for (param_name in names(final_params)) {
        if (is.null(final_params[[param_name]])) {
          args[param_name] <- list(NULL) # Preserve NULL values
        } else {
          args[[param_name]] <- final_params[[param_name]]
        }
      }

      rendered <- do.call(render, args)

      # Clean up extra whitespace
      rendered <- gsub("\\s+", " ", rendered)
      rendered <- trimws(rendered)

      if (show_messages) {
        cli_alert_success("Template rendered successfully")
      }
      return(rendered)
    },
    error = function(e) {
      # Provide more detailed error information for debugging
      cli_alert_danger("Template rendering failed!")
      cli_text("Template: {.code {substr(template, 1, 100)}}...")
      cli_text("Available parameters:")
      for (param_name in names(final_params)) {
        param_val <- final_params[[param_name]]
        if (is.character(param_val) && nchar(param_val) > 30) {
          param_val <- paste0(substr(param_val, 1, 27), "...")
        }
        cli_text("  {param_name}: {.val {param_val}}")
      }
      cli_abort("Failed to render template: {e$message}")
    }
  )
}

#' Prepare Parameters for Template Rendering
#'
#' Merges user parameters with defaults and handles special parameter types.
#'
#' @param params List. User-provided parameters.
#' @param inputs List. Input definitions with defaults.
#' @param outputs List. Output definitions.
#' @param command_config List. Command configuration.
#'
#' @return List. Final parameters for template rendering.
#' @keywords internal
.prepare_template_params <- function(params, inputs, outputs, command_config) {
  final_params <- list()

  # Add binary parameter if present in command config
  if (!is.null(command_config$binary)) {
    final_params$binary <- command_config$binary
  }

  # Initialize all inputs with defaults or NULL
  for (input_name in names(inputs)) {
    input_def <- inputs[[input_name]]
    if (!is.null(input_def$default)) {
      final_params[[input_name]] <- input_def$default
    } else {
      # For inputs without defaults, initialize to NULL using proper syntax to preserve the element
      # Using list(NULL) ensures the element exists in the list with NULL value
      final_params[input_name] <- list(NULL)
    }
  }

  # Add defaults from params definitions if present
  if (!is.null(command_config$params)) {
    for (param_name in names(command_config$params)) {
      param_def <- command_config$params[[param_name]]
      if (!is.null(param_def$default)) {
        final_params[[param_name]] <- param_def$default
      }
    }
  }

  # Add empty values for all outputs to ensure they exist in template context
  for (output_name in names(outputs)) {
    if (!output_name %in% names(final_params)) {
      output_def <- outputs[[output_name]]
      required <- output_def$required %||% FALSE
      if (required) {
        final_params[[output_name]] <- ""
      } else {
        # For optional outputs, use NULL so jinjar conditions work correctly
        final_params[output_name] <- list(NULL)
      }
    }
  }

  # Override with user-provided parameters
  for (param_name in names(params)) {
    value <- params[[param_name]]
    # Convert empty strings to NULL for optional inputs to make jinjar conditions work correctly
    if (param_name %in% names(inputs)) {
      input_def <- inputs[[param_name]]
      required <- input_def$required %||% TRUE
      if (!required && is.character(value) && length(value) == 1 && nzchar(value) == FALSE) {
        # Use proper syntax to preserve NULL values in list
        final_params[param_name] <- list(NULL)
      } else {
        final_params[[param_name]] <- value
      }
    } else {
      # For parameters (non-inputs), treat empty string as NULL so that
      # `{% if param %}` blocks evaluate to FALSE unless user provides a value.
      if (is.character(value) && length(value) == 1 && nzchar(value) == FALSE) {
        final_params[param_name] <- list(NULL)
      } else {
        final_params[[param_name]] <- value
      }
    }
  }

  # After merging defaults and overrides, ensure empty-string defaults for parameters
  # (those not supplied by user) are converted to NULL as well.
  for (param_name in names(final_params)) {
    val <- final_params[[param_name]]
    if (is.character(val) && length(val) == 1 && nzchar(val) == FALSE) {
      final_params[param_name] <- list(NULL)
    }
  }

  return(final_params)
}

#' Handle Special Parameter Types
#'
#' Processes parameters based on their data types and converts them
#' appropriately for template rendering.
#'
#' @param params List. Parameters to process.
#' @param inputs List. Input definitions.
#' @param command_config List. Command configuration.
#'
#' @return List. Processed parameters.
#' @keywords internal
.handle_special_param_types <- function(params, inputs, command_config) {
  # Process input types - check all defined inputs, not just those in params
  for (input_name in names(inputs)) {
    if (input_name %in% names(params)) {
      input_def <- inputs[[input_name]]
      datatype <- input_def$datatype

      # Handle different datatypes
      if (!is.null(datatype)) {
        converted_value <- .convert_param_by_datatype(params[[input_name]], datatype)
        # Use proper syntax to preserve NULL values in list
        if (is.null(converted_value)) {
          params[input_name] <- list(NULL)
        } else {
          params[[input_name]] <- converted_value
        }
      }
    }
    # Important: Keep NULL values in params even if they're not processed
    # This ensures all inputs are available in the template context
  }

  # Process parameter types if defined
  if (!is.null(command_config$params)) {
    for (param_name in names(command_config$params)) {
      if (param_name %in% names(params)) {
        param_def <- command_config$params[[param_name]]
        datatype <- param_def$datatype

        if (!is.null(datatype)) {
          converted_value <- .convert_param_by_datatype(params[[param_name]], datatype)
          # Use proper syntax to preserve NULL values in list
          if (is.null(converted_value)) {
            params[param_name] <- list(NULL)
          } else {
            params[[param_name]] <- converted_value
          }
        }
      }
    }
  }

  return(params)
}

#' Convert Parameter by Data Type
#'
#' Converts a parameter value based on its specified data type.
#'
#' @param value The parameter value to convert.
#' @param datatype Character. The target data type.
#'
#' @return Converted value.
#' @keywords internal
.convert_param_by_datatype <- function(value, datatype) {
  # Handle NULL or empty datatype
  if (is.null(datatype) || length(datatype) == 0) {
    return(value)
  }

  # Ensure datatype is a single character value
  if (length(datatype) > 1) {
    datatype <- datatype[1]
  }

  # Check for empty string
  if (datatype == "") {
    return(value)
  }

  switch(datatype,
    "integer" = as.integer(value),
    "numeric" = as.numeric(value),
    "logical" = as.logical(value),
    "string" = as.character(value),
    "flag" = if (as.logical(value)) value else "",
    # For common file datatypes, collapse multiple paths into a single space-separated string
    "fastq" = if (is.character(value) && length(value) > 1) paste(value, collapse = " ") else value,
    "bam" = if (is.character(value) && length(value) > 1) paste(value, collapse = " ") else value,
    "sam" = if (is.character(value) && length(value) > 1) paste(value, collapse = " ") else value,
    "file" = if (is.character(value) && length(value) > 1) paste(value, collapse = " ") else value,
    value # Default: return as-is
  )
}

#' Validate Template Parameters
#'
#' Validates that all required parameters are provided and have valid values.
#'
#' @param params List. Final parameters after merging defaults.
#' @param inputs List. Input definitions.
#' @param outputs List. Output definitions.
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
.validate_template_params <- function(params, inputs, outputs) {
  # Helper to determine if a value is empty/invalid
  is_empty <- function(x) {
    if (is.null(x)) {
      return(TRUE)
    }

    if (is.atomic(x)) {
      # Length-0 vector
      if (length(x) == 0) {
        return(TRUE)
      }

      # All NA
      if (all(is.na(x))) {
        return(TRUE)
      }

      # Character vectors: all empty strings?
      if (is.character(x) && all(nzchar(x) == FALSE)) {
        return(TRUE)
      }

      # Otherwise not empty
      return(FALSE)
    }

    # For non-atomic objects keep previous behaviour (not empty)
    FALSE
  }

  # Check required inputs
  for (input_name in names(inputs)) {
    input_def <- inputs[[input_name]]
    is_required <- input_def$required %||% FALSE

    if (is_required) {
      if (!input_name %in% names(params)) {
        cli_abort("Missing required input: {input_name}")
      }

      value <- params[[input_name]]
      if (is_empty(value)) {
        cli_abort("Required input '{input_name}' cannot be empty or NULL")
      }
    }
  }

  # Check required outputs
  for (output_name in names(outputs)) {
    output_def <- outputs[[output_name]]
    is_required <- output_def$required %||% FALSE

    if (is_required) {
      if (!output_name %in% names(params)) {
        cli_abort("Missing required output: {output_name}")
      }

      value <- params[[output_name]]
      if (is_empty(value)) {
        cli_abort("Required output '{output_name}' cannot be empty or NULL")
      }
    }
  }
}

#' Render Python Template
#'
#' Special handling for Python-based tools that use Python code templates
#' instead of shell commands.
#'
#' @param python_template Character. Python code template.
#' @param params List. Parameters for template rendering.
#' @param inputs List. Input definitions.
#' @param outputs List. Output definitions.
#' @param command_config List. Command configuration.
#' @param show_messages Logical. Whether to show template rendering messages.
#'
#' @return Character. Rendered Python code.
#' @family template system
#' @concept template system
#' @export
#'
#' @examples
#' \dontrun{
#' python_code <- 'import scanpy as sc\nadata = sc.read_h5ad("{{ input_h5ad }}")'
#' params <- list(input_h5ad = "data.h5ad")
#' rendered <- sn_render_python_template(python_code, params, inputs, outputs, cmd_config)
#' }
sn_render_python_template <- function(python_template, params, inputs = list(), outputs = list(), command_config = list(), show_messages = TRUE) {
  # Prepare parameters with prefixes for inputs/outputs
  final_params <- .prepare_python_template_params(params, inputs, outputs, command_config)

  # Validate required parameters
  .validate_template_params(final_params, inputs, outputs)

  # Render using jinjar
  tryCatch(
    {
      rendered <- do.call(render, c(list(python_template), final_params))

      if (show_messages) {
        cli_alert_success("Python template rendered successfully")
      }
      return(rendered)
    },
    error = function(e) {
      cli_abort("Failed to render Python template: {e$message}")
    }
  )
}

#' Prepare Parameters for Python Template
#'
#' Prepares parameters for Python templates, adding input_/output_ prefixes
#' as commonly used in Python tool templates.
#'
#' @param params List. User parameters.
#' @param inputs List. Input definitions.
#' @param outputs List. Output definitions.
#' @param command_config List. Command configuration.
#'
#' @return List. Prepared parameters with prefixes.
#' @keywords internal
.prepare_python_template_params <- function(params, inputs, outputs, command_config) {
  final_params <- .prepare_template_params(params, inputs, outputs, command_config)

  # Add input_ prefixed versions for convenience in Python templates
  for (input_name in names(inputs)) {
    if (input_name %in% names(final_params)) {
      final_params[[paste0("input_", input_name)]] <- final_params[[input_name]]
    }
  }

  # Add output_ prefixed versions for convenience in Python templates
  for (output_name in names(outputs)) {
    if (output_name %in% names(final_params)) {
      final_params[[paste0("output_", output_name)]] <- final_params[[output_name]]
    }
  }

  return(final_params)
}

#' Test Template Rendering
#'
#' Helper function to test template rendering with example parameters.
#' Useful for debugging and development.
#'
#' @param template Character. Template to test.
#' @param test_params List. Test parameters.
#'
#' @return Character. Rendered result or error message.
#' @family template system
#' @concept template system
#' @export
#'
#' @examples
#' \dontrun{
#' template <- "samtools view -@ {{ threads }} {{ input }}"
#' test_params <- list(threads = 4, input = "test.bam")
#' result <- sn_test_template(template, test_params)
#' }
sn_test_template <- function(template, test_params) {
  tryCatch(
    {
      rendered <- do.call(render, c(list(template), test_params))
      cli_alert_success("Template test successful")
      return(rendered)
    },
    error = function(e) {
      cli_alert_danger("Template test failed: {e$message}")
      return(paste("ERROR:", e$message))
    }
  )
}
