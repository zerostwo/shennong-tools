#' Render Template with Parameters
#'
#' This function uses glue to render the args_template from tool configuration
#' with user-provided parameters and default values.
#'
#' @param template Character. Template string from tool configuration.
#' @param params List. Named list of parameters to substitute in template.
#' @param inputs List. Input definitions from tool configuration.
#' @param cmd_config List. Command configuration from tool YAML.
#'
#' @return Character. Rendered command string.
#' @export
sn_render_template <- function(template, params, inputs, cmd_config) {
  # Prepare parameters with defaults from inputs
  final_params <- .sn_prepare_params(params, inputs)

  # Add output parameters from user params (outputs don't have defaults typically)
  outputs <- cmd_config$outputs %||% list()
  for (output_name in names(outputs)) {
    if (output_name %in% names(params)) {
      final_params[[output_name]] <- params[[output_name]]
    }
  }

  # Validate required parameters (only for inputs)
  .sn_validate_params(final_params, inputs)

  # Validate required outputs are provided
  .sn_validate_outputs(final_params, outputs, params)

  # Add special handling for conditional logic in templates
  # Convert logical values to proper conditional format
  for (name in names(final_params)) {
    if (is.logical(final_params[[name]])) {
      final_params[[name]] <- as.character(final_params[[name]])
    }
  }

  # Render using glue with custom delimiter to handle jinja2-like syntax
  tryCatch(
    {
      # First pass: handle conditional blocks (simplified jinja2-like)
      processed_template <- .sn_process_conditionals(template, final_params)

      # Second pass: handle simple glue-style substitutions
      rendered <- glue(processed_template, .envir = list2env(final_params), .na = "")

      # Clean up extra whitespace
      rendered <- gsub("\\s+", " ", rendered)
      rendered <- trimws(rendered)

      cli_alert_success("Template rendered successfully")

      return(rendered)
    },
    error = function(e) {
      cli_abort("Failed to render template: {e$message}")
    }
  )
}

#' Prepare Parameters with Defaults
#'
#' Internal function to merge user parameters with default values from
#' tool configuration.
#'
#' @param params List. User-provided parameters.
#' @param inputs List. Input definitions with defaults.
#'
#' @return List. Merged parameters.
#' @keywords internal
.sn_prepare_params <- function(params, inputs) {
  final_params <- list()

  # Start with defaults from input definitions
  for (input_name in names(inputs)) {
    input_def <- inputs[[input_name]]
    if (!is.null(input_def$default)) {
      final_params[[input_name]] <- input_def$default
    }
  }

  # Override with user-provided parameters
  for (param_name in names(params)) {
    final_params[[param_name]] <- params[[param_name]]
  }

  # Add common parameters that might not be in inputs but used in templates
  common_params <- list(
    extra = ""
  )

  for (common_name in names(common_params)) {
    if (!common_name %in% names(final_params)) {
      final_params[[common_name]] <- common_params[[common_name]]
    }
  }

  return(final_params)
}

#' Validate Required Parameters
#'
#' Internal function to check that all required parameters are provided.
#'
#' @param params List. Final parameters after merging defaults.
#' @param inputs List. Input definitions.
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
.sn_validate_params <- function(params, inputs) {
  required_inputs <- names(inputs)[sapply(inputs, function(x) x$required %||% FALSE)]

  missing_required <- setdiff(required_inputs, names(params))
  if (length(missing_required) > 0) {
    cli_abort("Missing required parameters: {paste(missing_required, collapse = ', ')}")
  }

  # Check for empty required parameters
  for (req_input in required_inputs) {
    value <- params[[req_input]]
    if (is.null(value) || (is.character(value) && value == "") || is.na(value)) {
      cli_abort("Required parameter '{req_input}' cannot be empty or NULL")
    }
  }
}

#' Process Conditional Blocks in Templates
#'
#' Internal function to handle simplified jinja2-like conditional syntax
#' in templates (e.g., if reads2 then include content endif).
#'
#' @param template Character. Template string possibly containing conditionals.
#' @param params List. Parameters for evaluation.
#'
#' @return Character. Template with conditionals processed.
#' @keywords internal
.sn_process_conditionals <- function(template, params) {
  # Handle {% if var %}...{% endif %} blocks (positive conditions)
  pattern_positive <- "\\{%\\s*if\\s+(\\w+)\\s*%\\}(.*?)\\{%\\s*endif\\s*%\\}"

  # Handle {% if not var %}...{% endif %} blocks (negative conditions)
  pattern_negative <- "\\{%\\s*if\\s+not\\s+(\\w+)\\s*%\\}(.*?)\\{%\\s*endif\\s*%\\}"

  # Process negative conditions first (more specific pattern)
  while (grepl(pattern_negative, template)) {
    matches <- regmatches(template, gregexpr(pattern_negative, template, perl = TRUE))[[1]]

    for (match in matches) {
      # Extract variable name and content
      var_match <- regmatches(match, regexec("\\{%\\s*if\\s+not\\s+(\\w+)\\s*%\\}", match))[[1]]
      var_name <- var_match[2]

      content_match <- regmatches(match, regexec("\\{%\\s*if\\s+not\\s+\\w+\\s*%\\}(.*?)\\{%\\s*endif\\s*%\\}", match))[[1]]
      content <- content_match[2]

      # Evaluate negative condition
      condition_met <- TRUE # Default to true for negative condition
      if (var_name %in% names(params)) {
        value <- params[[var_name]]
        # Condition is met if value is null, empty, false, or NA
        condition_met <- is.null(value) ||
          is.na(value) ||
          value == "" ||
          (is.logical(value) && !value)
      }

      # Replace with content or empty string
      replacement <- if (condition_met) content else ""
      template <- gsub(match, replacement, template, fixed = TRUE)
    }
  }

  # Process positive conditions
  while (grepl(pattern_positive, template)) {
    matches <- regmatches(template, gregexpr(pattern_positive, template, perl = TRUE))[[1]]

    for (match in matches) {
      # Extract variable name and content
      var_match <- regmatches(match, regexec("\\{%\\s*if\\s+(\\w+)\\s*%\\}", match))[[1]]
      var_name <- var_match[2]

      content_match <- regmatches(match, regexec("\\{%\\s*if\\s+\\w+\\s*%\\}(.*?)\\{%\\s*endif\\s*%\\}", match))[[1]]
      content <- content_match[2]

      # Evaluate positive condition
      condition_met <- FALSE
      if (var_name %in% names(params)) {
        value <- params[[var_name]]
        condition_met <- !is.null(value) &&
          !is.na(value) &&
          value != "" &&
          (is.logical(value) && value || !is.logical(value))
      }

      # Replace with content or empty string
      replacement <- if (condition_met) content else ""
      template <- gsub(match, replacement, template, fixed = TRUE)
    }
  }

  return(template)
}

#' Get Template Variables
#'
#' Extract variable names used in a template string.
#'
#' @param template Character. Template string.
#'
#' @return Character vector. Variable names found in template.
#' @export
sn_get_template_vars <- function(template) {
  # Find {variable} patterns
  glue_vars <- regmatches(template, gregexpr("\\{(\\w+)\\}", template))[[1]]
  glue_vars <- gsub("[{}]", "", glue_vars)

  # Find variables in positive conditional blocks
  pos_cond_vars <- regmatches(template, gregexpr("\\{%\\s*if\\s+(\\w+)\\s*%\\}", template))[[1]]
  pos_cond_vars <- gsub(".*if\\s+(\\w+).*", "\\1", pos_cond_vars)

  # Find variables in negative conditional blocks
  neg_cond_vars <- regmatches(template, gregexpr("\\{%\\s*if\\s+not\\s+(\\w+)\\s*%\\}", template))[[1]]
  neg_cond_vars <- gsub(".*if\\s+not\\s+(\\w+).*", "\\1", neg_cond_vars)

  unique(c(glue_vars, pos_cond_vars, neg_cond_vars))
}

#' Validate Required Output Parameters
#'
#' Internal function to check that required output parameters are provided.
#'
#' @param final_params List. All final parameters.
#' @param outputs List. Output definitions.
#' @param user_params List. User-provided parameters.
#'
#' @return NULL (throws error if validation fails)
#' @keywords internal
.sn_validate_outputs <- function(final_params, outputs, user_params) {
  # Find required outputs (those that are required=TRUE or used in template)
  required_outputs <- names(outputs)[sapply(outputs, function(x) x$required %||% FALSE)]

  # Also check for outputs that are referenced in template but not provided
  # This is handled by template variable validation, but we check here for better error messages
  for (output_name in names(outputs)) {
    if (output_name %in% names(user_params)) {
      value <- user_params[[output_name]]
      if (is.null(value) || (is.character(value) && value == "") || is.na(value)) {
        cli_abort("Output parameter '{output_name}' cannot be empty or NULL")
      }
    }
  }
}
