#' @importFrom processx run
#' @importFrom yaml yaml.load_file write_yaml read_yaml
#' @importFrom jsonlite fromJSON
#' @importFrom jinjar render
#' @importFrom methods new
#' @importFrom tools R_user_dir
#' @import cli
#' @import fs
#' @import rlang
NULL

# Global variables for caching
.global_toolbox <- new.env(parent = emptyenv())

#' Package startup
#'
#' @param libname library name
#' @param pkgname package name
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set default options for ShennongTools
  op <- options()
  op.sn <- list(
    sn.log_level = "minimal", # Default log level
    sn.log_dir = NULL # Use smart directory detection
  )
  toset <- !(names(op.sn) %in% names(op))
  if (any(toset)) options(op.sn[toset])

  # Initialize smart toolbox loading
  tryCatch(
    {
      # Pre-load toolbox using smart loading mechanism
      toolbox <- .load_smart_toolbox()
      .set_default_toolbox(toolbox)

      n_tools <- length(toolbox@tools)
      if (n_tools > 0) {
        cli_alert_success("ShennongTools loaded with {n_tools} pre-configured tools")
      } else {
        cli_alert_info("ShennongTools initialized with empty toolbox")
      }
    },
    error = function(e) {
      cli_alert_warning("Failed to initialize toolbox: {e$message}")
      # Fallback to empty toolbox
      toolbox <- sn_initialize_toolbox()
      .set_default_toolbox(toolbox)
    }
  )

  # Package initialization message
  packageStartupMessage("ShennongTools loaded. Use sn_get_toolbox_info() to see toolbox status.")
}
