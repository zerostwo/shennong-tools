#' @importFrom processx run
#' @importFrom yaml yaml.load_file write_yaml
#' @importFrom jsonlite fromJSON
#' @importFrom jinjar render
#' @importFrom methods new
#' @importFrom tools R_user_dir
#' @import cli
#' @import fs
#' @import rlang
NULL

# Global variables for caching
.sn_global_toolbox <- new.env(parent = emptyenv())

#' Package startup
#'
#' @param libname library name
#' @param pkgname package name
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set default options for ShennongTools
  op <- options()
  op.sn <- list(
    sn.log_level = "normal", # Default log level
    sn.log_dir = NULL # Use smart directory detection
  )
  toset <- !(names(op.sn) %in% names(op))
  if (any(toset)) options(op.sn[toset])

  # Initialize global toolbox cache
  tryCatch(
    {
      base_dir <- R_user_dir(package = "shennong-tools", which = "data")
      toolbox <- sn_initialize_toolbox(base_dir)
      assign("toolbox", toolbox, envir = .sn_global_toolbox)
      cli_alert_success("ShennongTools toolbox initialized")
    },
    error = function(e) {
      cli_alert_warning("Failed to initialize toolbox: {e$message}")
    }
  )

  # Package initialization message
  packageStartupMessage("ShennongTools loaded. Use sn_list_tools() to see available tools.")
}
