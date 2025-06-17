# Create Default Toolbox with All Built-in Tools
# This script runs during package build to create a pre-loaded toolbox
# with all built-in tools from inst/tools directory

# Load required packages
devtools::load_all()

# Get all available built-in tools
available_tools <- sn_list_tools(simple = TRUE)

cat("Found", length(available_tools), "built-in tools\n")

# Initialize empty toolbox
default_toolbox <- sn_initialize_toolbox()

# Add all built-in tools to the toolbox
for (tool_name in available_tools) {
  cat("Adding tool:", tool_name, "\n")

  tryCatch(
    {
      # Add tool without installing (just configuration)
      default_toolbox <- sn_add_tool(default_toolbox, tool_name = tool_name, install = FALSE)
      cat("  ✓ Added", tool_name, "\n")
    },
    error = function(e) {
      cat("  ✗ Failed to add", tool_name, ":", e$message, "\n")
    }
  )
}

cat("\nDefault toolbox created with", length(default_toolbox@tools), "tools\n")

# List tools in the default toolbox
cat("\nTools in default toolbox:\n")
for (tool_name in names(default_toolbox@tools)) {
  tool <- default_toolbox@tools[[tool_name]]
  default_version <- if (length(tool@default_version) > 0) tool@default_version else "unknown"
  cat("  -", tool_name, "(", default_version, ")\n")
}

# Save as package data using usethis
usethis::use_data(default_toolbox, overwrite = TRUE)

cat("\n✓ Default toolbox saved to data/default_toolbox.rda\n")
