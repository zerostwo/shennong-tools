% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\name{sn_diagnose_tool}
\alias{sn_diagnose_tool}
\title{Diagnose Tool Installation}
\usage{
sn_diagnose_tool(tool_name, version = NULL, base_dir = NULL)
}
\arguments{
\item{tool_name}{Character. Name of the tool to diagnose.}

\item{version}{Character. Specific version to diagnose (optional).}

\item{base_dir}{Character. Base directory for installations (optional).}
}
\value{
List with diagnostic information.
}
\description{
Provides detailed diagnostic information about tool installation status,
including missing directories, binaries, and potential issues.
}
\examples{
\dontrun{
# Diagnose a specific tool
sn_diagnose_tool("fastp")

# Diagnose specific version
sn_diagnose_tool("fastp", "0.26.0")
}
}
\seealso{
Other tool management: 
\code{\link{default_toolbox}},
\code{\link{sn_add_tool}()},
\code{\link{sn_check_user_toolbox}()},
\code{\link{sn_get_toolbox_info}()},
\code{\link{sn_initialize_toolbox}()},
\code{\link{sn_install_micromamba}()},
\code{\link{sn_install_tool}()},
\code{\link{sn_load_toolbox}()},
\code{\link{sn_remove_tool}()},
\code{\link{sn_reset_to_default}()},
\code{\link{sn_reset_toolbox}()},
\code{\link{sn_save_toolbox}()},
\code{\link{sn_validate_tool}()}
}
\concept{tool management}
