% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/environment.R
\name{sn_install_tool}
\alias{sn_install_tool}
\title{Install a tool or environment}
\usage{
sn_install_tool(
  tool_name,
  version = NULL,
  base_dir = NULL,
  channel = "bioconda",
  yaml = NULL,
  overwrite = FALSE,
  mamba = NULL
)
}
\arguments{
\item{tool_name}{Character. Tool name (used for environment naming and tool installation).}

\item{version}{Character. Version to install (NULL for latest). Ignored if yaml is provided.}

\item{base_dir}{Character. Base directory for installations.}

\item{channel}{Character. Conda channel to use. Ignored if yaml is provided.}

\item{yaml}{Character. Path to YAML environment file (optional). If provided, installs
the environment to base_dir/tool_name.}

\item{overwrite}{Logical. Force reinstallation.}

\item{mamba}{Character. Path to mamba/micromamba executable.}
}
\value{
Character. Path to the installed environment.
}
\description{
Install a bioinformatics tool using mamba/micromamba. Can install either:
- A single tool by name and version
- A complete environment from YAML file
}
\examples{
\dontrun{
# Install a single tool
sn_install_tool("fastqc")

# Install with specific version
sn_install_tool("samtools", version = "1.16")

# Install from YAML file to custom environment name
sn_install_tool("bioinformatics", yaml = "path/to/environment.yaml")
}

}
\seealso{
Other tool management: 
\code{\link{default_toolbox}},
\code{\link{sn_add_tool}()},
\code{\link{sn_check_user_toolbox}()},
\code{\link{sn_diagnose_tool}()},
\code{\link{sn_get_toolbox_info}()},
\code{\link{sn_initialize_toolbox}()},
\code{\link{sn_install_micromamba}()},
\code{\link{sn_load_toolbox}()},
\code{\link{sn_remove_tool}()},
\code{\link{sn_reset_to_default}()},
\code{\link{sn_reset_toolbox}()},
\code{\link{sn_save_toolbox}()},
\code{\link{sn_validate_tool}()}

Other tool management: 
\code{\link{default_toolbox}},
\code{\link{sn_add_tool}()},
\code{\link{sn_check_user_toolbox}()},
\code{\link{sn_diagnose_tool}()},
\code{\link{sn_get_toolbox_info}()},
\code{\link{sn_initialize_toolbox}()},
\code{\link{sn_install_micromamba}()},
\code{\link{sn_load_toolbox}()},
\code{\link{sn_remove_tool}()},
\code{\link{sn_reset_to_default}()},
\code{\link{sn_reset_toolbox}()},
\code{\link{sn_save_toolbox}()},
\code{\link{sn_validate_tool}()}
}
\concept{tool management}
