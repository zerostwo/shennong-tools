% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/registry.R
\name{sn_get_tool_config}
\alias{sn_get_tool_config}
\title{Get Tool Configuration}
\usage{
sn_get_tool_config(tool_name, registry = NULL)
}
\arguments{
\item{tool_name}{Character. Name of the tool.}

\item{registry}{List. Tool registry (optional, will load if not provided).}
}
\value{
List with tool configuration or NULL if not found.
}
\description{
Get configuration for a specific tool from the registry.
}
\seealso{
Other tool information: 
\code{\link{sn_get_tool_info}()},
\code{\link{sn_help}()},
\code{\link{sn_list_tools}()},
\code{\link{sn_load_registry}()}
}
\concept{tool information}
