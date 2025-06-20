% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/template.R
\name{sn_render_template}
\alias{sn_render_template}
\title{Render Template with Parameters using Jinjar}
\usage{
sn_render_template(tool, command, params, show_messages = TRUE)
}
\arguments{
\item{tool}{Tool object.}

\item{command}{Character. Command name.}

\item{params}{List. Named list of parameters to substitute in template.}

\item{show_messages}{Logical. Whether to show template rendering messages.}
}
\value{
Character. Rendered command string.
}
\description{
Uses the jinjar package to render command templates with user-provided
parameters. Supports Jinja2 syntax for conditional logic and variable substitution.
}
\examples{
\dontrun{
tool <- Tool("my_tool", "my_tool.yaml")
command <- "my_command"
params <- list(threads = 4, input = "test.bam", output = "filtered.bam")
rendered <- sn_render_template(tool, command, params)
}
}
\seealso{
Other template system: 
\code{\link{sn_render_python_template}()},
\code{\link{sn_test_template}()}
}
\concept{template system}
