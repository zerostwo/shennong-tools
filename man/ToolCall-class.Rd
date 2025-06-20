% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{ToolCall-class}
\alias{ToolCall-class}
\title{ToolCall S4 Class}
\description{
Represents a single tool invocation with inputs, outputs, and parameters.
}
\section{Slots}{

\describe{
\item{\code{tool}}{Tool. The Tool object being called.}

\item{\code{command}}{Character. The specific command/subcommand.}

\item{\code{rendered_command}}{Character. The fully rendered command string.}

\item{\code{inputs}}{List. Input files/objects with their roles.}

\item{\code{outputs}}{List. Expected output files/objects.}

\item{\code{params}}{List. Command parameters.}

\item{\code{work_dir}}{Character. Working directory for execution.}

\item{\code{log_file}}{Character. Path to log file.}

\item{\code{status}}{Character. Execution status ("pending", "running", "success", "failed").}

\item{\code{return_code}}{Numeric. Process return code.}

\item{\code{stdout}}{Character. Captured stdout.}

\item{\code{stderr}}{Character. Captured stderr.}

\item{\code{resources}}{List. Resource usage including runtime, memory, and CPU.}
}}

