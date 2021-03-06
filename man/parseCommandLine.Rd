% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/parseCommandLine.R
\docType{methods}
\name{parseCommandLine}
\alias{parseCommandLine}
\alias{parseCommandLine,ArgParser-method}
\title{Parse command line arguments via given ArgParser.}
\usage{
parseCommandLine(x, ...)

\S4method{parseCommandLine}{ArgParser}(x, cmdargs = commandArgs(),
  trim_prefix = FALSE)
}
\arguments{
\item{x}{An ArgParser object.}

\item{...}{Other arguments used in dispatched method.}

\item{cmdargs}{A character vector representing command line arguments.}

\item{trim_prefix}{A logical vector indicating wheather to strip dash prefix in argument names in parsed result.}
}
\value{
A list of parsed command line arguments.
}
\description{
Parse command line arguments via given ArgParser.
}
\section{Methods (by class)}{
\itemize{
\item \code{ArgParser}: 
}}
\examples{
library(magrittr)
cmdargs <- strsplit("prog.R --f1 v1", ' ')[[1]]
p <- ArgParser() \%>\%
   addFlag("--f1") \%>\%
   addFlag("--f2")
parseCommandLine(p, cmdargs=cmdargs)
}

