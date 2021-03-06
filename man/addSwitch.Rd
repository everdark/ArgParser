% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/addSwitch.R
\docType{methods}
\name{addSwitch}
\alias{addSwitch}
\alias{addSwitch,ArgParser,character-method}
\title{Add switch argument to parser.}
\usage{
addSwitch(x, name, ...)

\S4method{addSwitch}{ArgParser,character}(x, name, short = NULL,
  states = FALSE, help = NULL)
}
\arguments{
\item{x}{An ArgParser object.}

\item{name}{Character vector of the switch name. Should be of length 1 and prefixed with "-{}-".}

\item{...}{Other arguments used in dispatched method.}

\item{short}{Optional alias for the switch. Should be of length 1 and prefixed with "-".}

\item{states}{Optional states of unpushed/pushed in list of length 2, or a logical vector of length 1 with the unpushed state.}

\item{help}{Optional character vector shown in usage for the switch. If any, should be of length 1.}
}
\value{
An ArgParser with the switch definition added.
}
\description{
Add switch argument to parser.
}
\section{Methods (by class)}{
\itemize{
\item \code{x = ArgParser,name = character}: 
}}
\examples{
p <- ArgParser("a test parser")
p <- addSwitch(p, "--switch-logic", "-s1", FALSE)
p <- addSwitch(p, "--switch-any", "-s2", list(0, 1))
}

