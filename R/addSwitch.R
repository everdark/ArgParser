#' @include ArgParser.R
NULL

library(methods)

#' Add switch argument to parser.
#' 
#' @param x An ArgParser object.
#' @param name Character vector of the switch name. Should be of length 1 and prefixed with "-{}-".
#' @param short Optional alias for the switch. Should be of length 1 and prefixed with "-".
#' @param states Optional states of unpushed/pushed in list of length 2, or a logical vector of length 1 with the unpushed state. 
#' @param help Optional character vector shown in usage for the switch. If any, should be of length 1.
#' @return An ArgParser with the switch definition added.
#' @examples
#' p <- ArgParser("a test parser")
#' p <- addSwitch(p, "--switch-logic", "-s1", FALSE)
#' p <- addSwitch(p, "--switch-any", "-s2", list(0, 1))

setGeneric("addSwitch", def=function(x, name, ...) standardGeneric("addSwitch"))
setMethod("addSwitch", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, short=NULL, states=FALSE, help=NULL) {
              name <- .checkArgLen(name, 1)
              if ( !is.vector(states) ) 
                  stop("Value of states must be of type vector.")
              if ( is.logical(states) ) {
                  states <- .checkArgLen(states, 1)
                  x@switches_logic <- c(x@switches_logic, setNames(states, name))
              } else {
                  if ( length(states) < 2 )
                      stop("Non-logical states vector should have length 2.")
                  states <- .checkArgLen(states, 2)
                  names(states) <- c("unpushed", "pushed")
                  x@switches_any <- c(x@switches_any, setNames(list(as.list(states)), name))
              }
              x@switches_alias <- c(x@switches_alias, setNames(ifelse(is.null(short), NA_character_, short), name))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }
              validObject(x)
              x
          })



