#' @include ArgParser.R
NULL

#' Add directive to parser.
#' 
#' @param x An ArgParser object.
#' @param name Character vector of the directive name. Should be of length 1.
#' @param ... Other arguments used in dispatched method.

#' @export
setGeneric("addDirect", def=function(x, name, ...) standardGeneric("addDirect"))

#' @describeIn addDirect
#' @param optional Optional logical vector of length 1. Is the directive optional?
#' @param help Optional character vector shown in usage for the directive. If any, should be of length 1.
#' @return An ArgParser with the directive definition added.
#' @examples
#' p <- ArgParser("a test parser")
#' p <- addDirect(p, "dosomething")

#' @export
setMethod("addDirect", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, optional=TRUE, help=NULL) {
              newdirect <- list(flags=character(0), switches=character(0), opt=character(0))
              name <- .checkArgLen(name, 1)
              optional <- .checkArgLen(optional, 1)
              x@directs <- c(x@directs, setNames(list(newdirect), name))
              x@directs_isOptional <- c(x@directs_isOptional, setNames(optional, name))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }
              validObject(x)
              x
          })



