#' @include ArgParser.R
NULL

#' Add directive to parser.
#' 
#' @param x An ArgParser object.
#' @param name Character vector of directive. Length > 1 is allowed.
#' @param ... Other arguments used in dispatched method.

#' @export
setGeneric("addDirect", def=function(x, name, ...) standardGeneric("addDirect"))

#' @describeIn addDirect
#' @param groupname Optional character vector of length 1, name for grouped directives.
#' @param optional Optional logical vector of length 1, indicating wheather the (grouped) directive is optional or not.
#' @param help Optional character vector shown in usage for the directive.
#' @return An ArgParser with the directive definition added.
#' @examples
#' p <- ArgParser("a test parser")
#' p <- addDirect(p, "dosomething")

#' @export
setMethod("addDirect", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, groupname=NULL, optional=FALSE, help=NULL) {
              newdirect <- list(flags=character(0), switches=character(0), opt=character(0))
              optional <- .checkArgLen(optional, 1)
              groupname <- .checkArgLen(groupname, 1)
              x@directs <- c(x@directs, setNames(list(name), groupname))
              x@directs_isOptional <- c(x@directs_isOptional, setNames(optional, groupname))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, length(name))
                  x@help <- c(x@help, setNames(help, name))
              }
              validObject(x)
              x
          })



