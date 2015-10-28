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
#' @param optional Optional logical vector with same length as name.
#' @param help Optional character vector shown in usage for the directive. If any, should be of length 1.
#' @return An ArgParser with the directive definition added.
#' @examples
#' p <- ArgParser("a test parser")
#' p <- addDirect(p, "dosomething")

#' @export
setMethod("addDirect", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, optional=TRUE, help=NULL) {
              newdirect <- list(flags=character(0), switches=character(0), opt=character(0))
              optional <- .checkArgLen(optional, 1)
              if ( (nlen <- length(name)) > (olen <- length(optional)) ) {
                  warning("Length of optional is smaller than name. The rest are assumed TRUE.")
                  optional[(olen+1):nlen] <- TRUE
              } else if ( nlen < olen ) {
                  warning("Length of optional is greater than name and hence trimmed.")
                  optional <- optional[1:nlen]
              }
              for ( n in name )
                  x@directs <- c(x@directs, setNames(list(newdirect), n))
              x@directs_isOptional <- c(x@directs_isOptional, setNames(optional, name))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }
              validObject(x)
              x
          })



