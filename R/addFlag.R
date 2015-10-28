#' @include ArgParser.R
NULL

#' Add flag argument to parser.
#' 
#' @param x An ArgParser object.
#' @param name Character vector of the flag name. Should be of length 1 and prefixed with "-{}-".
#' @param ... Other arguments used in dispatched method.

#' @export
setGeneric("addFlag", def=function(x, name, ...) standardGeneric("addFlag"))

#' @describeIn addFlag
#' @param short Optional alias for the flag. Should be of length 1 and prefixed with "-".
#' @param default Optional default value. If any, Should be of length 1.
#' @param optional Optional logical vector of length 1. Is the flag optional?
#' @param help Optional character vector shown in usage for the flag. If any, should be of length 1.
#' @param directive character vector indicating any directive the flag belongs to.
#' @return An ArgParser with the flag definition added.
#' @examples
#' p <- ArgParser("a test parser")
#' p <- addFlag(p, "--flag", "-f")

#' @export
setMethod("addFlag", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, short=NULL, default=NULL, optional=TRUE, help=NULL, directive=NULL) {

              name <- .checkArgLen(name, 1)

              if ( !is.null(default) ) {
                  default <- .checkArgLen(default, 1)
                  x@flags <- c(x@flags, setNames(default, name))
              } else {
                  x@flags <- c(x@flags, setNames(NA, name))
              }

              x@flags_alias <- c(x@flags_alias, setNames(ifelse(is.null(short), NA_character_, short), name))
              x@flags_isOptional <- c(x@flags_isOptional, setNames(optional, name))

              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }

              if ( !is.null(directive) ) {
                  if ( any(not_defined <- !directive %in% names(x@directs)) )
                      stop(sprintf("Directive not found in parser (%s): must be defined first.",
                                   toString(directive[not_defined])))
                  x@directs[[directive]]$flags <- c(x@directs[[directive]]$flags, name)
              }

              validObject(x)
              x

          })



