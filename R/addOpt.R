#' @include ArgParser.R
NULL

#' Add positional argument to parser.
#' 
#' @param x An ArgParser object.
#' @param name Character vector of the opt name. Should be of length 1.
#' @param help Optional character vector shown in usage for the opt. If any, should be of length 1.
#' @param narg Optinal number of arguments to be consumed.
#' @param nrequired Optional number of arguments required to be consumed.
#' @return An ArgParser with the opt definition added.
#' @examples
#' p <- ArgParser("a test parser")
#' p <- addOpt(p, "filename")

setGeneric("addOpt", def=function(x, name, ...) standardGeneric("addOpt"))
setMethod("addOpt", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, help=NULL, narg=1L, nrequired=narg) {
              name <- .checkArgLen(name, 1)
              narg <- as.integer(.checkArgLen(narg, 1))
              nrequired <- as.integer(.checkArgLen(nrequired, 1))
              if ( nrequired > narg )
                  stop("Found nrequired > narg, which is impossible.")
              if ( nrequired < narg )
                  warning("Found nrequired < narg, then this opt MUST be the last opt defined so it will work.")
              x@opt <- c(x@opt, name)
              x@opt_narg <- c(x@opt_narg, setNames(narg, name))
              x@opt_nrequired <- c(x@opt_nrequired, setNames(nrequired, name))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }
              validObject(x)
              x
          })



