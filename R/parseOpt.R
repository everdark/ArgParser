
setGeneric("parseOpt", def=function(x, cmdargs_consumed, ...) standardGeneric("parseOpt"))

setMethod("parseOpt", signature=c(x="ArgParser", cmdargs_consumed="character"),
          definition=function(x, cmdargs_consumed, limited_to=NULL) {

              if ( !is.null(limited_to) ) {
                  x@opt <- limited_to
                  x@opt_narg <- x@opt_narg[limited_to]
                  x@opt_nrequired <- x@opt_nrequired[limited_to]
              }

              parsed <- list()

              if ( length(x@opt) ) {
                  for ( opt in x@opt ) {
                      if ( length(cmdargs_consumed) < x@opt_nrequired[opt] )
                          stop(sprintf("Number of supplied positional arg (%s) less than required.", opt))
                      to_narg <- 1:x@opt_narg[opt]
                      parsed <- c(parsed, setNames(list(cmdargs_consumed[to_narg]), opt))
                      cmdargs_consumed <- cmdargs_consumed[-to_narg]
                  }
              }

              list(argv=parsed,
                   cmdargs_consumed=cmdargs_consumed)
          })



