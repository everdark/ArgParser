
setGeneric(".parseOpt", def=function(x, cmdargs_consumed) standardGeneric(".parseOpt"))

setMethod(".parseOpt", signature=c(x="ArgParser", cmdargs_consumed="character"),
          definition=function(x, cmdargs_consumed) {
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
              parsed
          })



