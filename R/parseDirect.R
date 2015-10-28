
setGeneric("parseDirect", def=function(x, cmdargs) standardGeneric("parseDirect"))

setMethod("parseDirect", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs) {
              
              parsed <- list()

              if ( length(x@directs) ) {
                  # check occurrence of forced directives, if any
                  if ( length(forced_directs <- names(which(!x@directs_isOptional))) ) {
                      if ( !all(forced_and_raised <- forced_directs %in% cmdargs) )
                          stop(sprintf("Missing forced directive(s): %s", 
                                       paste(forced_directs[!forced_and_raised], collapse=", ")))
                  }

                  # check duplicated directives
                  .checkDupArg(cmdargs, names(x@directs))
             
                  # parse present directives
                  f1_idx <- match(f1_raised, cmdargs)
                  parsed <- c(parsed, setNames(cmdargs[f1_idx + 1L], f1_raised))
                  invalid_value <- parsed %in% c(allargs, NA)
                  if ( any(invalid_value) )
                      stop(sprintf("Invalid input in: %s.", 
                                   paste(names(parsed[invalid_value]), collapse=", ")))
                  flag_kv_idx <- c(flag_kv_idx, f1_idx, f1_idx + 1L)
              }

              list(argv=parsed, 
                   cmdargs_consumed=if (length(flag_kv_idx)) cmdargs[-flag_kv_idx] else cmdargs)
          })



