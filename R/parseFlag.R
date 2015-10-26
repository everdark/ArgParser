
setGeneric("parseFlag", def=function(x, cmdargs) standardGeneric("parseFlag"))

setMethod("parseFlag", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs) {
              
              parsed <- list()
              flag_kv_idx <- integer(0)
              allargs <- c(names(x@flags), names(x@switches_logic), names(x@switches_any))

              # replace short alias with full name, if any
              if ( any(alias_defined <- !is.na(x@flags_alias)) ) {
                  flags_with_alias <- x@flags_alias[alias_defined]
                  if ( any(is_alias <- cmdargs %in% flags_with_alias) ) {
                      alias_raised <- cmdargs[is_alias]
                      for ( a in alias_raised ) 
                          cmdargs[cmdargs == a] <- names(which(x@flags_alias == a))
                  }    
              }

              # check occurrence of forced flags, if any
              if ( length(forced_flags <- names(which(!x@flags_isOptional))) ) {
                  if ( !all(forced_and_raised <- forced_flags %in% cmdargs) )
                      stop(sprintf("Missing forced flag(s): %s", 
                                   paste(forced_flags[!forced_and_raised], collapse=", ")))
              }

              # check duplicated flag names
              .checkDupArg(cmdargs, names(x@flags))
             
              if ( length(x@flags) ) {
                  with_default <- sapply(x@flags, function(x) !is.na(x))

                  # parse flags without default value
                  f1 <- names(x@flags[!with_default])
                  if ( length(f1_raised <- f1[f1 %in% cmdargs]) ) {
                      f1_idx <- match(f1_raised, cmdargs)
                      parsed <- c(parsed, setNames(cmdargs[f1_idx + 1L], f1_raised))
                      invalid_value <- parsed %in% c(allargs, NA)
                      if ( any(invalid_value) )
                          stop(sprintf("Invalid input in: %s.", 
                                       paste(names(parsed[invalid_value]), collapse=", ")))
                      flag_kv_idx <- c(flag_kv_idx, f1_idx, f1_idx + 1L)
                  }

                  # parse flags with default value
                  f2 <- names(x@flags[with_default])
                  if ( length(f2_raised <- f2[f2 %in% cmdargs]) ) {
                      parsed <- c(parsed, x@flags[f2_raised])
                      for ( f in f2_raised ) {
                          v_idx <- match(f, cmdargs) + 1L
                          v <- cmdargs[v_idx]
                          if ( v %in% c(allargs, NA) ) {
                              flag_kv_idx <- c(flag_kv_idx, v_idx-1L)
                              next
                          }
                          parsed[f] <- v
                          flag_kv_idx <- c(flag_kv_idx, v_idx-1L, v_idx)
                      }
                  }
              }
              list(argv=parsed, 
                   cmdargs_consumed=if (length(flag_kv_idx)) cmdargs[-flag_kv_idx] else cmdargs)
          })



