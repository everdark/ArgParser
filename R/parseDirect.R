
setGeneric("parseDirect", def=function(x, cmdargs) standardGeneric("parseDirect"))

setMethod("parseDirect", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs) {
              
              parsed <- list()

              if ( length(x@directs) ) {
                  # check occurrence of forced directive, if any
                  is_optional <- sapply(x@directs_group, function(x) x$is_optional)
                  if ( length(forced_dgroups <- names(which(!is_optional))) ) {
                      for ( dg in forced_dgroups ) {
                          dnames <- x@directs_group[[dg]]$mem
                          nraised <- sum(dnames %in% cmdargs)
                          if ( nraised == 0 ) {
                              stop(sprintf("Missing forced directive (any of: %s)", 
                                           paste(dnames, collapse=", ")))
                          } else if ( nraised > 1 ) {
                              stop(sprintf("Directives of the same group can not co-exist: %s",
                                           paste(dnames, collapse=", ")))
                          }
                      }
                  }

                  # check duplicated directives
                  .checkDupArg(cmdargs, all_directs <- names(x@directs))
             
                  # parse present directives along with its sub-commands
                  raised_direct <- all_directs[all_directs %in% cmdargs]
                  raised_direct_idx <- match(raised_direct, cmdargs)
                  pre_cmdargs <- cmdargs[1:(raised_direct_idx-1)]
                  post_cmdargs <- cmdargs[-(1:raised_direct_idx)]
                  parsed <- c(parsed, setNames(list(list()), raised_direct))
                  # :sub-flag
                  if ( length(dflags <- x@directs[[raised_direct]]$flags) ) {
                      fparsed <- parseFlag(x, post_cmdargs, dflags)
                      parsed[[raised_direct]] <- c(parsed[[raised_direct]], fparsed$argv)
                      post_cmdargs <- fparsed$cmdargs_consumed
                  }
                  # :sub-switch
                  if ( length(dswitches <- x@directs[[raised_direct]]$switches) ) {
                      sparsed <- parseSwitch(x, post_cmdargs, dswitches)
                      parsed[[raised_direct]] <- c(parsed[[raised_direct]], sparsed$argv)
                      post_cmdargs <- sparsed$cmdargs_consumed
                  }
                  # :sub-opt
                  if ( length(dopt <- x@directs[[raised_direct]]$opt) ) {
                      oparsed <- parseOpt(x, post_cmdargs, dopt)
                      parsed[[raised_direct]] <- c(parsed[[raised_direct]], oparsed$argv)
                      post_cmdargs <- oparsed$cmdargs_consumed
                  }
             }

              list(argv=parsed, 
                   cmdargs_consumed=c(pre_cmdargs, post_cmdargs))
          })



