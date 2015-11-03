
setGeneric("parseSwitch", def=function(x, cmdargs, ...) standardGeneric("parseSwitch"))

setMethod("parseSwitch", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs, limited_to=NULL) {

              if ( !is.null(limited_to) ) {
                  x@switches_logic <- x@switches_logic[names(x@switches_logic) %in% limited_to]
                  x@switches_any <- x@switches_any[names(x@switches_any) %in% limited_to]
                  x@switches_alias <- x@switches_alias[names(x@switches_alias) %in% limited_to]
              }

              parsed <- list()

              # replace short alias with full name, if any
              if ( any(alias_defined <- !is.na(x@switches_alias)) ) {
                  switches_with_alias <- x@switches_alias[alias_defined]
                  if ( any(is_alias <- cmdargs %in% switches_with_alias) ) {
                      alias_raised <- cmdargs[is_alias]
                      for ( a in alias_raised ) 
                          cmdargs[cmdargs == a] <- names(which(x@switches_alias == a))
                  }    
              }

              # check duplicated switch names
              .checkDupArg(cmdargs, c(names(x@switches_logic), names(x@switches_any)))

              # get overall switch position
              if ( !is.null(all_snames <- c(names(x@switches_logic), names(x@switches_any))) ) {
                  switch_idx <- match(all_snames, cmdargs)
                  switch_idx <- switch_idx[!is.na(switch_idx)]
              }

              # parse logical switches
              parsed <- c(parsed, x@switches_logic)
              if ( any(pushed <- names(x@switches_logic) %in% cmdargs) ) {
                  pushed_switches_logic <- names(x@switches_logic)[pushed]
                  parsed[pushed_switches_logic] <- !x@switches_logic[pushed_switches_logic]
              }

              # parse ad-hoc switches pushed
              if ( any(pushed <- names(x@switches_any) %in% cmdargs) )
                  parsed <- c(parsed, lapply(x@switches_any[pushed], function(x) x[[2]]))

              # parse ad-hoc switches not pushed
              if ( any(unpushed <- !names(x@switches_any) %in% cmdargs) )
                  parsed <- c(parsed, lapply(x@switches_any[unpushed], function(x) x[[1]]))
              
              list(argv=parsed,
                   cmdargs_consumed=if (length(switch_idx)) cmdargs[-switch_idx] else cmdargs)
          })



