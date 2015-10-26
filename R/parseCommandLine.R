
setGeneric("parseCommandLine", def=function(x, ...) standardGeneric("parseCommandLine"))

setMethod("parseCommandLine", signature=c(x="ArgParser"), 
          definition=function(x, cmdargs=commandArgs(), trim_prefix=FALSE) {

              ## print usage then exit if --help is raised
              if ( any(c("--help", "-h") %in% cmdargs) ) {
                  .printUsageString(x, cmdargs)
                  quit(status=0)
              }
              
              parsed <- list()
              all_argnames <- c(names(x@flags), names(x@switches_logic), names(x@switches_any))

              ## parse flags
              parsed_flags <- .parseFlag(x=x, cmdargs=cmdargs)
              parsed <- c(parsed, parsed_flags$argv)

              ## parse switches
              parsed_switches <- .parseSwitch(x=x, cmdargs=parsed_flags$cmdargs_consumed)
              parsed <- c(parsed, parsed_switches$argv)

              ## parse positional args (opt)
              if ( !is.na(args_idx <- match("--args", parsed_switches$cmdargs_consumed)) ) {
                  cmdargs_consumed <- parsed_switches$cmdargs_consumed[-c(1:args_idx)]
              } else {
                  cmdargs_consumed <- parsed_switches$cmdargs_consumed
              }
              parsed_opt <- .parseOpt(x=x, cmdargs_consumed=cmdargs_consumed)
              parsed <- c(parsed, parsed_opt)

              if ( trim_prefix )
                  names(parsed) <- gsub("^--", '', names(parsed))

              parsed
          })
