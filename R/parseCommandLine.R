#' @include ArgParser.R
NULL

#' Parse command line arguments via given ArgParser.
#' 
#' @param x An ArgParser object.
#' @param cmdargs A character vector representing command line arguments.
#' @param trim_prefix A logical vector indicating wheather to strip dash prefix in argument names in parsed result.
#' @return A list of parsed command line arguments.
#' @examples
#' library(magrittr)
#' cmdargs <- strsplit("prog.R --f1 v1", ' ')[[1]]
#' p <- ArgParser() %>%
#'    addFlag("--f1") %>%
#'    addFlag("--f2")
#' parseCommandLine(p, cmdargs=cmdargs)

setGeneric("parseCommandLine", def=function(x, cmdargs, ...) standardGeneric("parseCommandLine"))

setMethod("parseCommandLine", signature=c(x="ArgParser", cmdargs="character"), 
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
