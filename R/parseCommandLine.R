#' @include ArgParser.R
NULL

#' Parse command line arguments via given ArgParser.
#' 
#' @param x An ArgParser object.
#' @param ... Other arguments used in dispatched method.

#' @export
setGeneric("parseCommandLine", def=function(x, ...) standardGeneric("parseCommandLine"))

#' @describeIn parseCommandLine
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

#' @export
setMethod("parseCommandLine", signature=c(x="ArgParser"), 
          definition=function(x, cmdargs=commandArgs(), trim_prefix=FALSE) {

              ## print usage then exit if --help is raised
              if ( any(c("--help", "-h") %in% cmdargs) ) {
                  printUsageString(x, cmdargs)
                  quit(status=0)
              }
              
              parsed <- list()
              all_argnames <- c(names(x@flags), names(x@switches_logic), names(x@switches_any))

              ## parse flags
              parsed_flags <- parseFlag(x=x, cmdargs=cmdargs)
              parsed <- c(parsed, parsed_flags$argv)

              ## parse switches
              parsed_switches <- parseSwitch(x=x, cmdargs=parsed_flags$cmdargs_consumed)
              parsed <- c(parsed, parsed_switches$argv)

              ## parse positional args (opt)
              if ( !is.na(args_idx <- match("--args", parsed_switches$cmdargs_consumed)) ) {
                  opt_supplied <- TRUE
                  cmdargs_consumed <- parsed_switches$cmdargs_consumed[-c(1:args_idx)]
              } else {
                  opt_supplied <- FALSE
                  cmdargs_consumed <- parsed_switches$cmdargs_consumed
              }
              if ( !opt_supplied && length(x@opt) && any(x@opt_nrequired > 0) )
                  stop("No positional argument found.")              
              parsed_opt <- parseOpt(x=x, cmdargs_consumed=cmdargs_consumed)
              parsed <- c(parsed, parsed_opt$argv)

              if ( trim_prefix )
                  names(parsed) <- gsub("^--", '', names(parsed))

              parsed
          })
