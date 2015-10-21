
#---------------------#
# define parser class #
#---------------------#
ArgParser <- setClass("ArgParser", 
                      slots=c(desc="character",
                              flags="list",
                              flags_alias="character",
                              flags_isOptional="logical",
                              switches_logic="logical",
                              switches_any="list",
                              switches_alias="character",
                              opt="character",
                              help="character"),
                      prototype=list(desc='',
                                     switches_logic=c(`--help`=FALSE),
                                     help=c(`--help`="show this message and exit"),
                                     flags_alias=c(`--help`="-h")),
                      validity=function(object) {
                          all_argnames <- c(names(object@switches_logic), 
                                            names(object@switches_any), 
                                            names(object@flags))
                          all_alias <- c(names(object@flags_alias),
                                         names(object@switches_alias))
                          if ( any(sapply(all_argnames, function(x) substr(x,1,2) != "--")) )
                              return("Name of flags/switches should have double-dash (--) prefix.")
                          if ( any(duplicated(all_argnames)) || any(duplicated(all_alias)) )
                              return("Duplicated flags/switches found.")
                          if ( any(sapply(all_alias, function(x) substr(x,1,1) != '-')) )
                              return("Short name alias should have single-dash (-) prefix.")
                          TRUE
                      })

#-----------------------------------#
# define methods for argument adder #
#-----------------------------------#
setGeneric("addFlag", def=function(x, f, default, ...) standardGeneric("addFlag"))
setMethod("addFlag", signature=c(x="ArgParser", f="character", default="missing"), 
          definition=function(x, f, short=NULL, optional=TRUE, help=NULL) {
              x@flags <- c(x@flags, setNames(NA, f))
              x@flags_isOptional <- c(x@flags_isOptional, setNames(optional, f))
              if ( !is.null(help) )
                  x@help <- c(x@help, setNames(help, f))
              validObject(x)
              x
          })
setMethod("addFlag", signature=c(x="ArgParser", f="character", default="ANY"), 
          definition=function(x, f, default, optional=TRUE, help=NULL) {
              if ( length(default) > 1 )
                  warning("Data for default is more than one, only keep the first.")
              x@flags <- c(x@flags, setNames(default[1], f))
              x@flags_isOptional <- c(x@flags_isOptional, setNames(optional, f))
              if ( !is.null(help) )
                  x@help <- c(x@help, setNames(help, f))
              validObject(x)
              x
          })

setGeneric("addSwitch", def=function(x, s, default, ...) standardGeneric("addSwitch"))
setMethod("addSwitch", signature=c(x="ArgParser", s="character", default="logical"), 
          definition=function(x, s, default=FALSE, help=NULL) {
              x@switches_logic <- c(x@switches_logic, setNames(default, s))
              if ( !is.null(help) )
                  x@help <- c(x@help, setNames(help, s))
              validObject(x)
              x
          })
setMethod("addSwitch", signature=c(x="ArgParser", s="character", default="list"), 
          definition=function(x, s, default, help=NULL) {
              if ( length(default) < 2 )
                  stop("The default, if a list, must be supplied both unpushed/pushed states, in order.")
              names(default) <- c("unpushed", "pushed")
              x@switches_any <- c(x@switches_any, setNames(list(default), s))
              if ( !is.null(help) )
                  x@help <- c(x@help, setNames(help, s))
              validObject(x)
              x
          })

setGeneric("addOpt", def=function(x, opt, ...) standardGeneric("addOpt"))
setMethod("addOpt", signature=c(x="ArgParser", opt="character", help=NULL), 
          definition=function(x, opt) {
              x@opt <- c(x@opt, opt)
              if ( !is.null(help) )
                  x@help <- c(x@help, setNames(help, opt))
              validObject(x)
              x
          })

#------------------------------------#
# define methods for argument parser #
#------------------------------------#
setGeneric(".parseFlag", def=function(x, cmdargs) standardGeneric(".parseFlag"))
setMethod(".parseFlag", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs) {
              
              parsed <- list()
              allargs <- c(names(x@flags), names(x@switches_logic), names(x@switches_any))
              
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
                  }

                  # parse flags with default value
                  f2 <- names(x@flags[with_default])
                  if ( length(f2_raised <- f2[f2 %in% cmdargs]) ) {
                      f2_idx <- match(f2_raised, cmdargs)
                      parsed <- c(parsed, x@flags[f2_raised])
                      f2_value <- setNames(cmdargs[f2_idx + 1L], f2_raised)
                      not_a_value <- f2_value %in% c(allargs, NA)
                      f2_overwrite_value <- f2_value[!not_a_value]
                      parsed[names(f2_overwrite_value)] <- f2_overwrite_value
                  }
              }
              parsed
          })

setGeneric(".parseSwitch", def=function(x, cmdargs) standardGeneric(".parseSwitch"))
setMethod(".parseSwitch", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs) {

              parsed <- list()
              
              # parse logical switches
              parsed <- c(parsed, x@switches_logic)
              if ( any(pushed <- names(x@switches_logic) %in% cmdargs) ) {
                  pushed_switches_logic <- names(x@switches_logic)[pushed]
                  parsed[pushed_switches_logic] <- !x@switches_logic[pushed_switches_logic]
              }

              # parse ad-hoc switches pushed
              if ( any(pushed <- names(x@switches_any) %in% cmdargs) )
                  parsed <- c(parsed, sapply(x@switches_any[pushed], function(x) x[[2]]))

              # parse ad-hoc switches not pushed
              if ( any(unpushed <- !names(x@switches_any) %in% cmdargs) )
                  parsed <- c(parsed, sapply(x@switches_any[unpushed], function(x) x[[1]]))
              
              parsed
          })

setGeneric(".parseOpt", def=function(x, cmdargs) standardGeneric(".parseOpt"))
setMethod(".parseOpt", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs) {
              parsed <- list()
              parsed
          })

setGeneric(".printUsageString", def=function(x) standardGeneric(".printUsageString"))
setMethod(".printUsageString", signature=c(x="ArgParser"),
          definition=function(x) {
              getHelpString <- function(f, h) sprintf("  %s\t%s", f, h)
              addBracket <- function(s) sprintf("[%s]", s)
              usage_line <- "Usage: prog.R"
              help_parag <- character(0)
              s1tag <- sapply(logic_switches <- names(x@switches_logic), addBracket)
              if ( ns1 <- length(s1tag) ) {
                  usage_line <- paste(usage_line, paste(s1tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Logical switch", ifelse(ns1 > 1, "es:", ':')))
                  for ( s in logic_switches )
                      help_parag <- c(help_parag, getHelpString(s, x@help[s]))
              }
              s2tag <- sapply(adhoc_switches <- names(x@switches_any), addBracket)
              if ( ns2 <- length(s2tag) ) {
                  usage_line <- paste(usage_line, paste(s2tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Ad-hoc switch", ifelse(ns2 > 1, "es:", ':')))
                  for ( s in adhoc_switches )
                      help_parag <- c(help_parag, getHelpString(s, x@help[s]))
              }
              f1tag <- forced_flags <- names(which(!x@flags_isOptional))
              if ( nf1 <- length(f1tag) ) {
                  usage_line <- paste(usage_line, paste(f1tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Forced flag", ifelse(nf1 > 1, "s:", ':')))
                  for ( f in forced_flags )
                      help_parag <- c(help_parag, getHelpString(f, x@help[f]))
              }
              f2tag <- sapply(optional_flags <- names(which(x@flags_isOptional)), addBracket)
              if ( nf2 <- length(f2tag) ) {
                  usage_line <- paste(usage_line, paste(f2tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Optional flag", ifelse(nf2 > 1, "s:", ':')))
                  for ( f in optional_flags )
                      help_parag <- c(help_parag, getHelpString(f, x@help[f]))
              }
              writeLines(usage_line)
              writeLines('')
              writeLines(x@desc)
              writeLines('')
              writeLines(help_parag)
          })

setGeneric("parseCommandLine", def=function(x, cmdargs, ...) standardGeneric("parseCommandLine"))
setMethod("parseCommandLine", signature=c(x="ArgParser", cmdargs="character"), 
          definition=function(x, cmdargs, trim_prefix=FALSE) {

              ## print usage then exit if --help is raised
              if ( any(c("--help", "-h") %in% cmdargs) ) {
                  .printUsageString(x)
                  quit(status=0)
              }
              
              parsed <- list()
              all_argnames <- c(names(x@flags), names(x@switches_logic), names(x@switches_any))

              ## check occurrence of forced flags, if any
              if ( length(forced_flags <- names(which(!x@flags_isOptional))) ) {
                  if ( !all(forced_and_raised <- forced_flags %in% cmdargs) )
                      stop(sprintf("Missing forced flag(s): %s", 
                                   paste(forced_flags[!forced_and_raised], collapse=", ")))
              }

              ## parse flags
              parsed_flags <- .parseFlag(x=x, cmdargs=cmdargs)
              parsed <- c(parsed, parsed_flags)

              ## parse switches
              parsed_switches <- .parseSwitch(x=x, cmdargs=cmdargs)
              parsed <- c(parsed, parsed_switches)

              ## parse positional args (opt)
              parsed_opt <- .parseOpt(x=x, cmdargs=cmdargs)
              parsed <- c(parsed, parsed_opt)

              if ( trim_prefix )
                  names(parsed) <- gsub("^--", '', names(parsed))

              parsed
          })



