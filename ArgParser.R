
#---------------------#
# define helper func  #
#---------------------#
.checkArgLen <- function(arg, maxlen) {
    if ( length(arg) > maxlen ) 
        warning(sprintf("Argument %s has length > %s; only the first %s is respected.", 
                        as.character(substitute(arg)), maxlen, maxlen))
    arg[1:maxlen]
}

.checkDupArg <- function(cmdargs, allargs) {
    arg_raised <- cmdargs[cmdargs %in% allargs]
    if ( any(is_dup <- duplicated(arg_raised)) )
        stop(sprintf("Some flags/switches are duplicated in given command line string: %s",
                     paste(arg_raised[is_dup])))
}

#---------------------#
# define parser class #
#---------------------#
ArgParser <- setClass("ArgParser", 
                      slots=c(desc="character",
                              prog="character",
                              flags="list",
                              flags_alias="character",
                              flags_isOptional="logical",
                              switches_logic="logical",
                              switches_any="list",
                              switches_alias="character",
                              opt="character",
                              opt_narg="integer",
                              opt_nrequired="integer",
                              help="character"),
                      prototype=list(#desc='',
                                     #prog='',
                                     switches_logic=c(`--help`=FALSE),
                                     switches_alias=c(`--help`="-h"),
                                     help=c(`--help`="show this message and exit")
                                     ),
                      validity=function(object) {
                          all_switches_flags <- c(names(object@switches_logic), 
                                                  names(object@switches_any), 
                                                  names(object@flags))
                          all_alias <- c(object@flags_alias,
                                         object@switches_alias)
                          all_alias <- all_alias[!is.na(all_alias)]
                          if ( any(sapply(all_switches_flags, function(x) substr(x,1,2) != "--")) )
                              return("Name of flags/switches should have double-dash (--) prefix.")
                          if ( any(duplicated(c(all_switches_flags, object@opt, all_alias))) )
                              return("Duplicated flags/switches/opt found.")
                          if ( any(sapply(all_alias, function(x) substr(x,1,1) != '-')) )
                              return("Short name alias should have single-dash (-) prefix.")
                          TRUE
                      })

setMethod("initialize", signature="ArgParser", 
          definition=function(.Object, desc='', prog='') {
              prog <- .checkArgLen(prog, 1)
              .Object@desc <- desc
              .Object@prog <- prog
              .Object
          })

#-----------------------------------#
# define methods for argument adder #
#-----------------------------------#
setGeneric("addFlag", def=function(x, name, ...) standardGeneric("addFlag"))
setMethod("addFlag", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, short=NULL, default=NULL, optional=TRUE, help=NULL) {
              name <- .checkArgLen(name, 1)
              if ( !is.null(default) ) {
                  default <- .checkArgLen(default, 1)
                  x@flags <- c(x@flags, setNames(default, name))
              } else {
                  x@flags <- c(x@flags, setNames(NA, name))
              }
              x@flags_alias <- c(x@flags_alias, setNames(ifelse(is.null(short), NA_character_, short), name))
              x@flags_isOptional <- c(x@flags_isOptional, setNames(optional, name))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }
              validObject(x)
              x
          })

setGeneric("addSwitch", def=function(x, name, ...) standardGeneric("addSwitch"))
setMethod("addSwitch", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, short=NULL, states=FALSE, help=NULL) {
              name <- .checkArgLen(name, 1)
              if ( !is.vector(states) ) 
                  stop("Value of states must be of type vector.")
              if ( is.logical(states) ) {
                  states <- .checkArgLen(states, 1)
                  x@switches_logic <- c(x@switches_logic, setNames(states, name))
              } else {
                  if ( length(states) < 2 )
                      stop("Non-logical states vector should have length 2.")
                  states <- .checkArgLen(states, 2)
                  names(states) <- c("unpushed", "pushed")
                  x@switches_any <- c(x@switches_any, setNames(list(as.list(states)), name))
              }
              x@switches_alias <- c(x@switches_alias, setNames(ifelse(is.null(short), NA_character_, short), name))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }
              validObject(x)
              x
          })

setGeneric("addOpt", def=function(x, name, ...) standardGeneric("addOpt"))
setMethod("addOpt", signature=c(x="ArgParser", name="character"), 
          definition=function(x, name, help=NULL, narg=1L, nrequired=narg) {
              name <- .checkArgLen(name, 1)
              narg <- as.integer(.checkArgLen(narg, 1))
              nrequired <- as.integer(.checkArgLen(nrequired, 1))
              if ( nrequired > narg )
                  stop("Found nrequired > narg, which is impossible.")
              if ( nrequired < narg )
                  warning("Found nrequired < narg, then this opt MUST be the last opt defined so it will work.")
              x@opt <- c(x@opt, name)
              x@opt_narg <- c(x@opt_narg, setNames(narg, name))
              x@opt_nrequired <- c(x@opt_nrequired, setNames(nrequired, name))
              if ( !is.null(help) ) {
                  help <- .checkArgLen(help, 1)
                  x@help <- c(x@help, setNames(help, name))
              }
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

setGeneric(".parseSwitch", def=function(x, cmdargs) standardGeneric(".parseSwitch"))
setMethod(".parseSwitch", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs) {

              parsed <- list()

              # replace short alias with full name, if any
              if ( any(alias_defined <- !is.na(x@switches_alias)) ) {
                  switches_with_alias <- x@switches_alias[alias_defined]
                  if ( any(is_alias <- cmdargs %in% switches_with_alias) ) {
                      alias_raised <- cmdargs[is_alias]
                      cmdargs[cmdargs == alias_raised] <- names(which(x@switches_alias == alias_raised))
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
                  parsed <- c(parsed, sapply(x@switches_any[pushed], function(x) x[[2]]))

              # parse ad-hoc switches not pushed
              if ( any(unpushed <- !names(x@switches_any) %in% cmdargs) )
                  parsed <- c(parsed, sapply(x@switches_any[unpushed], function(x) x[[1]]))
              
              list(argv=parsed,
                   cmdargs_consumed=if (length(switch_idx)) cmdargs[-switch_idx] else cmdargs)
          })

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

setGeneric(".printUsageString", def=function(x, cmdargs, ...) standardGeneric(".printUsageString"))
setMethod(".printUsageString", signature=c(x="ArgParser", cmdargs="character"),
          definition=function(x, cmdargs, align=TRUE) {
              # define helper func
              getHelpString <- function(argname, h, all_alias) {
                  if ( !is.na(short <- all_alias[argname]) ) {
                      out <- sprintf("  %s, %s\t%s", short, argname, h)
                  } else {
                      out <- sprintf("  %s\t%s", argname, h)
                  }
                  if ( align ) {
                      all_alias_defined <- all_alias[!is.na(all_alias)]
                      max_len <- max(nchar(all_alias_defined) + nchar(names(all_alias_defined))) + 4
                      tag_len <- ifelse(is.na(short), -2, nchar(short)) + nchar(argname) + 2
                      out <- gsub("\t", paste(rep(' ', max_len - tag_len), collapse=''), out)
                  }
                  out
              }
              addBracket <- function(s) sprintf("[%s]", s)

              # get script name
              prog_fullpath <- grep("^--file=", cmdargs, value=TRUE)
              prog_name <- ifelse(length(prog_fullpath), basename(prog_fullpath), '')
              usage_line <- sprintf("Usage: %s", ifelse(x@prog == '', prog_name, x@prog))

              # ensemble help paragraph
              help_parag <- character(0)
              all_alias <- c(x@flags_alias, x@switches_alias)
              s1tag <- sapply(logic_switches <- names(x@switches_logic), addBracket)
              if ( ns1 <- length(s1tag) ) {
                  usage_line <- paste(usage_line, paste(s1tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Logical switch", ifelse(ns1 > 1, "es:", ':')))
                  for ( s in logic_switches )
                      help_parag <- c(help_parag, getHelpString(s, x@help[s], all_alias))
              }
              s2tag <- sapply(adhoc_switches <- names(x@switches_any), addBracket)
              if ( ns2 <- length(s2tag) ) {
                  usage_line <- paste(usage_line, paste(s2tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Ad-hoc switch", ifelse(ns2 > 1, "es:", ':')))
                  for ( s in adhoc_switches )
                      help_parag <- c(help_parag, getHelpString(s, x@help[s], all_alias))
              }
              f1tag <- forced_flags <- names(which(!x@flags_isOptional))
              if ( nf1 <- length(f1tag) ) {
                  usage_line <- paste(usage_line, paste(f1tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Forced flag", ifelse(nf1 > 1, "s:", ':')))
                  for ( f in forced_flags )
                      help_parag <- c(help_parag, getHelpString(f, x@help[f], all_alias))
              }
              f2tag <- sapply(optional_flags <- names(which(x@flags_isOptional)), addBracket)
              if ( nf2 <- length(f2tag) ) {
                  usage_line <- paste(usage_line, paste(f2tag, collapse=' '))
                  help_parag <- c(help_parag, paste0("Optional flag", ifelse(nf2 > 1, "s:", ':')))
                  for ( f in optional_flags )
                      help_parag <- c(help_parag, getHelpString(f, x@help[f], all_alias))
              }
              if ( nopt <- length(x@opt) ) {
                  help_parag <- c(help_parag, paste0("Positional argument", ifelse(nopt > 1, "s:", ':')))
                  for ( opt in x@opt ) {
                      usage_line <- 
                      if ( (narg <- x@opt_narg[opt]) > (nreq <- x@opt_nrequired[opt]) ) {
                          paste(usage_line, paste0(paste(rep(opt, nreq), collapse=' '), "[...]"))
                      } else {
                          paste(usage_line, paste(rep(opt, narg), collapse=' '))
                      }
                      help_parag <- c(help_parag, getHelpString(opt, x@help[opt], all_alias))
                  }
              }

              # write out
              writeLines(usage_line)
              writeLines('')
              writeLines(x@desc)
              writeLines('')
              writeLines(help_parag)
          })

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



