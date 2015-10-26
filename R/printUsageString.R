
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
