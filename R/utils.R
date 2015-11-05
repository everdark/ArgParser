

# check length of an argument and return its valid truncated
.checkArgLen <- function(arg, maxlen) {
    if ( maxlen < 1 )
        stop("Condition maxlen >= 1 must hold.")
    if ( length(arg) > maxlen ) 
        warning(sprintf("Argument %s has length > %s; only the first %s respected.", 
                        as.character(substitute(arg)), maxlen, maxlen))
    arg[1:maxlen]
}

# check if any argument is duplicated in a given command line string
.checkDupArg <- function(cmdargs, allargs) {
    arg_raised <- cmdargs[cmdargs %in% allargs]
    if ( any(is_dup <- duplicated(arg_raised)) )
        stop(sprintf("Some args are duplicated in given command line string: %s",
                     paste(arg_raised[is_dup])))
}

# setup help message for a given argument
.getHelpMesg <- function(argname, h, all_alias) {
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

# ensemble help message for directives
.ensembleDirHelpMesg <- function(p, usage_line=character(0), help_parag=character(0)) {
  if ( length(p@directs) ) {
      all_alias <- c(x@flags_alias, x@switches_alias)
      direct_ustring <- "DIRECTIVE"
      if ( x@directs_group[[1]]$is_optional )
          direct_ustring <- addBracket(direct_ustring)
      usage_line <- paste(usage_line, paste(direct_ustring, collapse=' '))

      nd <- length(x@directs)
      help_parag <- c(help_parag, paste0("Directive", ifelse(nd > 1, "s:", ':')))
      for ( d in names(x@directs) )
          help_parag <- c(help_parag, getHelpString(d, x@help[d], all_alias))
      help_parag <- c(help_parag, '')
  }

  list(hp=help_parag,
       ul=usage_line)
}
