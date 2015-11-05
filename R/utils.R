

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

# setup argument help message string for a given ArgParser
.getHelpMesg <- function(p, hide.na=FALSE, categorize=FALSE) {
    all_alias <- c(p@flags_alias, p@switches_alias)
    help_headers <- sapply(c(names(p@directs), 
                             names(p@flags),
                             names(p@switches_logic),
                             names(p@switches_any),
                             p@opt), 
                           function(x) 
                               sprintf("%s%s", ifelse(is.na(s <- all_alias[x]), 
                                                      '', paste0(s, ", ")), x))

    if ( hide.na )
        help_headers <- help_headers[names(help_headers) %in% names(p@help)]

    len_before_tab <- nchar(help_headers)
    len_fill <- max(len_before_tab) - len_before_tab + 4
    help_parag <- paste(help_headers, 
                        sapply(mapply(rep, times=len_fill, MoreArgs=list(x=' ')), paste, collapse=''), 
                        p@help[names(help_headers)])
    help_parag
}

# setup the usage string line
.getUsageLine <- function() {
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
