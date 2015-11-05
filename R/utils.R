

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
.getHelpParag <- function(p, hide.na=FALSE) {
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
    blanks <- 
        if ( length(len_fill) == 1 ) {
            paste(rep(' ', len_fill), collapse='')
        } else {
            sapply(mapply(rep, times=len_fill, MoreArgs=list(x=' ')), paste, collapse='')
        }
    help_parag <- paste(help_headers, 
                        blanks, 
                        p@help[names(help_headers)])

    setNames(help_parag, names(help_headers))
}

# format help paragraph
.formatHelpParag <- function(p, help_parag, categorize=TRUE) {

    formatted_parag <- character(0)

    if ( categorize ) {
        directs_parag <- help_parag[names(help_parag) %in% names(p@directs)]
        flags_parag <- help_parag[names(help_parag) %in% names(p@flags)]
        lswitches_parag <- help_parag[names(help_parag) %in% names(p@switches_logic)]
        aswitches_parag <- help_parag[names(help_parag) %in% names(p@switches_any)]
        opt_parag <- help_parag[names(help_parag %in% p@opt)]

        if ( nd <- length(directs_parag) ) {
            formatted_parag <- c(formatted_parag, paste0("Directive", ifelse(nd > 1, "s:", ':')))
            formatted_parag <- c(formatted_parag, paste("  ", directs_parag))
            formatted_parag <- c(formatted_parag, '') # one-line gap between directives and all the other arguments
        }
        if ( nf <- length(flags_parag) ) {
            formatted_parag <- c(formatted_parag, paste0("Flag argument", ifelse(nf > 1, "s:", ':')))
            formatted_parag <- c(formatted_parag, paste("  ", flags_parag))
        }
        if ( nls <- length(lswitches_parag) ) {
            formatted_parag <- c(formatted_parag, paste0("Logical Switch argument", ifelse(nls > 1, "s:", ':')))
            formatted_parag <- c(formatted_parag, paste("  ", lswitches_parag))
        }
        if ( nas <- length(aswitches_parag) ) {
            formatted_parag <- c(formatted_parag, paste0("Ad-hoc Switch argument", ifelse(nas > 1, "s:", ':')))
            formatted_parag <- c(formatted_parag, paste("  ", aswitches_parag))
        }
        if ( nopt <- length(opt_parag) ) {
            formatted_parag <- c(formatted_parag, paste0("Positional argument", ifelse(nopt > 1, "s:", ':')))
            formatted_parag <- c(formatted_parag, paste("  ", opt_parag))
        }
    }

    formatted_parag
}

# setup the usage string line
.getUsageLine <- function(p) {
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
