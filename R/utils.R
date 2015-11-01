

# check length of an argument and return its valid truncated
.checkArgLen <- function(arg, maxlen) {
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
