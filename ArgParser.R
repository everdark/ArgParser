
ArgParser <- setClass("ArgParser", 
                      slots=c(cmdargs="character", 
                              flags="character",
                              flagsd="character",
                              flagso="logical",
                              switches="character",
                              switchesd="logical",
                              opt="character"))

setGeneric("addFlag", def=function(x, f, default, ...) standardGeneric("addFlag"))
setMethod("addFlag", signature=c(x="ArgParser", f="character", default="missing"), 
          definition=function(x, f, optional=TRUE) {
              x@flags <- c(x@flags, f)
              x@flagso <- c(x@flagso, setNames(optional, f))
              x
          })
setMethod("addFlag", signature=c(x="ArgParser", f="character", default="ANY"), 
          definition=function(x, f, default, optional=TRUE) {
              x@flags <- c(x@flags, f)
              x@flagso <- c(x@flagso, setNames(optional, f))
              x@flagsd <- c(x@flagsd, setNames(default, f))
              x
          })

setGeneric("addSwitch", def=function(x, s, default) standardGeneric("addSwitch"))
setMethod("addSwitch", signature=c(x="ArgParser", s="character", default="logical"), 
          definition=function(x, s, default=FALSE) {
              x@switches <- c(x@switches, s)
              x@switchesd <- c(x@switchesd, setNames(default, s))
              x
          })

setGeneric("addOpt", def=function(x, opt) standardGeneric("addOpt"))
setMethod("addOpt", signature=c(x="ArgParser", opt="character"), 
          definition=function(x, opt) {
              x@opt <- c(x@opt, opt)
              x
          })

setGeneric("parseCommandLine", def=function(x, cmdargs) standardGeneric("parseCommandLine"))
setMethod("parseCommandLine", signature=c(x="ArgParser", cmdargs="character"), 
          definition=function(x, cmdargs) {
              
              parsed <- list()
              all_argnames <- c(x@flags, names(x@flagsd), x@switches)

              ## parse flags ##
              with_default <- x@flags %in% names(x@flagsd)
              f1 <- x@flags[!with_default]
              if ( length(f1_raised <- f1[f1 %in% cmdargs]) ) {
                  f1_idx <- match(f1_raised, cmdargs)
                  parsed <- c(parsed, setNames(cmdargs[f1_idx + 1L], f1_raised))
                  invalid_value <- parsed %in% c(all_argnames, NA)
                  if ( any(invalid_value) )
                      stop(sprintf("Invalid input in: %s.", 
                                   paste(names(parsed[invalid_value]), collapse=', ')))
              }
              f2 <- x@flags[with_default]
              if ( length(f2_raised <- f2[f2 %in% cmdargs]) ) {
                  f2_idx <- match(f2_raised, cmdargs)
                  parsed <- c(parsed, x@flagsd[f2_raised])
                  f2_value <- setNames(cmdargs[f2_idx + 1L], f2_raised)
                  not_a_value <- f2_value %in% c(all_argnames, NA)
                  f2_overwrite_value <- f2_value[!not_a_value]
                  parsed[names(f2_overwrite_value)] <- f2_overwrite_value
              }
              ## parse switches ##
              parsed <- c(parsed, setNames(x@switchesd, x@switches))
              if ( length(s_pushed <- x@switches[x@switches %in% cmdargs]) )
                  parsed[s_pushed] <- !unlist(parsed[s_pushed])

              # parse positional args (opt)

              parsed
          })



