#' An S4 class to represent a command line arguments parser.
#' 
#' @slot desc A description shown in usage. desc with length > 1 will be printed in seperated lines.
#' @slot prog A program name shown in usage, if not set, will default to the value of "--file=" flag given in commandArg().
#' @return An S4 object of class ArgParser
#' @examples
#' p1 <- ArgParser(desc="a test description", prog="test.R")
#' p2 <- ArgParser(desc=c("a test description", "that can have multiple lines"))

#' @export ArgParser
#' @exportClass ArgParser
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

