#' @importFrom methods setClass setGeneric setMethod
NULL

#' An S4 class to represent a command line arguments parser.
#' 
#' @slot desc A description shown in usage. desc with length > 1 will be printed in seperated lines.
#' @slot prog A program name shown in usage, if not set, will default to the value of "-{}-file=" flag given in commandArg().
#' @slot flags A list of defined flag arguments.
#' @slot flags_alias A character vector of defined flag alias.
#' @slot flags_isOptional A logical vector indicating wheather a flag is optional or not.
#' @slot switches_logic A logical vector of defined logical switch arguments.
#' @slot switches_any A list of defined ad-hoc switch arguments.
#' @slot switches_alias A character vector of defined switch alias.
#' @slot opt A character vector of defined positional arguments.
#' @slot opt_narg An integer vector indicating the number of positinal arguments to consume for each opt.
#' @slot opt_nrequired An integer vector indicating the number of required positional arguments to consume for each opt.
#' @slot directs A list of defined directives.
#' @slot directs_isOptional A logical vector indicating wheather a directive is optional or not.
#' @slot help A character vector of defined help message for each argument.

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
                              directs="list",
                              directs_isOptional="logical",
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
                          if ( any(duplicated(c(all_switches_flags, object@opt, names(object@directs), all_alias))) )
                              return("Duplicated arg name found.")
                          if ( any(sapply(all_alias, function(x) substr(x,1,1) != '-')) )
                              return("Short name alias should have single-dash (-) prefix.")
                          TRUE
                      })

#' @describeIn ArgParser
#' The Constructer used for class ArgParser.
#'
#' @param .Object A prototyped object of class ArgParser.
#' @param desc A description shown in usage. desc with length > 1 will be printed in seperated lines.
#' @param prog A program name shown in usage, if not set, will default to the value of "-{}-file=" flag given in commandArg().
#' @return An S4 object of class ArgParser.
#' @examples
#' p1 <- ArgParser(desc="a test description", prog="test.R")
#' p2 <- ArgParser(desc=c("a test description", "that can have multiple lines"))

#' @export
setMethod("initialize", signature="ArgParser", 
          definition=function(.Object, desc='', prog='') {
              prog <- .checkArgLen(prog, 1)
              .Object@desc <- desc
              .Object@prog <- prog
              .Object
          })

