#!/usr/bin/env Rscript

library(methods)
library(magrittr)
source("ArgParser.R")
p <- ArgParser(desc="A test for ArgParser") %>% 
    addFlag("--f1", help="this is an optional flag") %>%
    addFlag("--f2", help="this is an optional flag with default value", default="f1d") %>%
    addFlag("--f3", help="this is a forced flag", optional=FALSE) %>%
    addSwitch("--s1", help="a logical switch", default=FALSE) %>%
    addSwitch("--s2", help="an ad-hoc switch", default=list(1,0))
pargs <- parseCommandLine(p, commandArgs())
for ( arg in names(pargs) )
    write(paste(arg, pargs[[arg]], '\t'), stdout())

