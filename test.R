#!/usr/bin/env Rscript

library(methods)
library(magrittr)
source("ArgParser.R")
p <- ArgParser() %>% addFlag("--f1") %>%
    addFlag("--f2", optional=FALSE) %>%
    addSwitch("--s1", default=FALSE) %>%
    addSwitch("--s2", default=list(1,0))
pargs <- parseCommandLine(p, commandArgs())
for ( arg in names(pargs) )
    write(paste(arg, pargs[[arg]], '\t'), stdout())

