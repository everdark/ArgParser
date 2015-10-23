#!/usr/bin/env Rscript

library(methods)
library(magrittr)
source("ArgParser.R")
p <- ArgParser(desc="A test for ArgParser") %>% 
    addFlag("--flag1", "-f1", help="this is an optional flag") %>%
    addFlag("--flag2", help="this is an optional flag with default value", default="f1d") %>%
    addFlag("--flag3", help="this is a forced flag", optional=FALSE) %>%
    addSwitch("--logical-switch", "-s1", help="a logical switch", states=FALSE) %>%
    addSwitch("--adhoc-switch", "-s2", help="an ad-hoc switch", states=list(1,0))
pargs <- parseCommandLine(p, commandArgs())
for ( arg in names(pargs) )
    write(paste(arg, pargs[[arg]], '\t'), stdout())

