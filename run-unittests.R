#!/usr/bin/env Rscript

library(methods)
library(testthat)
library(magrittr)

source("ArgParser.R")

test_dir("unittests", reporter="summary")
