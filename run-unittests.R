#!/usr/bin/env Rscript

library(methods)
library(testthat)
library(magrittr)

invisible(sapply(dir("./R", full.names=TRUE), source))

test_dir("tests/testthat", reporter="summary")
