
context("Parse command line arguments")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

argParser1 <- ArgParser() %>%
    addFlag("--f1") %>%
    addFlag("--f2")

cmdargs1 <- getTestInput("prog.R --f1 v1")
cmdargs2 <- getTestInput("prog.R --f1")
cmdargs3 <- getTestInput("prog.R --f1 v1 --f2 v2")
cmdargs4 <- getTestInput("prog.R --f1 --f2 v2")
cmdargs5 <- getTestInput("prog.R --f1 --f2")

test_that("flags without default value should be successfully parsed provided values are given", {
          expect_identical(parseCommandLine(argParser1, cmdargs1), list(`--f1`="v1"))
          expect_error(parseCommandLine(argParser1, cmdargs2))
          expect_identical(parseCommandLine(argParser1, cmdargs3), list(`--f1`="v1", `--f2`="v2"))
          expect_error(parseCommandLine(argParser1, cmdargs4))
          expect_error(parseCommandLine(argParser1, cmdargs5))
})

cmdargs6 <- getTestInput("prog.R --f1 --f3")

test_that("flag value prefixed with '--' is okay if that value is not a flag nor a switch", {
          expect_identical(parseCommandLine(argParser1, cmdargs6), list(`--f1`="--f3"))
})
