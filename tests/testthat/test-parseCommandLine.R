
context("Parse command line arguments")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

#-----------------------------------------------#
# test for optional flags without default value #
#-----------------------------------------------#

argParser1 <- ArgParser() %>%
    addFlag("--f1") %>%
    addFlag("--f2")

cmdargs1 <- getTestInput("prog.R --f1 v1")
cmdargs2 <- getTestInput("prog.R --f1 v1 --f2 v2")

test_that("flags without default value should be successfully parsed provided values are well followed", {
          expect_identical(parseCommandLine(argParser1, cmdargs1), list(`--f1`="v1", `--help`=FALSE))
          expect_identical(parseCommandLine(argParser1, cmdargs2), list(`--f1`="v1", `--f2`="v2", `--help`=FALSE))
})

cmdargs3 <- getTestInput("prog.R --f1")
cmdargs4 <- getTestInput("prog.R --f1 --f2 v2")
cmdargs5 <- getTestInput("prog.R --f1 --f2")

test_that("flags without default value and not followed by corresponding values will cause error", {
          expect_error(parseCommandLine(argParser1, cmdargs3))
          expect_error(parseCommandLine(argParser1, cmdargs4))
          expect_error(parseCommandLine(argParser1, cmdargs5))
})

cmdargs6 <- getTestInput("prog.R --f1 --f3")

test_that("flag value prefixed with '--' is okay if that value is not a flag nor a switch", {
          expect_identical(parseCommandLine(argParser1, cmdargs6), list(`--f1`="--f3", `--help`=FALSE))
})

cmdargs6_1 <- getTestInput("prog.R --f1 v1 --f2 --f1")

test_that("flag values collide with other flag names will cause error", {
          expect_error(parseCommandLine(argParser1, cmdargs6_1))
})

cmdargs7 <- getTestInput("prog.R")
cmdargs8 <- getTestInput("prog.R --f3")

test_that("optional flags not found should be okay", {
          expect_identical(parseCommandLine(argParser1, cmdargs7), list(`--help`=FALSE))
          expect_identical(parseCommandLine(argParser1, cmdargs8), list(`--help`=FALSE))
})

cmdargs9 <- getTestInput("prog.R --f1 v1 --f1 v2")
cmdargs10 <- getTestInput("prog.R --f1 v1 --f2 v1 --f1 v2 --f2 v2")

test_that("duplicated flags cause error", {
          expect_error(parseCommandLine(argParser1, cmdargs9))
          expect_error(parseCommandLine(argParser1, cmdargs10))
})

#--------------------------------------------#
# test for optional flags with default value #
#--------------------------------------------#

argParser2 <- ArgParser() %>%
    addFlag("--f1", default="f1_default") %>%
    addFlag("--f2", default="f2_default")

cmdargs11 <- getTestInput("prog.R --f1")
cmdargs12 <- getTestInput("prog.R --f1 --f2")

test_that("flags with default value could be followed no value", {
          expect_identical(parseCommandLine(argParser2, cmdargs11), list(`--f1`="f1_default", `--help`=FALSE))
          expect_identical(parseCommandLine(argParser2, cmdargs12), list(`--f1`="f1_default", `--f2`="f2_default", `--help`=FALSE))
})

cmdargs13 <- getTestInput("prog.R --f1 v1")
cmdargs14 <- getTestInput("prog.R --f1 --f2 v2")
cmdargs15 <- getTestInput("prog.R --f1 v1 --f2 v2")

test_that("flags with default value should be overwritten if well followed by valid values", {
          expect_identical(parseCommandLine(argParser2, cmdargs13), list(`--f1`="v1", `--help`=FALSE))
          expect_identical(parseCommandLine(argParser2, cmdargs14), list(`--f1`="f1_default", `--f2`="v2", `--help`=FALSE))
          expect_identical(parseCommandLine(argParser2, cmdargs15), list(`--f1`="v1", `--f2`="v2", `--help`=FALSE))
})

cmdargs16 <- getTestInput("prog.R --f1 v1 --f1 v2")

test_that("duplicated flags with default value cause error", {
          expect_error(parseCommandLine(argParser2, cmdargs16))
})

#-------------------#
# test for switches #
#-------------------#

argParser3 <- ArgParser() %>%
    addSwitch("--s1", states=TRUE) %>%
    addSwitch("--s2", states=FALSE)

cmdargs17 <- getTestInput("prog.R")

test_that("default values are respected if switch not pushed", {
          expect_identical(parseCommandLine(argParser3, cmdargs17), list(`--help`=FALSE, `--s1`=TRUE, `--s2`=FALSE))
})

cmdargs18 <- getTestInput("prog.R --s1")
cmdargs19 <- getTestInput("prog.R --s1 --s2")

test_that("default values are flipped given switch pushed", {
          expect_identical(parseCommandLine(argParser3, cmdargs18), list(`--help`=FALSE, `--s1`=FALSE, `--s2`=FALSE))
          expect_identical(parseCommandLine(argParser3, cmdargs19), list(`--help`=FALSE, `--s1`=FALSE, `--s2`=TRUE))
})

cmdargs20 <- getTestInput("prog.R --s1 v1")
cmdargs21 <- getTestInput("prog.R --s1 v1 --s2 v2")

test_that("follow-by values for switches are ignored", {
          expect_identical(parseCommandLine(argParser3, cmdargs18), list(`--help`=FALSE, `--s1`=FALSE, `--s2`=FALSE))
          expect_identical(parseCommandLine(argParser3, cmdargs19), list(`--help`=FALSE, `--s1`=FALSE, `--s2`=TRUE))
})

#-----------------------#
# test for forced flags #
#-----------------------#

argParser4 <- ArgParser() %>%
    addFlag("--f1", default="f1_default") %>%
    addFlag("--f2", optional=FALSE) %>%
    addFlag("--f3", optional=FALSE, default="f3_default")

cmdargs22 <- getTestInput("prog.R --f1")
cmdargs23 <- getTestInput("prog.R --f2")

test_that("forced flag not raised will cause error", {
          expect_error(parseCommandLine(argParser4, cmdargs22))
          expect_error(parseCommandLine(argParser4, cmdargs23))
})

cmdargs24 <- getTestInput("prog.R --f2 v2 --f3 --f2 v20")

test_that("duplicated forced flags cause error", {
          expect_error(parseCommandLine(argParser4, cmdargs24))
})
