
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

#------------------------------#
# test for comprehensive usage #
#------------------------------#

argParser5 <- ArgParser() %>%
    addFlag("--flag", "-f") %>%
    addSwitch("--switch", "-s") %>%
    addOpt("opt")

cmdargs25 <- getTestInput("prog.R --args -f vf -s a")
cmdargs26 <- getTestInput("prog.R --args a -f vf -s")
cmdargs27 <- getTestInput("prog.R --args -f vf a -s")

test_that("all arguments work together properly: opt can be inter-positioned", {
          expect_identical(parseCommandLine(argParser5, cmdargs25), list(`--flag`="vf", `--help`=F, `--switch`=T, opt='a'))
          expect_identical(parseCommandLine(argParser5, cmdargs26), list(`--flag`="vf", `--help`=F, `--switch`=T, opt='a'))
          expect_identical(parseCommandLine(argParser5, cmdargs27), list(`--flag`="vf", `--help`=F, `--switch`=T, opt='a'))
})

cmdargs28 <- getTestInput("prog.R --args -s -f vf a")
cmdargs29 <- getTestInput("prog.R --args a -f vf -s")
cmdargs30 <- getTestInput("prog.R --args -s -f vf a")

test_that("all arguments work together properly: order of flags/siwtches do not matter", {
          expect_identical(parseCommandLine(argParser5, cmdargs28), list(`--flag`="vf", `--help`=F, `--switch`=T, opt='a'))
          expect_identical(parseCommandLine(argParser5, cmdargs29), list(`--flag`="vf", `--help`=F, `--switch`=T, opt='a'))
          expect_identical(parseCommandLine(argParser5, cmdargs30), list(`--flag`="vf", `--help`=F, `--switch`=T, opt='a'))
})

cmdargs31 <- getTestInput("prog.R --args a")
cmdargs32 <- getTestInput("prog.R --args a -s")
cmdargs33 <- getTestInput("prog.R --args a -f vf")

test_that("all arguments work together properly: optional flag and switch is indeed optional", {
          expect_identical(parseCommandLine(argParser5, cmdargs31), list(`--help`=F, `--switch`=F, opt='a'))
          expect_identical(parseCommandLine(argParser5, cmdargs32), list(`--help`=F, `--switch`=T, opt='a'))
          expect_identical(parseCommandLine(argParser5, cmdargs33), list(`--flag`="vf", `--help`=F, `--switch`=F, opt='a'))
})

argParser6 <- ArgParser() %>%
    addFlag("--flag1", "-f1") %>%
    addFlag("--flag2", "-f2", optional=FALSE) %>%
    addFlag("--flag3", "-f3", default="df") %>%
    addSwitch("--switch1", "-s1") %>%
    addSwitch("--switch2", "-s2", states=1:2L) %>%
    addOpt("opt1", 2, 2) %>%
    addOpt("opt2")

cmdargs34 <- getTestInput("prog.R --args -f1 1 -f2 2 -f3 3 -s1 -s2 a b c")
cmdargs35 <- getTestInput("prog.R --args -f1 1 -f2 2 -s1 -s2 a b c d -f3")
cmdargs36 <- getTestInput("prog.R --args -f2 2 a b c")

test_that("all arguments work togetjer properly: ", {
          expect_identical(parseCommandLine(argParser6, cmdargs34), 
                           list(`--flag1`='1', `--flag2`='2', `--flag3`='3', `--help`=F, `--switch1`=T, `--switch2`=2L, opt1=c('a','b'), opt2='c'))
          expect_identical(parseCommandLine(argParser6, cmdargs35), 
                           list(`--flag1`='1', `--flag2`='2', `--flag3`='df', `--help`=F, `--switch1`=T, `--switch2`=2L, opt1=c('a','b'), opt2='c'))
          expect_identical(parseCommandLine(argParser6, cmdargs36), 
                           list(`--flag2`='2', `--help`=F, `--switch1`=F, `--switch2`=1L, opt1=c('a','b'), opt2='c'))
})

cmdargs37 <- getTestInput("prog.R --args -f2 2 a b")
cmdargs38 <- getTestInput("prog.R --args -f2 2 -f3 3 a b -s1")
cmdargs39 <- getTestInput("prog.R --args -s1 a -f2 2 b -s2 -f3")

test_that("all arguments work togetjer properly: insufficient opt cause error", {
          expect_error(parseCommandLine(argParser6, cmdargs37), "^.*less than required.*")
          expect_error(parseCommandLine(argParser6, cmdargs38), "^.*less than required.*")
          expect_error(parseCommandLine(argParser6, cmdargs39), "^.*less than required.*")
})

cmdargs40 <- getTestInput("prog.R --args -f2 2 a b c -f2 3")
cmdargs41 <- getTestInput("prog.R --args -f2 2 a b -s1 c --switch1")
cmdargs42 <- getTestInput("prog.R --args -s1 a -f2 2 b -s2 -f3 --flag3")

test_that("all arguments work togetjer properly: duplicated flags/switches cause error", {
          expect_error(parseCommandLine(argParser6, cmdargs40), "^.*duplicated.*")
          expect_error(parseCommandLine(argParser6, cmdargs41), "^.*duplicated.*")
          expect_error(parseCommandLine(argParser6, cmdargs42), "^.*duplicated.*")
})

