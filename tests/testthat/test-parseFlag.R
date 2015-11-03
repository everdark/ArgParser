
context("Parse flags in command line argument string")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

p1 <- ArgParser() %>%
    addFlag("--f1") %>%
    addFlag("--f2")

cmdargs1 <- getTestInput("prog.R --f1 v1")
cmdargs2 <- getTestInput("prog.R --f1 v1 --f2 v2")

test_that("flags without default value are properly consumed", {
          expect_identical(parseFlag(p1, cmdargs1), 
                           list(argv=list(`--f1`="v1"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p1, cmdargs2), 
                           list(argv=list(`--f1`="v1", `--f2`="v2"), cmdargs_consumed="prog.R"))
})

cmdargs2_1 <- getTestInput("prog.R --f1")
cmdargs2_2 <- getTestInput("prog.R --f1 v1 --f2")
cmdargs2_3 <- getTestInput("prog.R --f1 --f2")
cmdargs2_4 <- getTestInput("prog.R --f1 --f2 v2")

test_that("flags without default value must be supplied value; otherwise error is thrown", {
          expect_error(parseFlag(p1, cmdargs2_1))
          expect_error(parseFlag(p1, cmdargs2_2))
          expect_error(parseFlag(p1, cmdargs2_3))
          expect_error(parseFlag(p1, cmdargs2_4))
})

cmdargs2_5 <- getTestInput("prog.R a b c")

test_that("command line string without any flag can be properly by-passed", {
          expect_identical(parseFlag(p1, cmdargs2_5), 
                           list(argv=list(), cmdargs_consumed=c("prog.R", 'a', 'b', 'c')))
})

p2 <- ArgParser() %>%
    addFlag("--flag1") %>%
    addFlag("--flag2", "-f2") %>%
    addFlag("--flag3", "-f3")

cmdargs3 <- getTestInput("prog.R --flag1 v1")
cmdargs4 <- getTestInput("prog.R --flag1 v1 --flag2 v2 -f3 v3")
cmdargs5 <- getTestInput("prog.R a --flag3 v3 b --flag2 v2 c")
cmdargs6 <- getTestInput("prog.R -f2 v2 -f3 v3")
cmdargs7 <- getTestInput("prog.R a -f2 v2 b -f3 v3")

test_that("flags with alias and without default are properly consumed", {
          expect_identical(parseFlag(p2, cmdargs3), 
                           list(argv=list(`--flag1`="v1"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p2, cmdargs4), 
                           list(argv=list(`--flag1`="v1", `--flag2`="v2", `--flag3`="v3"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p2, cmdargs5), 
                           list(argv=list(`--flag2`="v2", `--flag3`="v3"), cmdargs_consumed=c("prog.R", 'a' ,'b', 'c')))
          expect_identical(parseFlag(p2, cmdargs6), 
                           list(argv=list(`--flag2`="v2", `--flag3`="v3"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p2, cmdargs7), 
                           list(argv=list(`--flag2`="v2", `--flag3`="v3"), cmdargs_consumed=c("prog.R", 'a', 'b')))
})

cmdargs8 <- getTestInput("prog.R -f2 v2 -f2 v3")
cmdargs9 <- getTestInput("prog.R --flag2 v2 -f2 v3")
cmdargs10 <- getTestInput("prog.R --flag2 v2 --flag2 v3")

test_that("duplicated flag names and/or alias cause error", {
          expect_error(parseFlag(p2, cmdargs8))
          expect_error(parseFlag(p2, cmdargs9))
          expect_error(parseFlag(p2, cmdargs10))
})

p3 <- ArgParser() %>%
    addFlag("--flag1", default="v1") %>%
    addFlag("--flag2", "-f2", default="v2") %>%
    addFlag("--flag3", "-f3", default="v3")

cmdargs11 <- getTestInput("prog.R --flag1")
cmdargs12 <- getTestInput("prog.R --flag1 --flag2")
cmdargs13 <- getTestInput("prog.R --flag2 --flag1")

test_that("flags with default are properly consumed", {
          expect_identical(parseFlag(p3, cmdargs11), 
                           list(argv=list(`--flag1`="v1"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p3, cmdargs12), 
                           list(argv=list(`--flag1`="v1", `--flag2`="v2"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p3, cmdargs13), 
                           list(argv=list(`--flag1`="v1", `--flag2`="v2"), cmdargs_consumed="prog.R"))
})

cmdargs14 <- getTestInput("prog.R --flag1 o1")
cmdargs15 <- getTestInput("prog.R --flag1 --flag2 o2")
cmdargs16 <- getTestInput("prog.R --flag2 --flag1 o1")
cmdargs17 <- getTestInput("prog.R a --flag1 o1 b")

test_that("flags with default can be properly overwriten", {
          expect_identical(parseFlag(p3, cmdargs14), 
                           list(argv=list(`--flag1`="o1"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p3, cmdargs15), 
                           list(argv=list(`--flag1`="v1", `--flag2`="o2"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p3, cmdargs16), 
                           list(argv=list(`--flag1`="o1", `--flag2`="v2"), cmdargs_consumed="prog.R"))
          expect_identical(parseFlag(p3, cmdargs17), 
                           list(argv=list(`--flag1`="o1"), cmdargs_consumed=c("prog.R", 'a', 'b')))
})

p4 <- ArgParser() %>%
    addFlag("--flag1", optional=FALSE) %>%
    addFlag("--flag2", default="v2", optional=FALSE) %>%
    addFlag("--flag3", "-f3", default="v3", optional=FALSE) %>%
    addFlag("--flag4", "-f4")

cmdargs18 <- getTestInput("prog.R --flag1 v1")
cmdargs19 <- getTestInput("prog.R --flag1 v1 --flag2")
cmdargs19 <- getTestInput("prog.R --flag2 o2 --flag3")
cmdargs20 <- getTestInput("prog.R -f3")
cmdargs21 <- getTestInput("prog.R --flag1 v1 -f4 v4")
cmdargs22 <- getTestInput("prog.R --flag1 v1 --flag2 -f4 v4")
cmdargs23 <- getTestInput("prog.R --flag2 o2 --flag3 -f4 v4")
cmdargs24 <- getTestInput("prog.R -f3 -f4 v4")

test_that("forced flags must be present; otherwise error is thrown", {
          expect_error(parseFlag(p4, cmdargs18))
          expect_error(parseFlag(p4, cmdargs19))
          expect_error(parseFlag(p4, cmdargs20))
          expect_error(parseFlag(p4, cmdargs21))
          expect_error(parseFlag(p4, cmdargs22))
          expect_error(parseFlag(p4, cmdargs23))
          expect_error(parseFlag(p4, cmdargs24))
})

p5 <- ArgParser() %>%
    addFlag("--flag1", optional=FALSE) %>%
    addFlag("--flag2", "-f2", optional=FALSE) %>%
    addFlag("--flag3", default="v3", optional=FALSE) %>%
    addFlag("--flag4", "-f4", default="v4") %>%
    addFlag("--flag5")

cmdargs25 <- getTestInput("prog.R --flag1 v1 -f2 v2 --flag3 -f4 o4 --flag5 v5")

test_that("all components work together properly", {
          expect_identical(parseFlag(p5, cmdargs25), 
                           list(argv=list(`--flag1`="v1",
                                          `--flag2`="v2",
                                          `--flag5`="v5",
                                          `--flag3`="v3",
                                          `--flag4`="o4"), 
                                cmdargs_consumed="prog.R"))
})

test_that("limited_to subset of flags works properly", {
          expect_identical(parseFlag(p5, cmdargs25, limited_to=c("--flag1")), 
                           list(argv=list(`--flag1`="v1"), 
                                cmdargs_consumed=c("prog.R", "-f2", "v2", "--flag3", "-f4", "o4", "--flag5", "v5")))
          expect_identical(parseFlag(p5, cmdargs25, limited_to=c("--flag1", "--flag2")), 
                           list(argv=list(`--flag1`="v1", `--flag2`="v2"), 
                                cmdargs_consumed=c("prog.R", "--flag3", "-f4", "o4", "--flag5", "v5")))
          expect_identical(parseFlag(p5, cmdargs25, limited_to=c("--flag1", "--flag2", "--noflag")), 
                           list(argv=list(`--flag1`="v1", `--flag2`="v2"), 
                                cmdargs_consumed=c("prog.R", "--flag3", "-f4", "o4", "--flag5", "v5")))
})

