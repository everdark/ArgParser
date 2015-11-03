
context("Parse switches in command line argument string")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

p1 <- ArgParser() %>%
    addSwitch("--s1") %>%
    addSwitch("--s2")

cmdargs1 <- getTestInput("prog.R --s1")
cmdargs2 <- getTestInput("prog.R --s1 --s2")

test_that("switches without given states are properly consumed", {
          expect_identical(parseSwitch(p1, cmdargs1), 
                           list(argv=list(`--help`=FALSE, `--s1`=TRUE, `--s2`=FALSE), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p1, cmdargs2), 
                           list(argv=list(`--help`=FALSE, `--s1`=TRUE, `--s2`=TRUE), cmdargs_consumed="prog.R"))
})

cmdargs2_1 <- getTestInput("prog.R a b c")

test_that("command line string without any switch can be properly by-passed", {
          expect_identical(parseSwitch(p1, cmdargs2_1),
                           list(argv=list(`--help`=FALSE, `--s1`=FALSE, `--s2`=FALSE), cmdargs_consumed=c("prog.R", 'a', 'b', 'c')))
})

p2 <- ArgParser() %>%
    addSwitch("--switch1") %>%
    addSwitch("--switch2", "-s2") %>%
    addSwitch("--switch3", "-s3")

cmdargs3 <- getTestInput("prog.R --switch1")
cmdargs4 <- getTestInput("prog.R --switch1 --switch2")
cmdargs5 <- getTestInput("prog.R a --switch1 b -s2 c")
cmdargs6 <- getTestInput("prog.R -s3 --switch2")
cmdargs7 <- getTestInput("prog.R --switch1 -s2 -s3")

test_that("switchess with alias and without given states are properly consumed", {
          expect_identical(parseSwitch(p2, cmdargs3), 
                           list(argv=list(`--help`=F, `--switch1`=T, `--switch2`=F, `--switch3`=F), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p2, cmdargs4), 
                           list(argv=list(`--help`=F, `--switch1`=T, `--switch2`=T, `--switch3`=F), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p2, cmdargs5), 
                           list(argv=list(`--help`=F, `--switch1`=T, `--switch2`=T, `--switch3`=F), cmdargs_consumed=c("prog.R", 'a', 'b', 'c')))
          expect_identical(parseSwitch(p2, cmdargs6), 
                           list(argv=list(`--help`=F, `--switch1`=F, `--switch2`=T, `--switch3`=T), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p2, cmdargs7), 
                           list(argv=list(`--help`=F, `--switch1`=T, `--switch2`=T, `--switch3`=T), cmdargs_consumed="prog.R"))
})

cmdargs8 <- getTestInput("prog.R --switch1 --switch1")
cmdargs9 <- getTestInput("prog.R -s2 -s2")
cmdargs10 <- getTestInput("prog.R --switch1 -s3 -s3")

test_that("duplicated switch names and/or alias cause error", {
          expect_error(parseSwitch(p2, cmdargs8))
          expect_error(parseSwitch(p2, cmdargs9))
          expect_error(parseSwitch(p2, cmdargs10))
})

p3 <- ArgParser() %>%
    addSwitch("--switch1", states=TRUE) %>%
    addSwitch("--switch2", "-s2", states=list(1,2)) %>%
    addSwitch("--switch3", "-s3", states=list("no", "yes"))

cmdargs11 <- getTestInput("prog.R --switch1")
cmdargs12 <- getTestInput("prog.R --switch1 --switch2")
cmdargs13 <- getTestInput("prog.R -s2 -s3")

test_that("switches with given states are properly consumed", {
          expect_identical(parseSwitch(p3, cmdargs11), 
                           list(argv=list(`--help`=F, `--switch1`=F, `--switch2`=1, `--switch3`="no"), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p3, cmdargs12), 
                           list(argv=list(`--help`=F, `--switch1`=F, `--switch2`=2, `--switch3`="no"), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p3, cmdargs13), 
                           list(argv=list(`--help`=F, `--switch1`=T, `--switch2`=2, `--switch3`="yes"), cmdargs_consumed="prog.R"))
})

p4 <- ArgParser() %>%
    addSwitch("--s1", states=list(1L, "pushed!"))

cmdargs14 <- getTestInput("prog.R --s1")
cmdargs15 <- getTestInput("prog.R")

test_that("states can be multi-typed", {
          expect_identical(parseSwitch(p4, cmdargs14), 
                           list(argv=list(`--help`=FALSE, `--s1`="pushed!"), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p4, cmdargs15), 
                           list(argv=list(`--help`=FALSE, `--s1`=1L), cmdargs_consumed="prog.R"))
})

p5 <- ArgParser() %>%
    addSwitch("--s1") %>%
    addSwitch("--s2")

cmdargs16 <- getTestInput("prog.R --s1 --s2")

test_that("limited_to subset of switches works properly", {
          expect_identical(parseSwitch(p5, cmdargs16, "--s1"), 
                           list(argv=list(`--s1`=T), cmdargs_consumed=c("prog.R", "--s2")))
          expect_identical(parseSwitch(p5, cmdargs16, c("--s1", "--s2")), 
                           list(argv=list(`--s1`=T, `--s2`=T), cmdargs_consumed="prog.R"))
          expect_identical(parseSwitch(p5, cmdargs16, c("--s1", "--s2", "--noswitch")), 
                           list(argv=list(`--s1`=T, `--s2`=T), cmdargs_consumed="prog.R"))
})

cmdargs17 <- getTestInput("prog.R abc")

test_that("null parser works properly", {
          expect_identical(parseSwitch(ArgParser(), cmdargs17),
                           list(argv=list(`--help`=F), cmdargs_consumed=c("prog.R", "abc")))
})

