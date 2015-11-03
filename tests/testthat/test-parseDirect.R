
context("Parse directive and its sub-commands in command line argument string")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

p1 <- ArgParser() %>%
    addDirect("run")

cmdargs1 <- getTestInput("prog.R run")
cmdargs2 <- getTestInput("prog.R a run b")

test_that("single directive is properly consumed", {
          expect_identical(parseDirect(p1, cmdargs1), 
                           list(argv=list(run=list()), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p1, cmdargs2), 
                           list(argv=list(run=list()), cmdargs_consumed=c("prog.R", 'a', 'b')))
})

p2 <- ArgParser() %>%
    addDirect("run") %>%
    addFlag("--f1", "-f1", dir="run") %>%
    addFlag("--f2", "-f2", optional=FALSE, dir="run")

cmdargs3 <- getTestInput("prog.R run --f2 v2")
cmdargs4 <- getTestInput("prog.R run -f1 v1 --f2 v2")
cmdargs5 <- getTestInput("prog.R a run -f1 v1 b --f2 v2 c")
cmdargs6 <- getTestInput("prog.R --f1 v1 run -f2 v2")
cmdargs7 <- getTestInput("prog.R")
cmdargs8 <- getTestInput("prog.R -f1 v1")
cmdargs9 <- getTestInput("prog.R -f2")

test_that("sub-commands (flags) are properly consumed", {
          expect_identical(parseDirect(p2, cmdargs3), 
                           list(argv=list(run=list(`--f2`="v2")), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p2, cmdargs4), 
                           list(argv=list(run=list(`--f1`="v1", `--f2`="v2")), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p2, cmdargs5), 
                           list(argv=list(run=list(`--f1`="v1", `--f2`="v2")), cmdargs_consumed=c("prog.R", 'a', 'b', 'c')))
          expect_identical(parseDirect(p2, cmdargs6), 
                           list(argv=list(run=list(`--f2`="v2")), cmdargs_consumed=c("prog.R", "--f1", "v1")))
          expect_error(parseDirect(p2, cmdargs7))
          expect_error(parseDirect(p2, cmdargs8))
          expect_error(parseDirect(p2, cmdargs9))
})

p3 <- ArgParser() %>%
    addDirect("run") %>%
    addSwitch("--s1", dir="run") %>%
    addSwitch("--s2", states=0:1L, dir="run")

cmdargs10 <- getTestInput("prog.R run --s1")
cmdargs11 <- getTestInput("prog.R run --s1 --s2")
cmdargs12 <- getTestInput("prog.R --s1 run --s2")
cmdargs13 <- getTestInput("prog.R a run --s2 b --s1 c")

test_that("sub-commands (switches) are properly consumed", {
          expect_identical(parseDirect(p3, cmdargs10), 
                           list(argv=list(run=list(`--s1`=T, `--s2`=0L)), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p3, cmdargs11), 
                           list(argv=list(run=list(`--s1`=T, `--s2`=1L)), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p3, cmdargs12), 
                           list(argv=list(run=list(`--s1`=F, `--s2`=1L)), cmdargs_consumed=c("prog.R", "--s1")))
          expect_identical(parseDirect(p3, cmdargs13), 
                           list(argv=list(run=list(`--s1`=T, `--s2`=1L)), cmdargs_consumed=c("prog.R", 'a', 'b', 'c')))
})


p4 <- ArgParser() %>%
    addDirect("run") %>% 
    addOpt("opt1", dir="run") %>% 
    addOpt("opt2", dir="run")

cmdargs14 <- getTestInput("prog.R run a b")
cmdargs15 <- getTestInput("prog.R run a b c")
cmdargs16 <- getTestInput("prog.R a run b c")
cmdargs17 <- getTestInput("prog.R run a")

test_that("sub-commands (opt) are properly consumed",{
          expect_identical(parseDirect(p4, cmdargs14), 
                           list(argv=list(run=list(opt1='a', opt2='b')), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p4, cmdargs15), 
                           list(argv=list(run=list(opt1='a', opt2='b')), cmdargs_consumed=c("prog.R", 'c')))
          expect_identical(parseDirect(p4, cmdargs16), 
                           list(argv=list(run=list(opt1='b', opt2='c')), cmdargs_consumed=c("prog.R", 'a')))
          expect_error(parseDirect(p4, cmdargs17)) 

})

p5 <- ArgParser() %>%
    addDirect("run") %>%
    addFlag("--f", dir="run") %>%
    addSwitch("--s", dir="run") %>%
    addOpt("opt", dir="run")

cmdargs18 <- getTestInput("prog.R run --f v1 a")
cmdargs19 <- getTestInput("prog.R run --s a")
cmdargs20 <- getTestInput("prog.R run a")
cmdargs21 <- getTestInput("prog.R run --f v1 --s a")
cmdargs22 <- getTestInput("prog.R a run --f v1 --s b")
cmdargs23 <- getTestInput("prog.R run --f v1")
cmdargs24 <- getTestInput("prog.R run --s")

test_that("sub-commands all together are properly consumed",{
          expect_identical(parseDirect(p5, cmdargs18), 
                           list(argv=list(run=list(`--f`="v1", `--s`=F, opt='a')), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p5, cmdargs19), 
                           list(argv=list(run=list(`--s`=T, opt='a')), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p5, cmdargs20), 
                           list(argv=list(run=list(`--s`=F, opt='a')), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p5, cmdargs21), 
                           list(argv=list(run=list(`--f`="v1", `--s`=T, opt='a')), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p5, cmdargs22), 
                           list(argv=list(run=list(`--f`="v1", `--s`=T, opt='b')), cmdargs_consumed=c("prog.R", 'a')))
          expect_error(parseDirect(p5, cmdargs23)) 
          expect_error(parseDirect(p5, cmdargs24)) 
})

p6 <- ArgParser() %>% addDirect("run")

cmdargs25 <- getTestInput("prog.R")

test_that("forced directive can not be missing", {
          expect_error(parseDirect(p6, cmdargs25))
})

cmdargs26 <- getTestInput("prog.R run run")

test_that("duplicated directive names given will cause error", {
          expect_error(parseDirect(p6, cmdargs26))
})

test_that("null parser works properly", {
          expect_identical(parseDirect(ArgParser(), cmdargs25),
                           list(argv=list(), cmdargs_consumed="prog.R"))
})

p7 <- ArgParser() %>% 
    addDirect(c("start", "stop")) %>% 
    addFlag("--f1", dir="start") %>%
    addFlag("--f2", dir="stop") %>%
    addFlag("--f3", dir=c("start", "stop"))

cmdargs27 <- getTestInput("prog.R start --f1 v1")
cmdargs28 <- getTestInput("prog.R start --f2 v2")
cmdargs29 <- getTestInput("prog.R start --f3 v3")
cmdargs30 <- getTestInput("prog.R stop --f1 v1")
cmdargs31 <- getTestInput("prog.R stop --f2 v2")
cmdargs32 <- getTestInput("prog.R stop --f3 v3")
cmdargs33 <- getTestInput("prog.R start --f1 v1 --f2 v2 --f3 v3")

test_that("directive set works properly", {
          expect_identical(parseDirect(p7, cmdargs27),
                           list(argv=list(start=list(`--f1`="v1")), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p7, cmdargs28),
                           list(argv=list(start=list()), cmdargs_consumed=c("prog.R", "--f2", "v2")))
          expect_identical(parseDirect(p7, cmdargs29),
                           list(argv=list(start=list(`--f3`="v3")), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p7, cmdargs30),
                           list(argv=list(stop=list()), cmdargs_consumed=c("prog.R", "--f1", "v1")))
          expect_identical(parseDirect(p7, cmdargs31),
                           list(argv=list(stop=list(`--f2`="v2")), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p7, cmdargs32),
                           list(argv=list(stop=list(`--f3`="v3")), cmdargs_consumed="prog.R"))
          expect_identical(parseDirect(p7, cmdargs33),
                           list(argv=list(start=list(`--f1`="v1", `--f3`="v3")), cmdargs_consumed=c("prog.R", "--f2", "v2")))
})

cmdargs34 <- getTestInput("prog.R start stop")

test_that("directives of the same group can not co-exist", {
          expect_error(parseDirect(p7, cmdargs34))
})



