
context("Parse opts in command line argument string")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

p1 <- ArgParser() %>%
    addOpt("opt1") %>%
    addOpt("opt2")

cmdargs1 <- getTestInput("a b")
cmdargs2 <- getTestInput("a b c")

test_that("opts are properly consumed", {
          expect_identical(parseOpt(p1, cmdargs1), 
                           list(argv=list(opt1="a", opt2="b"), cmdargs_consumed=character(0)))
          expect_identical(parseOpt(p1, cmdargs2), 
                           list(argv=list(opt1="a", opt2="b"), cmdargs_consumed='c'))
})

suppressWarnings(
p2 <- ArgParser() %>%
    addOpt("opt1") %>%
    addOpt("opt2", narg=2, nreq=2) %>%
    addOpt("opt3", narg=2, nreq=1)
)

cmdargs3 <- getTestInput("a b c d e")
cmdargs4 <- getTestInput("a b c d")

test_that("opts with varying numbers are properly consumed", {
          expect_identical(parseOpt(p2, cmdargs3), 
                           list(argv=list(opt1='a', opt2=c('b', 'c'), opt3=c('d', 'e')), cmdargs_consumed=character(0)))
          expect_identical(parseOpt(p2, cmdargs4), 
                           list(argv=list(opt1='a', opt2=c('b', 'c'), opt3=c('d', NA)), cmdargs_consumed=character(0)))
})

cmdargs5 <- getTestInput("a")
cmdargs6 <- getTestInput("a b")
cmdargs7 <- getTestInput("a b c")

test_that("insufficient opts cause errors", {
          expect_error(parseOpt(p2, cmdargs5), "^.*less than required.*")
          expect_error(parseOpt(p2, cmdargs6), "^.*less than required.*")
          expect_error(parseOpt(p2, cmdargs7), "^.*less than required.*")
})



