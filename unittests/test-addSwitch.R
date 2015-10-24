


context("Add switches onto ArgParser instance")

test_that("name definition of length > 1 will be warned, and the first one kept", {
          expect_warning(ArgParser() %>% addSwitch(c("--s1", "--s2")))
          expect_identical((ArgParser() %>% addSwitch(c("--s1", "--s2")))@switches_logic,
                           c(`--help`=FALSE, `--s1`=FALSE))
          expect_identical((ArgParser() %>% addSwitch(c("--s1", "--s2"), states=0:1L))@switches_any,
                           list(`--s1`=list(unpushed=0L, pushed=1L)))
})

test_that("help definition of length > 1 will be warned, and the first one kept", {
          expect_warning(ArgParser() %>% addSwitch("--s1", help=c("a", "b")))
          expect_identical((ArgParser() %>% addSwitch("--s1", help=c("a", "b")))@help["--s1"],
                           c(`--s1`="a"))
})

test_that("duplicated switch definition will cause error", {
          expect_error(ArgParser() %>% addSwitch("--s1") %>% addFlag("--s1"), 
                       regexp="^.*invalid class")
          expect_error(ArgParser() %>% addSwitch("--s1") %>% addFlag("--s2") %>% addFlag("--s1"),
                       regexp="^.*invalid class")
})

test_that("switch name without the -- prefix will casue error", {
          expect_error(ArgParser() %>% addSwitch("-s1"),
                       regexp="^.*invalid class")
          expect_error(ArgParser() %>% addSwitch("s1"),
                       regexp="^.*invalid class")
})

test_that("short alias is properly set, if any", {
          expect_identical((ArgParser() %>% addSwitch("--s1", "-s"))@switches_alias, 
                           c(`--help`="-h", `--s1`="-s"))
          expect_identical((ArgParser() %>% addSwitch("--s1") %>% addSwitch("--s2", "-s"))@switches_alias, 
                           c(`--help`="-h", `--s1`=NA, `--s2`="-s"))
})

test_that("duplicated alias definition will cause error", {
          expect_error(ArgParser() %>% addSwitch("--s1", "-s") %>% addSwitch("--s2", "-s"), 
                       regexp="^.*invalid class")
})

test_that("states default at FALSE", {
          expect_identical((ArgParser() %>% addSwitch("--s1"))@switches_logic, 
                           c(`--help`=FALSE, `--s1`=FALSE))
          })

test_that("states must be of type vector; otherwise throw error", {
          expect_error(ArgParser() %>% addSwitch("--s1", states=matrix(0,1,1))) 
          expect_error(ArgParser() %>% addSwitch("--s1", states=data.frame()))
})

test_that("logical states with length > 1 will cause warning", {
          expect_warning(ArgParser() %>% addSwitch("--s1", states=c(TRUE,FALSE)))
          expect_warning(ArgParser() %>% addSwitch("--s1", states=c(TRUE,FALSE,TRUE)))
})

test_that("non-logical states vector with length > 2 will cause warning", {
          expect_warning((ArgParser() %>% addSwitch("--s1", states=c(1:3L))))
          expect_warning((ArgParser() %>% addSwitch("--s1", states=list(T,F,F))))
})

test_that("non-logical states vector with length < 2 will cause error", {
          expect_error((ArgParser() %>% addSwitch("--s1", states=1)))
          expect_error((ArgParser() %>% addSwitch("--s1", states=list("unpushsed"))))
})

test_that("help, if any, is property set", {
          expect_identical((ArgParser() %>% addSwitch("--s1", help="s1 help"))@help["--s1"],
                           c(`--s1`="s1 help"))
          expect_identical((ArgParser() %>% addSwitch("--s1", help="s1 help") 
                                %>% addSwitch("--s2"))@help,
                           c(`--help`="show this message and exit", `--s1`="s1 help"))
})

test_that("all arguments work properly together", {
          p <- ArgParser() %>% addSwitch("--s1", "-s", list("unpushed", "pushed"), "help")
          expect_identical(p@switches_any, list(`--s1`=list(unpushed="unpushed", pushed="pushed")))
          expect_identical(p@switches_alias, c(`--help`="-h", `--s1`="-s"))
          expect_identical(p@help["--s1"], c(`--s1`="help"))
})



