


context("Add positional args onto ArgParser instance")

test_that("name definition of length > 1 will be warned, and the first one kept", {
          expect_warning(ArgParser() %>% addOpt(c("opt1", "opt2")))
          expect_warning(ArgParser() %>% addOpt(c("opt1", "opt1")))
          expect_identical((ArgParser() %>% addOpt("opt", help=c("a", "b")))@help["opt"], 
                           c(opt="a"))
})

test_that("duplicated opt definition will cause error", {
          expect_error(ArgParser() %>% addOpt("opt") %>% addOpt("opt"), 
                       regexp="^.*invalid class")
          expect_error(ArgParser() %>% addOpt("opt1") %>% addOpt("opt2") %>% addFlag("opt1"),
                       regexp="^.*invalid class")
})

test_that("nrequired > narg will cause error", {
          expect_error(ArgParser() %>% addOpt("opt", narg=1, nreq=2))
          expect_error(ArgParser() %>% addOpt("opt", narg=2, nreq=3))
})

test_that("nrequired < narg will be warned", {
          expect_warning(ArgParser() %>% addOpt("opt", narg=2, nreq=1))
          expect_warning(ArgParser() %>% addOpt("opt", narg=3, nreq=2))
})

test_that("narg and nrequired are properly set", {
          p1 <- ArgParser() %>% addOpt("opt", narg=2)
          expect_identical(p1@opt, "opt")
          expect_identical(p1@opt_narg,c(opt=2L))
          expect_identical(p1@opt_nrequired, c(opt=2L))
          p2 <- ArgParser() %>% addOpt("opt", narg=2, nreq=1)
          expect_identical(p2@opt, "opt")
          expect_identical(p2@opt_narg,c(opt=2L))
          expect_identical(p2@opt_nrequired, c(opt=1L))
})

test_that("narg or nrequired with length > 1 will be warned", {
          expect_warning(ArgParser() %>% addOpt("opt", narg=1:2))
          expect_warning(ArgParser() %>% addOpt("opt", narg=1, nreq=1:2))
})

test_that("help, if any, is property set", {
          expect_identical((ArgParser() %>% addSwitch("--s1", help="s1 help"))@help["--s1"],
                           c(`--s1`="s1 help"))
          expect_identical((ArgParser() %>% addSwitch("--s1", help="s1 help") 
                                %>% addSwitch("--s2"))@help,
                           c(`--help`="show this message and exit", `--s1`="s1 help"))
})

test_that("help definition of length > 1 will be warned, and the first one kept", {
          expect_warning(ArgParser() %>% addOpt("opt", help=c("a", "b")))
          expect_identical((ArgParser() %>% addOpt("opt", help=c("a", "b")))@help["opt"],
                           c(opt="a"))
})

test_that("all arguments work properly together", {
          p <- ArgParser() %>% addOpt("opt", "help", narg=2, nreq=1)
          expect_identical(p@opt, "opt")
          expect_identical(p@opt_narg, c(opt=2L))
          expect_identical(p@opt_nrequired, c(opt=1L))
          expect_identical(p@help["opt"], c(opt="help"))
})



