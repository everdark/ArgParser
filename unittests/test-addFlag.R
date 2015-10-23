


context("Add flags onto ArgParser instance")

test_that("duplicated flag definition will cause error", {
          expect_error(ArgParser() %>% addFlag("--f1") %>% addFlag("--f1"), 
                       regexp="^.*invalid class")
          expect_error(ArgParser() %>% addFlag("--f1") %>% addFlag("--f2") %>% addFlag("--f1"),
                       regexp="^.*invalid class")
})

test_that("flag name without the -- prefix will casue error", {
          expect_error(ArgParser() %>% addFlag("-f1"),
                       regexp="^.*invalid class")
          expect_error(ArgParser() %>% addFlag("f1"),
                       regexp="^.*invalid class")
})

test_that("short alias is properly set, if any", {
          expect_identical((ArgParser() %>% addFlag("--f1", "-f"))@flags_alias, 
                           c(`--f1`="-f"))
          expect_identical((ArgParser() %>% addFlag("--f1") %>% addFlag("--f2", "-f"))@flags_alias, 
                           c(`--f1`=NA, `--f2`="-f"))
})

test_that("duplicated alias definition will cause error", {
          expect_error(ArgParser() %>% addFlag("--f1", "-f") %>% addFlag("--f2", "-f"), 
                       regexp="^.*invalid class")
})

test_that("optional, if any, is properly set", {
          expect_identical((ArgParser() %>% addFlag("--f1", optional=TRUE))@flags_isOptional, 
                           c(`--f1`=TRUE))
          expect_identical((ArgParser() %>% addFlag("--f1", optional=FALSE))@flags_isOptional, 
                           c(`--f1`=FALSE))
          expect_identical((ArgParser() %>% addFlag("--f1", optional=FALSE) %>% 
                                addFlag("--f2", optional=FALSE))@flags_isOptional, 
                           c(`--f1`=FALSE, `--f2`=FALSE))
          expect_identical((ArgParser() %>% addFlag("--f1", optional=TRUE) %>% 
                                addFlag("--f2", optional=FALSE))@flags_isOptional, 
                           c(`--f1`=TRUE, `--f2`=FALSE))
})

test_that("default, if any, is properly set", {
          expect_identical((ArgParser() %>% addFlag("--f1"))@flags, 
                           list(`--f1`=NA))
          expect_identical((ArgParser() %>% addFlag("--f1", default=TRUE))@flags, 
                           list(`--f1`=TRUE))
})

test_that("data for default can be multi-typed", {
          expect_identical((ArgParser() %>% addFlag("--f0") %>%
                                addFlag("--f1", default=TRUE) %>%
                                addFlag("--f2", default=1) %>%
                                addFlag("--f3", default="a"))@flags, 
                           list(`--f0`=NA, `--f1`=TRUE, `--f2`=1, `--f3`="a"))
})

test_that("data for default more than one will be trimmed with warning", {
          expect_warning((ArgParser() %>% addFlag("--f1", default=c(1:2L))))
          expect_warning((ArgParser() %>% addFlag("--f1", default=list(T, F))))
          expect_identical((ArgParser() %>% addFlag("--f1", default=c(1:2L)))@flags, list(`--f1`=1L))
})

test_that("help, if any, is property set", {
          expect_identical((ArgParser() %>% addFlag("--f1", help="f1 help"))@help["--f1"],
                           c(`--f1`="f1 help"))
          expect_identical((ArgParser() %>% addFlag("--f1", help="f1 help") 
                                %>% addFlag("--f2"))@help,
                           c(`--help`="show this message and exit", `--f1`="f1 help"))
})

test_that("all arguments work properly together", {
          p <- ArgParser() %>% addFlag("--f1", "-f", "default", T, "help")
          expect_identical(p@flags, list(`--f1`="default"))
          expect_identical(p@flags_alias, c(`--f1`="-f"))
          expect_identical(p@flags_isOptional, c(`--f1`=T))
          expect_identical(p@help["--f1"], c(`--f1`="help"))
})



