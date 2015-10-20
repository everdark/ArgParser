


context("Add flags onto ArgParser instance")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

test_that("duplicated flag definition will cause error", {
          expect_error(ArgParser() %>% addFlag("--f1") %>% addFlag("--f1"))
})

test_that("flag name without the -- prefix will casue error", {
          expect_error(ArgParser() %>% addFlag("-f1"))
          expect_error(ArgParser() %>% addFlag("f1"))
})

test_that("the argument optional is properly set", {
          expect_identical((ArgParser() %>% addFlag("--f1", optional=TRUE))@flags_isOptional, c(`--f1`=TRUE))
          expect_identical((ArgParser() %>% addFlag("--f1", optional=FALSE))@flags_isOptional, c(`--f1`=FALSE))
          expect_identical((ArgParser() %>% addFlag("--f1", optional=FALSE) %>% 
                            addFlag("--f2", optional=FALSE))@flags_isOptional, c(`--f1`=FALSE, `--f2`=FALSE))
})

test_that("the argument default is properly set", {
          expect_identical((ArgParser() %>% addFlag("--f1"))@flags, list(`--f1`=NA))
          expect_identical((ArgParser() %>% addFlag("--f1", default=TRUE))@flags, list(`--f1`=TRUE))
})

test_that("data for default can be multi-typed", {
          expect_identical((ArgParser() %>% addFlag("--f0") %>%
                            addFlag("--f1", default=TRUE) %>%
                            addFlag("--f2", default=1) %>%
                            addFlag("--f3", default="a"))@flags, list(`--f0`=NA, `--f1`=TRUE, `--f2`=1, `--f3`="a"))
})

test_that("data for default more than one will be trimmed with warning", {
          expect_warning((ArgParser() %>% addFlag("--f1", default=c(1:2L))))
          expect_identical((ArgParser() %>% addFlag("--f1", default=c(1:2L)))@flags, list(`--f1`=1L))
})
