


context("Add flags onto ArgParser instance")

getTestInput <- function(x) strsplit(x, ' ')[[1]]

test_that("Duplicated flag definition will cause error", {
          expect_error(ArgParser() %>% addFlag("--f1") %>% addFlag("--f1"))
})

