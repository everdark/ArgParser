


context("Utility function test: check for duplicated argument names")

test_that("duplicated elements will cause error", {
          expect_error(.checkDupArg(c('a','b','b'), letters), "Some args are duplicated")
})

test_that("non-duplicated will cause nothing to happen", {
          expect_output(.checkDupArg('a', 'a'), '')
})

test_that("element not recognized will cause nothing to happen even a duplicated", {
          expect_output(.checkDupArg('a', 'b'), '')
          expect_output(.checkDupArg(c('a', 'a'), 'b'), '')
})
