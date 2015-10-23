


context("Instantiate ArgParser object")

test_that("program name, if any, is properly set", {
          expect_identical(ArgParser(prog="test.R")@prog, "test.R")
})

test_that("program name with length > 1 will be warned", {
          expect_warning(ArgParser(prog=c("test1.R", "test2.R")))
})

test_that("description, if any, is porperly set", {
          expect_identical(ArgParser(desc="test")@desc, "test")
})

test_that("dsecription of lentgh > 1 is allowed", {
          expect_identical(ArgParser(desc=letters)@desc, letters)
})
