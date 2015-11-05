


context("Utility function test: argument length checker")

test_that("vector with length larger than maxlen is trimmed with a warning", {
          expect_warning(.checkArgLen(1:2L, 1), "Argument.*has length >.*; only the first.*respected.")
          expect_identical(.checkArgLen(1:2L, 1), 1L)
          expect_warning(.checkArgLen(1:3L, 2), "Argument.*has length >.*; only the first.*respected.")
          expect_identical(.checkArgLen(1:3L, 2), 1:2L)
          expect_warning(.checkArgLen(1:4L, 3), "Argument.*has length >.*; only the first.*respected.")
          expect_identical(.checkArgLen(1:4L, 3), 1:3L)
          expect_warning(.checkArgLen(letters, 1), "Argument.*has length >.*; only the first.*respected.")
          expect_identical(.checkArgLen(letters, 1), 'a')
})

test_that("maxlen < 1 will cause error", {
          expect_error(.checkArgLen(1, 0), "maxlen >= 1 must hold")
          expect_error(.checkArgLen(1, -1), "maxlen >= 1 must hold")
})
