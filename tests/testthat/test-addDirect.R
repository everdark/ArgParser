


context("Add directives onto ArgParser instance")

test_that("name definition of any length can be properly set", {
          expect_identical((ArgParser() %>% addDirect(c("dir1")))@directs,
                           list(dir1=NULL))
          expect_identical((ArgParser() %>% addDirect(c("dir1", "dir2")))@directs,
                           list(dir1=NULL, dir2=NULL))
})

test_that("duplicated directive name definition will cause error", {
          expect_error(ArgParser() %>% addDirect(c("dir1", "dir1")), 
                       regexp="^.*invalid class")
          expect_error(ArgParser() %>% addDirect("dir1") %>% addDirect("dir1"),
                       regexp="^.*invalid class")
})

test_that("duplicated directive group name definition will cause error", {
          expect_error(ArgParser() %>% addDirect("dir1", "g1") %>% addDirect("dir2", "g1"),
                       regexp="^.*invalid class")
})

test_that("optional, if any, is properly set", {
          expect_identical((ArgParser() %>% addDirect("dir"))@directs_group$`_g1`$is_optional, 
                           c(FALSE))
          #expect_identical((ArgParser() %>% addDirect("dir", optional=TRUE))@directs_group$`_g1`$is_optional, 
          #                 c(TRUE))
          #expect_identical((ArgParser() %>% addDirect("dir1") %>% addDirect("dir2", optional=T))@directs_group, 
          #                 list(`_g1`=list(mem="dir1", is_optional=F), `_g2`=list(mem="dir2", is_optional=T)))
          #expect_identical((ArgParser() %>% addDirect("dir1", "g1") %>% addDirect("dir2", optional=T))@directs_group, 
          #                 list(g1=list(mem="dir1", is_optional=F), `_g2`=list(mem="dir2", is_optional=T)))
          #expect_identical((ArgParser() %>% addDirect("dir1", "g1") %>% addDirect("dir2", "g2", optional=T))@directs_group, 
          #                 list(g1=list(mem="dir1", is_optional=F), g2=list(mem="dir2", is_optional=T)))
})

test_that("help, if any, is property set", {
          expect_identical((ArgParser() %>% addDirect("dir", help="dir help"))@help["dir"],
                           c(dir="dir help"))
          expect_identical((ArgParser() %>% addDirect(c("dir1", "dir2"), help=c("dir1 help", "dir2 help")))@help, 
                           c(`--help`="show this message and exit", dir1="dir1 help", dir2="dir2 help"))
          expect_identical((ArgParser() %>% addDirect(c("dir1", "dir2"), help=c("dir1 help")))@help, 
                           c(`--help`="show this message and exit", dir1="dir1 help", dir2=NA))
          #expect_identical((ArgParser() %>% addDirect("dir1", help="dir1 help") %>% addDirect("dir2", help="dir2 help"))@help, 
          #                 c(`--help`="show this message and exit", dir1="dir1 help", dir2="dir2 help"))
})

test_that("help definition of length > length(name) will be warned, and the first length(name) kept", {
          expect_warning(ArgParser() %>% addDirect("dir", help=c('a', 'b')))
          expect_identical((ArgParser() %>% addDirect("dir", help=c('a', 'b')))@help["dir"],
                           c(dir='a'))
          expect_warning(ArgParser() %>% addDirect(c("dir1", "dir2"), help=c('a', 'b', 'c')))
          expect_identical((ArgParser() %>% addDirect(c("dir1", "dir2"), help=c('a', 'b', 'c')))@help,
                           c(`--help`="show this message and exit", dir1='a', dir2='b'))

})

test_that("all arguments work properly together", {
          p <- ArgParser() %>% addDirect(c("dir1", "dir2"), "dir", F, c("dir1 help", "dir2 help"))
          expect_identical(p@directs, list(dir1=NULL, dir2=NULL))
          expect_identical(p@directs_group, list(dir=list(mem=c("dir1", "dir2"), is_optional=F)))
          expect_identical(p@help["dir1"], c(dir1="dir1 help"))
          expect_identical(p@help["dir2"], c(dir2="dir2 help"))
})

test_that("add no-defined directive will cause error", {
          expect_error(ArgParser() %>% addFlag("--f", directive="dir"), "^.*must be defined first.*")
          expect_error(ArgParser() %>% addSwitch("--s", directive="dir"), "^.*must be defined first.*")
          expect_error(ArgParser() %>% addOpt("opt", directive="dir"), "^.*must be defined first.*")
          expect_error(ArgParser() %>% addOpt("opt", directive="dir") %>% addDirect("dir"), "^.*must be defined first.*")
          expect_error(ArgParser() %>% addDirect("dir") %>% addOpt("opt", directive=c("dir", "nodir")), "^.*must be defined first.*")
})

