# ArgParser
command line argument parser for R, implemented in S4

## Example Use
+ Declare the parser in `test.R` as the following:
```R
#!/usr/bin/env Rscript

library(methods)
library(magrittr)
source("ArgParser.R")
p <- ArgParser(desc="A test for ArgParser") %>% 
    addFlag("--flag1", "-f1", help="this is an optional flag") %>%
    addFlag("--flag2", help="this is an optional flag with default value", default="f1d") %>%
    addFlag("--flag3", "-f3", help="this is a forced flag", optional=FALSE) %>%
    addSwitch("--logical-switch", "-s1", help="a logical switch", states=FALSE) %>%
    addSwitch("--adhoc-switch", "-s2", help="an ad-hoc switch", states=list(1,0)) %>%
    addOpt("opt1", help="positional arg 1") %>%
    addOpt("opt2", narg=2, nrequired=1, help="positional arg 2")
pargs <- parseCommandLine(p, commandArgs(), trim=T)

writeLines('')
writeLines("Original command line string:")
writeLines("-----------------------------")
commandArgs()
writeLines('')
writeLines("Parsed result:")
writeLines("--------------")
pargs
```
+ Then run:
```
$ ./test.R -f3 v3 opt1 --flag2 opt2 -s1
Warning message:
In .local(x, name, ...) :
  Found nrequired < narg, then this opt MUST be the last opt defined so it will work.

Original command line string:
-----------------------------
 [1] "/Library/Frameworks/R.framework/Resources/bin/exec/R"
 [2] "--slave"
 [3] "--no-restore"
 [4] "--file=./test.R"
 [5] "--args"
 [6] "-f3"
 [7] "v3"
 [8] "opt1"
 [9] "--flag2"
[10] "opt2"
[11] "-s1"

Parsed result:
--------------
$flag3
[1] "v3"

$flag2
[1] "opt2"

$help
[1] FALSE

$`logical-switch`
[1] TRUE

$`adhoc-switch`
[1] 1

$opt1
[1] "opt1"

$opt2
[1] "opt2" NA
```
