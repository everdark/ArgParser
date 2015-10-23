# ArgParser
command line argument parser for R, implemented in S4

## Example Use
+ Declare the parser in `test.R` as the following:
```R
#!/usr/bin/env Rscript

library(methods)
library(magrittr)
source("ArgParser.R")
p <- ArgParser() %>% addFlag("--f1") %>%
    addFlag("--f2", optional=FALSE) %>%
    addSwitch("--s1", default=FALSE) %>%
    addSwitch("--s2", default=list(1,0))
pargs <- parseCommandLine(p, commandArgs())
for ( arg in names(pargs) )
    write(paste(arg, pargs[[arg]], '\t'), stdout())

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
