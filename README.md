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
$ ./test.R --f1 v1 --f2 v2 --s1
--f1 v1 
--f2 v2 
--help FALSE 
--s1 TRUE 
```
