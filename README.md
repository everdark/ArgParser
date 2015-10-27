# ArgParser
Inspired by the module `argparse` in python, a command line argument parser for R, implemented in S4 without external dependency. Still in beta!

## Installation
```R
devtools::install_github("everdark/ArgParser")
```
Please be noticed that the repo is currently under development and hence is highly dynamic.

## Introduction
There are three types of command line arguments defined in `ArgParser`:
+ Flag argument
    + a key-value pair supplied in command line; key string must be prefixed with `--` (in full name) or `-` (in short alias)
    + in the form of `--f v` where `--f` is the flag name and `v` is the corresponding value
    + can be optional
    + can have a default value, such that only key is supplied in command line
+ Switch argument
    + like a flag without value; must be prefixed with `--` (in full name) or `-` (in short alias)
    + a states of unpushed/pushed values are pre-defined
    + states can be logical (by default) or any other types
    + A default `--help` (`-h`) switch is always available for printing help message
+ Positional (opt) argument
    + any string (after `--args` in the parsed result of `commandArgs`) left after consuming all flags and switches will be treated as positional argument
    + one opt can optionally consume more than one string

## Usage
### Flag
Use `addFlag` to define flag argument to your parser.

```R
#!/usr/bin/env Rscript
library(methods)
library(ArgParser)
library(magrittr)
p <- ArgParser(desc="a test for flags") %>% 
    addFlag("--flag1", "-f1", help="this is an optional flag") %>%
    addFlag("--flag2", "-f2", optional=F, help="this is a forced flag") %>%
    addFlag("--flag3", "-f3", default="v3", help="this is a flag with default value")
parseCommandLine(p)
```
Test the above script, assuming file name "test.R":
+ Show help message by supplying `-h` (e.g, `./test.R -h`)
```
Usage: test.R [--help] --flag2 [--flag1] [--flag3]

a test for flags

Logical switch:
  -h, --help    show this message and exit
Forced flag:
  -f2, --flag2  this is a forced flag
Optional flags:
  -f1, --flag1  this is an optional flag
  -f3, --flag3  this is a flag with default value
```
+ Parse command line strings
```
$ ./test.R -f1 v1 -f2 v2 -f3
```
shall give result as:
```
$`--flag1`
[1] "v1"

$`--flag2`
[1] "v2"

$`--flag3`
[1] "v3"

$`--help`
[1] FALSE
```

### Switch
Use `addSwitch` to define switch argument to your parser. The `states` argument should be of length 2 if not a logical vector, where the first element corresponds to the "unpushed" state (i.e., the switch is not present in the given command line string) and the second "pushed" state.

```R
#!/usr/bin/env Rscript
library(methods)
library(ArgParser)
library(magrittr)
p <- ArgParser(desc="a test for switches") %>% 
    addSwitch("--switch1", "-s1", help="this is a default logical switch") %>%
    addSwitch("--switch2", "-s2", states=list(0,1), help="this is an ad-hoc switch")
parseCommandLine(p)
```
Test the above script, assuming file name "test.R":
```
$ ./test.R -s2
```
shall give result as:
```
$`--help`
[1] FALSE

$`--switch1`
[1] FALSE

$`--switch2`
[1] 1
```

### Opt
Use `addOpt` to define positional argument to your parser.
```R
#!/usr/bin/env Rscript
library(methods)
library(ArgParser)
library(magrittr)
p <- ArgParser(desc="a test for opts") %>% 
    addFlag("--flag", "-f") %>%
    addSwitch("--switch", "-s") %>%
    addOpt("opt1") %>%
    addOpt("opt2", narg=3, nreq=1)
parseCommandLine(p)
```
Test the above script, assuming file name "test.R":
```
$ ./test.R -f vf abc -s def ghi
```
shall give result as:
```
Warning message:
In .local(x, name, ...) :
  Found nrequired < narg, then this opt MUST be the last opt defined so it will work.
$`--flag`
[1] "vf"

$`--help`
[1] FALSE

$`--switch`
[1] TRUE

$opt1
[1] "abc"

$opt2
[1] "def" "ghi" NA   
```
Notice that a warning is issued. The reason being clear in the message and the care is left for user to take.
