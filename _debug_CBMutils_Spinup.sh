#!/bin/bash

## ---------------------------------------------------------------------------------------------- ##
## based on: <https://blog.davisvaughan.com/posts/2019-04-05-debug-r-package-with-cpp/>            ##
## ---------------------------------------------------------------------------------------------- ##

cd ~/GitHub/spadesCBM

R -d lldb

#breakpoint set -E c++
#breakpoint set --name Spinup
breakpoint set --name StepPoolsRef
breakpoint set -p BREAK
breakpoint modify -c 'row == 537 && col == 0'

breakpoint list
#breakpoint delete

run

## in the R session:
.debug <- TRUE
source("new_libCBMGlobal.R")

## type 'next' to step through line by line
## type 'frame variable' to show what variables are available
## type 'expr varName' to see the current value of varName
