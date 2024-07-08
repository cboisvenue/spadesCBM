## this is a place to save all the Python related info
#install_libcbm(method = "virutalenv")
# Error in basename(condaenv) : a character vector argument expected
# In addition: Warning message:
#   In any(cfg$anaconda, cfg$conda) :
#   coercing argument of type 'character' to logical
###This is another error I get while running the line above:
# Error in basename(condaenv) : a character vector argument expected
# In addition: Warning message:
#   In any(cfg$anaconda, cfg$conda) :
#   coercing argument of type 'character' to logical

##TODO do we need to install libcbm? (this is a python package) doesn't having
##libcbmr suffice? don't we have to do it like this?
# library(reticulate)
# reticulate::use_virtualenv(virtualenv = "r-reticulate")
###CELINE NOTES: the following line takes a long time...do we need it?? or is it
###a check to make sure things are as they should be?
#reticulate::import("sys")$executable
#[1] "C:\\Users\\cboisven\\AppData\\Local\\R-MINI~1\\envs\\R-RETI~1\\python.exe"
###CELINE NOTES: this next line was to revert to an older version of numpy
###Python package. The June 16th update to version 2.0 created some issues.
###Scott had the same error, error pasted here.
# source("C:/Users/smorken/dev/code/spades/libcbmr/tests/spatial_integration_test/run_spatial_test.R") Show Traceback Rerun with DebugError in py_call_impl(callable, call_args$unnamed, call_args$named) :
#   ValueError: If using all scalar values, you must pass an indexRun `reticulate::py_last_error()` for details.
# reticulate::py_install("numpy<2.0", envname = "r-reticulate", ignore_installed=TRUE)
#libcbm <- reticulate::import("libcbm")

### other possible ways if the above stopped working, ran this instead:

#print(reticulate::py_get_attr(libcbm, "__version__"))
#install the latest numpy < 2.0
#
# reticulate::use_virtualenv(virtualenv = "r-reticulate")
# reticulate::py_install("numpy==1.26.4", envname = "r-reticulate")
# reticulate::py_install("pandas==2.2.2", envname = "r-reticulate")
#
# #put this last: because when you install libcbm, it will automatically install all of its dependencies, potentially overwriting your custom-selected pacakge versions
# reticulate::py_install("libcbm", envname = "r-reticulate")
##check things with
# py_config() ## this is confusing!!

##Lines of code from Scott Morken to remove all packages and reload them. Run
##this from command line
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall libcbm
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall numpy
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall pandas
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall scipy
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall numexpr
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall numba
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip cache purge

# Then, back in R run this (ensuring libcbm is very last)

library("reticulate")
reticulate::use_virtualenv(virtualenv = "r-reticulate")
reticulate::py_install("numpy==1.26.4", envname = "r-reticulate")
reticulate::py_install("pandas==2.2.2", envname = "r-reticulate")
reticulate::py_install("libcbm", envname = "r-reticulate")

#other lines
# can you run this and share the output?
  C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip freeze

