## this is a place to save all the Python related info

##function to load Python and the correct version of packages for libcbm


### If you do not have Python do this:
library(reticulate)
version <- "3.11.9" # see https://www.python.org/ftp/python/ for the available version strings
install_python(version)
virtualenv_create("r-reticulate", version = version)
use_virtualenv("r-reticulate")
# the above lines slightly modified from here:
# https://rstudio.github.io/reticulate/reference/install_python.html

##After installing Python it is always good to check where it is thinking your
##Python .exe is.
reticulate::import("sys")$executable
# Note that this should be the same after a restart. If you reinstal reticulate,
# you have to make sure the virtual environment is correct and finds your Python
# .exe

##### getting the libcbm package####################

### Installing libcbm with numpy<2.0 There is currently a problem with running
### libcbm with the Python package numpy 2.0.
reticulate::py_install("numpy<2", envname = "r-reticulate")
reticulate::py_install("pandas>=1.1.5", envname = "r-reticulate")
reticulate::py_install("scipy", envname = "r-reticulate")
reticulate::py_install("numexpr>=2.8.7", envname = "r-reticulate")
reticulate::py_install("numba", envname = "r-reticulate")
reticulate::py_install("pyyaml", envname = "r-reticulate")
reticulate::py_install("mock", envname = "r-reticulate")
reticulate::py_install("openpyxl", envname = "r-reticulate")

### Alternatively, you can load the libcbm from the command line like this:

#After you create the environment named: "r-reticulate"
#Here is how I would install the libcbm requirements at a command prompt:

# get the path to your python install with
the_path <- reticulate::import("sys")$executable

#Then at a command prompt use the above path which should end in python.exe like this:

<the_path>\python.exe -m pip install -r requirements.txt
# put the full path to where the requirements.txt file is. In Celine's case, it
# is here:
#C:\Celine\Syncdocs\RES_Work\Work\LandRCBM\libCBMtransition\PythonToR\requirements.txt

### Once the above package versions are loaded, load libcbm (always do this
### last)
reticulate::py_install("libcbm", envname = "r-reticulate")

###############################################################################
### If you need to get rid of all your python installation try this:
#If you are not using python other things than for cbm_exn, I'd also consider cleanup of other installations as well
#there might be more environments/installs in here:
C:/Users/cboisven/AppData/Local/r-reticulate
#or here:
C:/Users/cboisven/AppData/Local/programs/python
# and you should remove this:
C:\Users\cboisven\Documents\.virtualenvs\r-reticulate

## if you have been working with Require you should do this also:
Require::clearRequirePackageCache()



### Installing libcbm with numpy<2.0
### There is currently a problem with running libcbm with the Python package
### numpy 2.0. It is recommended to load the libcbm from the command line like
### this:

#So after you create the environment named: "r-reticulate"
#Here is how I would install the libcbm requirements at a command prompt:

 # get the path to your python install with
the_path <- reticulate::import("sys")$executable

#Then at a command prompt use the above path which should end in python.exe like this:

  <the_path>\python.exe -m pip install -r requirements.txt
# put the full path to where the requirements.txt file is. In Celine's case, it
# is here:
#C:\Celine\Syncdocs\RES_Work\Work\LandRCBM\libCBMtransition\PythonToR\requirements.txt
# File content:
# numpy<2.0
# pandas>=1.1.5
# scipy
# numexpr>=2.8.7
# numba
# pyyaml
# mock
# openpyxl

### Other potentially useful info from Scott Morken:
##Lines of code from Scott Morken to remove all packages and reload them. Run
##this from command line
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall libcbm
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall numpy
C:\Users\cboisven\DOCUME~1\VIRTUA~1insta\R-RETI~1\Scripts\python.exe -m pip uninstall pandas
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall scipy
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall numexpr
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip uninstall numba
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip cache purge

# Then, back in R run this (ensuring libcbm is very last)

library("reticulate")
reticulate::use_virtualenv(virtualenv = "r-reticulate")
reticulate::py_install("numpy==1.26.4", envname = "r-reticulate")
reticulate::py_install("pandas==2.2.2", envname = "r-reticulate")
reticulate::py_install("libcbm", envname = "r-reticulate") ## this one always last

#other lines
# can you run this and share the output?
C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip freeze
# et-xmlfile==1.1.0
# libcbm==2.6.6
# llvmlite==0.43.0
# mock==5.1.0
# numba==0.60.0
# numexpr==2.10.1
# numpy==1.26.4
# openpyxl==3.1.5
# pandas==2.2.2
# python-dateutil==2.9.0.post0
# pytz==2024.1
# PyYAML==6.0.1
# scipy==1.13.1
# six==1.16.0
# tzdata==2024.1


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
# #put this last: because when you install libcbm, it will automatically install all of its dependencies, potentially overwriting your custom-selected pacakge versions
# reticulate::py_install("libcbm", envname = "r-reticulate")
##check things with
# py_config() ## this is confusing!!




#other lines
# can you run this and share the output?
  C:\Users\cboisven\DOCUME~1\VIRTUA~1\R-RETI~1\Scripts\python.exe -m pip freeze
  # et-xmlfile==1.1.0
  # libcbm==2.6.6
  # llvmlite==0.43.0
  # mock==5.1.0
  # numba==0.60.0
  # numexpr==2.10.1
  # numpy==1.26.4
  # openpyxl==3.1.5
  # pandas==2.2.2
  # python-dateutil==2.9.0.post0
  # pytz==2024.1
  # PyYAML==6.0.1
  # scipy==1.13.1
  # six==1.16.0
  # tzdata==2024.1

  # packages = NULL
  ##not sure if this should be in the out...runs like this on my desktop but not on
  ##my laptop...?
  # sideEffects = {
  #   reticulate::use_virtualenv(virtualenv = "./r-reticulate")
  #   reticulate::py_install("libcbm", envname = "./r-reticulate")
  # }
  ##these two package updates are only necessary if you have loaded version 2.0
  ##of numpy
  # reticulate::py_install("numpy==1.26.4", envname = "r-reticulate"),
  # reticulate::py_install("pandas==2.2.2", envname = "r-reticulate"),
  # reticulate::py_install("libcbm", envname = "r-reticulate")

  ##some usefull functions
  # the reticulate::py_to_r function that will convert any pandas dataframes to
  # R dataframes which should be a bit more user friendly to deal with for R
  # users
  # https://rstudio.github.io/reticulate/reference/r-py-conversion.html

