Sys.setenv(PKG_CXXFLAGS = "-std=c++0x") ## TODO: not needed on Ubuntu 20.04; elsewhere?
# sourceCpp(file='RCBMStep.cpp')
Rcpp::sourceCpp(
  file = "RCBMGrowthIncrements.cpp", ## TODO: source from module directory, not main repo
  cacheDir = cachePath(sim),
  env = envir(sim)[[".mods"]][["spadesCBMcore"]]
)
