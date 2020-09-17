
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "spadesCBM",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = c("spadesCBMdefaults","spadesCBMinputs","spadesCBMm3ToBiomass","spadesCBMcore"),
  version = list(SpaDES.core = "0.1.0.9007", spadesCBM = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "spadesCBM.Rmd")
))
