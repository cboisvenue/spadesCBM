## use empty .Rprofile to prevent loading of user's .Rprofile
local({
  message("initializing spadesCBM_RIA project...\n")

  ## TODO: trying force Rstudio to behave...
  local({
    prjDir <- switch(Sys.info()[["user"]],
                     cboisven = "C:/Celine/github/spadesCBM",
                     "~/GitHub/spadesCBM")
    pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                        version$platform, getRversion()[, 1:2])
    dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
    .libPaths(pkgDir, include.site = FALSE)
    message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))
  })

  message("\n...done")
})
