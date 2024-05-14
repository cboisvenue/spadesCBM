## use empty .Rprofile to prevent loading of user's .Rprofile
local({
  if (Sys.info()[["user"]] == "achubaty") {
    message("initializing spadesCBM project...\n")

    ## TODO: trying force Rstudio to behave...
    prjDir <- "~/GitHub/spadesCBM"
    pkgDir <- file.path(tools::R_user_dir(basename(prjDir), "data"), "packages",
                        version$platform, getRversion()[, 1:2])
    dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
    .libPaths(pkgDir, include.site = FALSE)
    message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

    message("\n...done")
  }
})

#### setLibPaths start #### New File:FALSE # DO NOT EDIT BETWEEN THESE LINES
### DELETE THESE LINES BELOW TO RESTORE STANDARD R Package LIBRARY
### Previous .libPaths: C:/Users/cboisven/AppData/Local/R/win-library/4.3, C:/Program Files/R/R-4.3.2/library
._libPaths <- c('C:/Users/cboisven/AppData/Roaming/R/data/R/spadesCBM/packages/x86_64-w64-mingw32/4.3')
._standAlone <- TRUE
{
    .oldLibPaths <- .libPaths()
    if (!dir.exists(._libPaths[1])) dir.create(._libPaths[1], recursive = TRUE)
    gte4.1 <- isTRUE(getRversion() >= "4.1")
    if (gte4.1) {
        do.call(.libPaths, list(new = ._libPaths, if (gte4.1) include.site <- !._standAlone))
    }
    .shim_fun <- .libPaths
    .shim_env <- new.env(parent = environment(.shim_fun))
    if (isTRUE(._standAlone)) {
        .shim_env$.Library <- utils::tail(.libPaths(), 1)
    }
    else {
        .shim_env$.Library <- .libPaths()
    }
    .shim_env$.Library.site <- character()
    environment(.shim_fun) <- .shim_env
    .shim_fun(unique(._libPaths))
    message(".libPaths() is now: ", paste(.libPaths(), collapse = ", "))
}
message("To reset libPaths to previous state, run: Require::setupOff() (or delete section in .Rprofile file)") 
#### setLibPaths end
