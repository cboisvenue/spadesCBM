# spadesCBM

Three-module Carbon Budget Model (CBM) implemented in `SpaDES`

In summer 2017 Scott Morken wrote a second incarnation of a CBM-like R-based carbon model in an intensive 2-week stint with EMcIntire and CBoisvenue.
The code and scripts, including the calls to C++ via `Rcpp`, are all contained in `RunSK_new.R`.
This emulates the science in CBM-CFS3 but works in matrix operations (the original CBM-CFS3 does not).
Scott got a spin up working with a comparison for 1 stand in SK, which he ran for 100 years post spin-up in both CBMCFS3 and the script as developed here.
Results are the same compared to classic CBMCFS3 (see `C:\Celine\GitHub\RCBM\data\12_Spades_run\CBM3BaselineTesting`).
A map of the links and functions of the R and C++ code (created by CBoisvenue) is available here (`G:\RES_Work\Work\SpaDES\work\RCBMoverview\June2017CBMcore\cbmCoreMap.xlsx`).
SMorken's work is archived in a GitHub repo (<https://github.com/cboisvenue/RCBM> branch `rcbmV1cb`).

The present code is the new repo that forms the `spadesCBM` family (<https://github.com/cboisvenue/spadesCBM>).
The initial effort has three `spadesCBM`-family modules: `spadesCBMdefaults`, `spadesCBMinputs`, and `spadesCBMcore`.
These are three child modules that are run by the current `spadesCBM` parent module. 

Documentation describing these three modules is in `spadesCBM\spadesCBM.Rmd`.

An overview of this family is in a prezi here `G:\RES_Work\Work\SpaDES\spadesCBM\Prezi WIN spadesCBM modules ov.exe`.

List of improvements that still need to be done to the Rcpp code:
CBM features not yet implemented:
- "Flux indicators" CBM-speak for automatic calculation of NPP, heterotrophic respiration, and NEP
- "Land-use change tracking" pieces of land that switch between the UNCFFF land-classification such as Forest-Land to Agricultural-Land, etc.
- "Transition rules" CBM-speak for switching from one growth trajectory pre-disturbance to a different growth trajectory post disturbance.
- Testing: test the script against lots of different edge cases.  The priority might be to test hardwood stands, and the mixed hardwood softwood stands versus and equivalent stand in CBM-CFS3.  So far we’ve only seen one stand confirmed against CBM results
Function documentation - RCpp functions are documented in the map of RunSK_new.R (`G:\RES_Work\Work\SpaDES\work\RCBMoverview\June2017CBMcore\cbmCoreMap.xlsx`), but a clearer map of what RCpp functions does what in each spadesCBM modules would be useful.
- pools "input" object is passed by reference - programmer to change that so we can Cache() things...
- something is also passed by reference in the stepfunct - to change by programmer that also.
- **DANGER with the `spinupDebug` parameter**: The `spinupDebug parameter` is a logical parameter defined in the metadata of this module (in `spadesCBMcore.R`). It determines if the results from the spinup will be saved as an external file. The default is `FALSE`. If this is set to `TRUE`, the `postSpinup` event re-runs the cpp Spinup function because the cpp Spinup function actually uses the spinpupDebug and spits-out all the runs of for all the stands until the stabilization of the DOM pools instead of the DOM for each stand needed to start the annual simulation runs. This could take a long time if you have a lot of stands/pixels. This can be changed later, it was a work around to get these modules running. 
- **Known Errors carried over from `carb1`:** `Error in Spinup(pools = sim$pools, opMatrix = opMatrix, constantProcesses = sim$processes,  :   Expecting a single value: [extent=0]`. This is because the pooldef values are expected to be in the `.GlobalEnv` due to a function inside `RCMBGrowthIncrements.cpp`. That file should be changed so it is not looking in `.GlobalEnv`. Current work around is to place all the pooldef values in `.GlobalEnv`. This happens inside the `.inputObjects` function.

List of SpaDES-related work:
- Make this work with spatial data (module?) - first test spot SK data used in Boisvenue et al 2016 (almost done - test area running January 2019)
- check the volume to biomass function with the SK PSP-built growth curves and biomass info
- expand the `CBMVolumeToBiomass` library to access all Boudewyn et al parameters, and to be able to skip the double "translation". This probably means a new transparent module for volume to biomass conversion with the capability of skipping directly to the use of biomass (as opposed to $m^3 / ha$).
- documentation of `CBMVolumeToBiomass` and the whole new module
- make modules for all the parts of the S4 `cbm_default.db` so that it is clear what those are and where they come from, so that eventually any part of them could be replaced/tested/data assimilation
- `decayRates`: matrix 48X12 - holds the decay rates for all the dom pools (11 of them) per spatial unit. Where is this created? How does it link to the "decay_parameter" table in `cbmData`?

**SEE LOG**
 
