`CBM_defaults` reads all the default parameters and disturbance matrices for running the SpaDES CBM family of modules.
**See also** `CBM_dataPrep`, `CBM_core`, and `CBM_vol2biomass`.

CBM-CFS3 science was re-written by Scott Morken, a programmer for the development of CBM-CFS3 which runs in an MS Access framework.
Scott used R and wrapped C++ functions to emulate what happens in CBM-CFS3.

An SQLite database is required to run this module (`inputs/cbm_defaults/cbm_defaults.db`) as well as a series of sql scripts to extract the parameters from the database.
Both the database and the sql scripts are provided in the `CBM_defaults` data folder, which, unlike the main data folder in the family of modules (i.e, `inputs/`) is not in `.gitignore`.
The database corresponds to the 2018 version of the CBM-CFS3 Archive Index.
No updates are provided since all parameters are accessible and changeable through the R environment.

This module runs independently of the other modules in the SpaDES CBM family.
