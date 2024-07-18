# minesfood

The structure of this repository is

- `scripts/` with scripts to run
- `R/` with functions to use
- `inst/` with small reference files
- `data/` with (intermediate) data outputs

The `.Rprofile` (which should be sourced automagically) defines two paths (`DATA` and `DATA_ALT`) which point to shared data, and the function `p()` which is `paste0` with a prepended argument with default `pre = DATA`.
That means – wrap your paths with `p()` to access the shared data folder.
