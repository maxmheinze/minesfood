# minesfood

The structure of this repository is

- `scripts/` with scripts to run
- `R/` with functions to use
- `inst/` with small reference files
- `data/` with (intermediate) data outputs

Note that `R/00_prelims.R` defines the following

- `DATA` which points to a shared (on the server) data directory,
- `DATA_ALT` which points to another shared directory with data from another project that is used to create covariates,
- `p()` which acts as `paste0`, but prepends the argument `pre`, which defaults to `DATA`.

In order to avoid the mess from using absolute paths everywhere, just use `p()` to access files in the shared directory, e.g. `readRDS(p("file.rds"))`.
