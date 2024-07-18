
message("Setting shared paths.")

# Paths to the data (shared between users on the Server)
DATA <- "/data/jde/" # Main
DATA_ALT <- "/data/redd" # Alternative, reused from another project

p <- \(path, ..., pre = DATA) { # More concise paths
  paste0(pre, path, ...)
}

if(!dir.exists(DATA)) {
  message("\tCouldn't find the shared directory `DATA`. ",
    "Attempting to create the directory...")
  dir.create(p(DATA, "processed"),
    showWarnings = FALSE, recursive = TRUE) # Make sure this exists
}
if(!dir.exists(DATA_ALT)) {
  message("\tCouldn't find the directory `DATA_ALT`. ",
    "This may prevent some code from executing properly.")
}
