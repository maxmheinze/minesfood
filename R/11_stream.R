
# Retrieve up- and downstream basins of 'id' using the NEXT_DOWN field
stream <- \(id, n = 1L, max = MAX_ORDER + 1, down = TRUE) {
 if(n >= max) return()
 if(down) {
   id_next <- d[["NEXT_DOWN"]][d[["HYBAS_ID"]] == id]
 } else { # Upstream
   id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] %in% id]
 }
 if(all(id_next == 0) || length(id_next) == 0) return()
 return(c(id_next, Recall(id_next, n = n + 1L, max = max, down = down)))
}

# Retrieve up- and downstream basins of 'id' and track the order
stream_ordered <- function(id, n = 1L, max = MAX_ORDER + 1, down = TRUE) {
  if (n >= max) return(NULL)
  if (down) {
    id_next <- d[["NEXT_DOWN"]][d[["HYBAS_ID"]] == id]
  } else { # Upstream
    id_next <- d[["HYBAS_ID"]][d[["NEXT_DOWN"]] %in% id]
  }
  if (all(id_next == 0) || length(id_next) == 0) return()
  results <- cbind(id_next, n)
  id_next <- unique(id_next)
  recursive_results <- Recall(id_next, n = n + 1L, max = max, down = down)
  if (!is.null(recursive_results)) {
    results <- rbind(results, recursive_results)
  }
  return(results)
}

# Retrieve the distance between two IDs from a dataframe
get_distance <- function(id1, id2, dataframe, downstream = TRUE) {
  if (is.na(id1) | is.na(id2)) stop("IDs not found.")
  distance <- if(downstream) {
    dataframe[["distance"]][dataframe[["HYBAS_ID"]] == id2 & dataframe[["NEXT_DOWN"]] == id1]
  } else {
    dataframe[["distance"]][dataframe[["HYBAS_ID"]] == id1 & dataframe[["NEXT_DOWN"]] == id2]
  }
  if (length(distance) == 0) {
    message("No distance between ", id1, " and ", id2, " found.")
    return(NA_real_)
  }
  return(distance)
}
get_distance_upstream <- \(...) get_distance(..., downstream = FALSE)
