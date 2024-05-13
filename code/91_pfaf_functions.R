
# Check Whether a Basin Is Downstream of Another Basin --------------------

pfaf_downstream <- function(pfaf_code_i, pfaf_code_ii) {
  # This function checks two strings against each other. The strings should represent
  # Pfafstetter river basin codes. The function outputs TRUE if the first string is
  # downstream of the second, and FALSE if it is not downstream. The function can
  # also handle two inputs that are not of the same length, but definitions get a 
  # bit shaky in these cases. We define "downstream" in these cases as follows: At
  # least some part of Basin 1 must be affected by all of Basin 2 in order for Basin
  # 1 to be considered downstream of Basin 2. Thus, a basin that contains another basin
  # is considered downstream of that basin, a basin that is contained within another
  # basin is not considered downstream of that basin. A basin is also considered
  # as being downstream of itself.
  
  # Usage:
  # --------------------
  # > pfaf_downstream("123", "125")
  # [1] TRUE
  # --------------------
  
  # Before we start, check that there are no letters in the strings.
  if (grepl("[^0-9]", pfaf_code_i) || grepl("[^0-9]", pfaf_code_ii)) {
    stop("Input strings can only contain numbers.")
  }
  
  # Check whether they are strings
  if (!is.character(pfaf_code_i) || !is.character(pfaf_code_ii)) {
    stop("Both inputs must be character strings.")
  }
  
  # If strings are of same length (i.e. the main use case)
  if (nchar(pfaf_code_i) == nchar(pfaf_code_ii)) {
    
    # Count from the left the digits that are equal
    n <- sum(sapply(1:nchar(pfaf_code_i), function(x) substr(pfaf_code_i, 1, x) == substr(pfaf_code_ii, 1, x)))
    
    # A basin is considered downstream of itself
    if (n == nchar(pfaf_code_i)) {
      return(TRUE)
    }
    
    # Check if the number made up of the remaining digits of pfaf_code_i is smaller than that of pfaf_code_ii
    # Output FALSE if it is not smaller, or if it is NA (i.e., the strings are equal)
    remaining_i <- as.integer(substr(pfaf_code_i, n+1, nchar(pfaf_code_i)))
    remaining_ii <- as.integer(substr(pfaf_code_ii, n+1, nchar(pfaf_code_ii)))
    if (is.na(remaining_i) || is.na(remaining_ii) || remaining_i >= remaining_ii) {
      return(FALSE)
    }
    
    # Check if all remaining digits of pfaf_code_i are odd. If at least one is not, output FALSE
    remaining_digits_i <- as.integer(strsplit(substr(pfaf_code_i, n+1, nchar(pfaf_code_i)), "")[[1]])
    if (any(remaining_digits_i %% 2 == 0)) {
      return(FALSE)
    }
    
    # Return TRUE otherwise (we are still in the part of equal-length strings)
    return(TRUE)
    
  # Now we are talking about different length strings
  } else {
    
    # Again, count from the left the digits that are equal. The only thing that's different is 
    # that we stop at the minimum of the lengths of the two strings
    n <- sum(sapply(1:min(nchar(pfaf_code_i), nchar(pfaf_code_ii)), function(x) substr(pfaf_code_i, 1, x) == substr(pfaf_code_ii, 1, x)))
    
    # Here we consider the case where the first string is shorter than the second string. We 
    # determine a higher-level basin downstream of a lower-level basin as long as some
    # portion of the higher-level basin is downstream of the lower-level basin. That is,
    # it is sufficient to check whether the basin that the lower-level basin is in, but that
    # corresponds to the level of our higher-level basin, is upstream of that higher-level
    # basin.
    if (nchar(pfaf_code_i) < nchar(pfaf_code_ii)) {
      
      # m is the length of the shorter string after we clipped off all digits that are equal.
      m <- nchar(pfaf_code_i) - n
      
      # If m is 0, the first basin encompasses the second (lower-level) basin and is thus
      # by our definition from above downstream because some part of it is downstream of
      # the second basin. That part can be the second basin itself. That is, we assume that
      # a basin is downstream of itself. 
      if (m == 0) {
        return(TRUE)
      }
      
      # Check if the number made up by the remaining digits of pfaf_code_i is smaller than that of pfaf_code_ii.
      # Same process as above, but the longer string needs to be cut off at n + m.
      remaining_i <- as.integer(substr(pfaf_code_i, n+1, nchar(pfaf_code_i)))
      remaining_ii <- as.integer(substr(pfaf_code_ii, n+1, n + m))
      
      # Return FALSE if it is not smaller.
      if (remaining_i >= remaining_ii) {
        return(FALSE)
      }
      
      # If it is smaller, check whether all remaining digits are odd. 
      remaining_digits_i <- as.integer(strsplit(substr(pfaf_code_i, n+1, nchar(pfaf_code_i)), "")[[1]])
      
      # If not, output FALSE.
      if (any(remaining_digits_i %% 2 == 0)) {
        return(FALSE)
      }
      
      # If yes, output TRUE.
      return(TRUE)
      
    # This part concerns cases where the second string is shorter than the first string. That is,
    # The second basin is a higher-level basin.
    } else {
      
      # m is the length of the shorter string after we clipped off all digits that are equal.
      m <- nchar(pfaf_code_ii) - n
      
      # If m is 0, the second basin encompasses the first (lower-level) basin. We determine the
      # first basin to not be downstream of the second basin since it is not affected by all of the basin.
      if (m == 0) {
        return(FALSE)
      }
      
      # Check if the number made up by the first m remaining digits of pfaf_code_i is smaller than that of pfaf_code_ii.
      # Same process as above, again, the longer string needs to be cut off at n + m.
      remaining_i <- as.integer(substr(pfaf_code_i, n+1, n+m))
      remaining_ii <- as.integer(substr(pfaf_code_ii, n+1, nchar(pfaf_code_ii)))
      
      # If it is not smaller, return FALSE.
      if (remaining_i >= remaining_ii) {
        return(FALSE)
      }
      
      # If it is smaller, Check if all remaining digits of pfaf_code_i are odd
      remaining_digits_i <- as.integer(strsplit(substr(pfaf_code_i, n+m+1, nchar(pfaf_code_i)), "")[[1]])
      
      # Return FALSE if at least one is not
      if (any(remaining_digits_i %% 2 == 0)) {
        return(FALSE)
      }
      
      # Return TRUE otherwise.
      return(TRUE)
    }
  }
}

# Check whether a basin is upstream of another basin
pfaf_upstream <- function(pfaf_code_i, pfaf_code_ii) {
  # Usafe 
  # If i is upstream of ii, ii must be downstream of i. Thus:
  return(pfaf_downstream(pfaf_code_ii, pfaf_code_i))
}

# Same functions, easier syntax
# Usage:
# --------------------
# > "123" %DS% "125"
# [1] TRUE
# --------------------
`%DS%` <- pfaf_downstream
`%US%` <- pfaf_upstream


# Output a List of Basins that are Downstream of Each Basin ---------------

pfaf_list_downstream <- function(pfaf_codes_vector, reflexive = TRUE) {
  # This function takes as input a vector containing a number of river
  # basin codes. It then uses the pfaf_downstream() function to compare
  # all of them against each other and outputs a list containing a 
  # vector of downstream basins for each of the basins contained in the
  # input vector.
  
  # Check if reflexive is TRUE or FALSE
  if (!is.logical(reflexive) || length(reflexive) != 1) {
    stop("Reflexive must be a single logical value (TRUE or FALSE).")
  }
  
  # Check for duplicates
  if (length(unique(pfaf_codes_vector)) != length(pfaf_codes_vector)) {
    stop("Input vector must not contain duplicate elements.")
  }
  
  # Check if all elements are strings
  if (!all(sapply(pfaf_codes_vector, is.character))) {
    stop("All elements of the vector must be strings.")
  }
  
  # Check if all elements are numeric strings
  if (!all(sapply(pfaf_codes_vector, function(x) grepl("^[0-9]+$", x)))) {
    stop("All elements must contain only numeric characters.")
  }
  
  # Check if all elements are of the same length
  if (length(unique(nchar(pfaf_codes_vector))) != 1) {
    stop("All strings must be of the same length.")
  }
  
  # Initialize a list to store downstream basin vectors, name it 
  # using the original basin codes.
  downstream_list <- vector("list", length(pfaf_codes_vector))
  names(downstream_list) <- pfaf_codes_vector
  
  # Loop through each basin code in the vector.
  for (i in seq_along(pfaf_codes_vector)) {
    
    # Get the current basin code
    current_basin <- pfaf_codes_vector[i]
    
    # Initialize a vector to store basins that are downstream of the current basin
    downstream_basins <- c()
    
    # Compare the current basin with every other basin
    for (j in seq_along(pfaf_codes_vector)) {
      
      # If the reflexive option is set to FALSE, avoid comparing the basin
      # with itself, as such a comparison will yield include the basin in the
      # list of downstream basins.
      if (reflexive == FALSE) { 
        
        # Avoid the self-comparison.
        if (i != j) { 
          
          # Check if the j-th basin is downstream of the i-th basin. If yes,
          # add it to the vector that will be added to the output list.
          if (pfaf_downstream(pfaf_codes_vector[j], current_basin)) {
            downstream_basins <- c(downstream_basins, pfaf_codes_vector[j])
          }
        }
        
      # If the reflexive option is set to TRUE, do the same but do not include
      # the line that avoids a self-comparison.  
      } else {
        if (pfaf_downstream(pfaf_codes_vector[j], current_basin)) {
          downstream_basins <- c(downstream_basins, pfaf_codes_vector[j])
        }
      }
    }
    
    # Assign the vector of downstream basins to the corresponding list entry.
    downstream_list[[i]] <- downstream_basins
  }
  
  # Finally, return the desired list.
  return(downstream_list)
}

# Output a List of Basins that are Upstream of Each Basin -----------------

pfaf_list_upstream <- function(pfaf_codes_vector, reflexive = TRUE) {
  # This function takes as input a vector containing a number of river
  # basin codes. It then uses the pfaf_upstream() function to compare
  # all of them against each other and outputs a list containing a 
  # vector of upstream basins for each of the basins contained in the
  # input vector.
  
  # Check if reflexive is TRUE or FALSE
  if (!is.logical(reflexive) || length(reflexive) != 1) {
    stop("Reflexive must be a single logical value (TRUE or FALSE).")
  }
  
  # Check for duplicates
  if (length(unique(pfaf_codes_vector)) != length(pfaf_codes_vector)) {
    stop("Input vector must not contain duplicate elements.")
  }
  
  # Check if all elements are strings
  if (!all(sapply(pfaf_codes_vector, is.character))) {
    stop("All elements of the vector must be strings.")
  }
  
  # Check if all elements are numeric strings
  if (!all(sapply(pfaf_codes_vector, function(x) grepl("^[0-9]+$", x)))) {
    stop("All elements must contain only numeric characters.")
  }
  
  # Check if all elements are of the same length
  if (length(unique(nchar(pfaf_codes_vector))) != 1) {
    stop("All strings must be of the same length.")
  }
  
  # Initialize a list to store upstream basin vectors, name it 
  # using the original basin codes.
  upstream_list <- vector("list", length(pfaf_codes_vector))
  names(upstream_list) <- pfaf_codes_vector
  
  # Loop through each basin code in the vector.
  for (i in seq_along(pfaf_codes_vector)) {
    
    # Get the current basin code
    current_basin <- pfaf_codes_vector[i]
    
    # Initialize a vector to store basins that are upstream of the current basin
    upstream_basins <- c()
    
    # Compare the current basin with every other basin
    for (j in seq_along(pfaf_codes_vector)) {
      
      # If the reflexive option is set to FALSE, avoid comparing the basin
      # with itself, as such a comparison will yield include the basin in the
      # list of upstream basins.
      if (reflexive == FALSE) { 
        
        # Avoid the self-comparison.
        if (i != j) { 
          
          # Check if the j-th basin is downstream of the i-th basin. If yes,
          # add it to the vector that will be added to the output list.
          if (pfaf_upstream(pfaf_codes_vector[j], current_basin)) {
            upstream_basins <- c(upstream_basins, pfaf_codes_vector[j])
          }
        }
        
        # If the reflexive option is set to TRUE, do the same but do not include
        # the line that avoids a self-comparison.  
      } else {
        if (pfaf_upstream(pfaf_codes_vector[j], current_basin)) {
          upstream_basins <- c(upstream_basins, pfaf_codes_vector[j])
        }
      }
    }
    
    # Assign the vector of downstream basins to the corresponding list entry.
    upstream_list[[i]] <- upstream_basins
  }
  
  # Finally, return the desired list.
  return(upstream_list)
}


# Check the Distance Between Two Basins -----------------------------------


pfaf_distance <- function(pfaf_code_i, pfaf_code_ii, pfaf_codes_vector) {
  # This function calculates the distance between two basins. The distance
  # between a basin and itself is 0. The distance between a basin and the
  # one directly upstream or downstream is 1. The distance to the basin 
  # upstream or downstream of that one is 2, and so on. The order in which
  # the basins are entered does not matter. If two basins are entered that
  # are not connected by a water flow, the function will output NA.
  
  # Check for duplicates
  if (length(unique(pfaf_codes_vector)) != length(pfaf_codes_vector)) {
    stop("Input vector must not contain duplicate elements.")
  }
  
  # Check if all elements are strings
  if (!all(sapply(pfaf_codes_vector, is.character))) {
    stop("All elements of the input vector must be strings.")
  }
  
  # Check if all elements are numeric strings
  if (!all(sapply(pfaf_codes_vector, function(x) grepl("^[0-9]+$", x)))) {
    stop("All elements of the input vector must contain only numeric characters.")
  }
  
  # Check if all elements are of the same length
  if (length(unique(nchar(pfaf_codes_vector))) != 1) {
    stop("All strings in the input vector must be of the same length.")
  }
  
  # Check if inputs are actual river basin codes
  if (!is.character(pfaf_code_i) || length(pfaf_code_i) != 1 || 
      !is.character(pfaf_code_ii) || length(pfaf_code_ii) != 1) {
    stop("Both river basin codes must be character strings.")
  }
  
  # Check if they are contained in the input vector
  if (!(pfaf_code_i %in% pfaf_codes_vector && pfaf_code_ii %in% pfaf_codes_vector)) {
    stop("Both river basin strings must be contained in the input vector.")
  }
  
  # If i is downstream
  if(pfaf_downstream(pfaf_code_i, pfaf_code_ii)){
    
    # Filter the vector of all basins for those that i is downstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(pfaf_code_i, x))]
    
    # Filter the vector of all basins for those that ii is upstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(x, pfaf_code_ii))]
    
    # Return the number of basins left minus one.
    return(length(pfaf_codes_vector)-1)
  }
  # If i is upstream
  if(pfaf_downstream(pfaf_code_ii, pfaf_code_i)){
    
    # Filter the vector of all basins for those that ii is downstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(pfaf_code_ii, x))]
    
    # Filter the vector of all basins for those that i is upstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(x, pfaf_code_i))]
    
    # Return the number of basins left minus one.
    return(length(pfaf_codes_vector)-1)
  } else{
    # If none is downstream of the other
    return(NA)
  }
}


# Compute the Path Between Two Basins -------------------------------------

pfaf_path <- function(pfaf_code_i, pfaf_code_ii, pfaf_codes_vector) {
  # This function outputs the "path" between two basins, i.e., an ordered
  # vector of all basins that water flows through on its way from the one 
  # to the other specified basin. The order in which the basins are entered
  # does not matter. The output vector will always start from the upstream
  # basin.
  
  # Check for duplicates
  if (length(unique(pfaf_codes_vector)) != length(pfaf_codes_vector)) {
    stop("Input vector must not contain duplicate elements.")
  }
  
  # Check if all elements are strings
  if (!all(sapply(pfaf_codes_vector, is.character))) {
    stop("All elements of the input vector must be strings.")
  }
  
  # Check if all elements are numeric strings
  if (!all(sapply(pfaf_codes_vector, function(x) grepl("^[0-9]+$", x)))) {
    stop("All elements of the input vector must contain only numeric characters.")
  }
  
  # Check if all elements are of the same length
  if (length(unique(nchar(pfaf_codes_vector))) != 1) {
    stop("All strings in the input vector must be of the same length.")
  }
  
  # Check if inputs are actual river basin codes
  if (!is.character(pfaf_code_i) || length(pfaf_code_i) != 1 || 
      !is.character(pfaf_code_ii) || length(pfaf_code_ii) != 1) {
    stop("Both river basin codes must be character strings.")
  }
  
  # Check if they are contained in the input vector
  if (!(pfaf_code_i %in% pfaf_codes_vector && pfaf_code_ii %in% pfaf_codes_vector)) {
    stop("Both river basin strings must be contained in the input vector.")
  }
  
  
  # If i is downstream
  if(pfaf_downstream(pfaf_code_i, pfaf_code_ii)){
    
    # Filter the vector of all basins for those that i is downstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(pfaf_code_i, x))]
    
    # Filter the vector of all basins for those that ii is upstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(x, pfaf_code_ii))]
    
    # The remaining vector contains only basins that are on the path from 
    # ii to i. Thus, we can sort them using pfaf_downstream()
    n <- length(pfaf_codes_vector)
    for (i in 2:n) {
      key <- pfaf_codes_vector[i]
      j <- i - 1
      while (j > 0 && !pfaf_downstream(key, pfaf_codes_vector[j])) {
        pfaf_codes_vector[j + 1] <- pfaf_codes_vector[j]
        j <- j - 1
      }
      pfaf_codes_vector[j + 1] <- key
    }
    
    return(pfaf_codes_vector)
  }
  # If i is upstream
  if(pfaf_downstream(pfaf_code_ii, pfaf_code_i)){
    
    # Filter the vector of all basins for those that ii is downstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(pfaf_code_ii, x))]
    
    # Filter the vector of all basins for those that i is upstream of
    pfaf_codes_vector <- pfaf_codes_vector[sapply(pfaf_codes_vector, function(x) pfaf_downstream(x, pfaf_code_i))]
    
    # The remaining vector contains only basins that are on the path from 
    # ii to i. Thus, we can sort them using pfaf_downstream()
    n <- length(pfaf_codes_vector)
    for (i in 2:n) {
      key <- pfaf_codes_vector[i]
      j <- i - 1
      while (j > 0 && !pfaf_downstream(key, pfaf_codes_vector[j])) {
        pfaf_codes_vector[j + 1] <- pfaf_codes_vector[j]
        j <- j - 1
      }
      pfaf_codes_vector[j + 1] <- key
    }
    
    return(pfaf_codes_vector)
  } else{
    # If none is downstream of the other
    return(NA)
  }
}

   
