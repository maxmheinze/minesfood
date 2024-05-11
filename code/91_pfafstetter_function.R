# Check whether pfaf_code_i is downstream of pfaf_code_ii
`%DS%` <- function(pfaf_code_i, pfaf_code_ii) {
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
  # > "123" %DS% "125"
  # [1] TRUE
  # --------------------
  
  # Before we start, check that there are no letters in the strings.
  if (grepl("[^0-9]", pfaf_code_i) || grepl("[^0-9]", pfaf_code_ii)) {
    stop("Input strings can only contain numbers.")
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
