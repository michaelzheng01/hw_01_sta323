mh_distance <- function(x, y) {
  if(is.na(x) | is.na(y) | is.infinite(x) | is.infinite(y))  {
    warning('x and y cannot be NA, NaN, Inf, or -Inf')
    return(-1)
  } else if(mode(x) != "numeric" &
            mode(x) != "character" &
            mode(x) != "logical" &
            mode(y) != 'numeric' &
            mode(y) != "character" &
            mode(y) != "logical") {
    warning('x and y are not of type logical, character, or numeric')
    return(-1)
  } else if(typeof(x) != typeof(y)) {
    warning('x and y are not the same type')
    return(-1)
  } else if (is.logical(x)) {
    if(x == y) {
      return(0)
    } else {
      return(1)
    }
  } else if (is.numeric(x) | is.numeric(y)) {
    if(x %% 1 != 0 | y %% 1 != 0) {
      warning('x or y contains decimal values')
      return(-1)
    } else {
      x <- abs(x)
      y <- abs(y)
    }
  }
  x <- as.character(x)
  y <- as.character(y)
  l <- nchar(x)
  if(nchar(x) != nchar(y)) {
    warning('x and y are not the same length')
    return(-1)
  } else {
    x_split <- unlist(strsplit(x, split = ""))
    y_split <- unlist(strsplit(y, split = ""))
    a <- 1
    b <- 0
    while (a <= l) {
      if(x_split[a] != y_split[a]) {
        b <- b + 1
      }
      a <- a + 1
    }
    return(b)
  }
  
}
