# <<- operator: assign a value to an object in an environment that is different from the current environment

# Example: create a special vector
makeVector <- function(x = numeric()) {
  m <- NULL
  # Set value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Get the value of the vector
  get <- function() x
  # Set the value of the mean
  setmean <- function(mean) m <<- mean
  # Get the value of the mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# Example: calculates the mean of the special vector
cachemean <- function(x, ...) {
  m <- x$getmean()
  # Check if the mean has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Calculates mean 
  data <- x$get()
  m <- mean(data, ...)
  # Sets the value of the mean in the cache via
  x$setmean(m)
  m
}

# Assignment: caching the inverse of a matrix

# Creates a special matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Set value of the vector
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Get the value of the vector
  get <- function() x
  # Set the value of the mean
  setinv <- function(inv) i <<- inv
  # Get the value of the mean
  getinv <- function() i
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}

# Computes the inverse of the special matrix
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  # Check if the inverse has already been calculated
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Calculates inverse 
  data <- x$get()
  i <- solve(data, ...)
  # Sets the value of the inverse in the cache via
  x$setinv(i)
  i
}