## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  # 's' will store the cached inverse of the matrix, initially NULL.
  
  # The 'set' function updates the matrix with a new value and clears the cached inverse.
  set <- function(y) {
    x <<- y  # Assigns the new matrix to 'x' in the parent environment.
    s <<- NULL  # Clears any previously cached inverse since the matrix has changed.
  }
  
  # The 'get' function simply returns the current matrix.
  get <- function() x
  
  # The 'setsolve' function stores the inverse of the matrix in the cache.
  setsolve <- function(solve) s <<- solve
  
  # The 'getsolve' function retrieves the cached inverse, if it exists.
  getsolve <- function() s
  
  # Returns a list of all four functions, giving external access to the matrix and cache.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Retrieves any existing cached inverse.
  s <- x$getsolve()
  
  # If the cached inverse exists, a message is shown and it is returned immediately.
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  
  # If there is no cached inverse, the function gets the matrix.
  data <- x$get()
  
  # Calculates the inverse of the matrix.
  s <- solve(data, ...)
  
  # Caches the calculated inverse for future use.
  x$setsolve(s)
  
  # Returns the newly calculated inverse.
  s
}
