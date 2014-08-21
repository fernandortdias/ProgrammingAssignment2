## This two functions explores the use of cache for speeding up inverse matrix 
## calculations. Calculation of inverse matrix are time and resource consuming, 
## so have them calculated once and stored may be a good idea on some situations.
## In order to do cache this calculation, a special object is created that with 
## a list of functions to set and get the value of the matrix and its inverse.

## The first function, makeVector creates a special "matrix", which is really a 
## a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix, using function solve()
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(x, set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##The following function calculates the inverse  of the special "matrix" created 
##with the above function. It first checks to see if its inverse has 
##already been calculated. If so, it gets the inverse from cache and skips the 
##computation. Otherwise, it calculates the inverse of the matrix using solve() 
## and sets the value of it the mean in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
