## makeCacheMatrix and cacheSolve compute the inverse of an invertable
## matrix and store the result. If the inverse is required again, the
## cached result is used instead of computed again
## example:
##    cm <- makeCacheMatrix(rnorm(9), 3, 3)
##    cacheSolve(cm) // => inverse of matrix is computed and stored
##    cacheSolve(cm) // => inverse of matrix is retrieved from cache and returned

## makeCacheMatrix takes an invertable matrix, stores it and returns an object
## with the following functions:
## - get: returns the stored matrix
## - set: stores a new, invertable matrix
## - setsolve: stores the compute inverse of the matrix
## - getsolve: returns the stored inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # empty inverse matrix value 
  m <- NULL

  # set `y` as the matrix and empty the inverse matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # return the store matrix
  get <- function()  x

  # stores the inverted matrix `solved`
  setsolve <- function(solved) m <<- solved

  # returns the inverted matrix
  getsolve <- function() m

  # exposes the functions
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}

## cacheSolve takes an `cacheMatrix` object x and returns its inverse. If the
## inverse is computed before, the cached result is used
cacheSolve <- function(x, ...) {

  # try to return a matrix that is the inverse of 'x'
  m <- x$getsolve()

  if (!is.null(m)) {
    # matrix x has its inverse cache, so that value is returned
    return(m)
  }

  # get the stored matrix
  data <- x$get()

  # compute its inverse
  m <- solve(data, ...)

  # store the inverted matrix
  x$setsolve(m)

  # return the inverted matrix
  m
}