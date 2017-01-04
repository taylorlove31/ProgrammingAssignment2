## The below functions, when makeCacheMatrix is nested within cacheSolve, take the matrix given
## as input and give its inverse.  In addition the nested functions cache the inverse value after
## it is calculated so that this value only has to be calculated once for the given input - for
## example, if the inverse calculation was part of a loop that was set to run 100 times, these
## nested functions would prevent the inverse calculation from being calculated 100 times and would
## instead only require that it be calculated once and then would allow the loop to call the 
## calculated value whenever it was needed.

## The makeCacheMatrix function creates a list of four functions that 1)  Sets the value of the matrix
## for which the inverse is to be calculated, 2)  Gets the value of the matrix for which the
## inverse is to be calculated, 3)  Sets (caches) the value of the inverse of the inputted matrix once it
## is calculated, and 4) Gets the cached inverse when called for.

makeCacheMatrix <- function(x = matrix()) {
  
  Nverse <- NULL

  set <- function(y) {
    x <<- y
    Nverse <<- NULL
  }

  get <- function() x

  setNverse <- function(inv) Nverse <<- inv

  getNverse <- function() Nverse

  list(set = set, get = get,
     setNverse = setNverse,
     getNverse = getNverse)
}

## The cacheSolve function below returns the value of the inputted matrix, either by calculating it,
## or if it has already been calculated, pulling it from the cache using the thrid function in the
## list created by makeCacheMatrix ("getNverse").

cacheSolve <- function(x, ...) {
  
  Nverse <- x$getNverse()
  
  if(!is.null(Nverse)) {
    message("getting cached data")
    Nverse
  }
  
  data <- x$get()
  
  Nverse <- solve(data, ...)
  
  x$setNverse(Nverse)
  Nverse
}
