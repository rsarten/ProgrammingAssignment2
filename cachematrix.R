## The following functions calculate and caches the inverse of a matrix 'x'
##
## The makeCacheMatrix function creates ('set') the matrix object and has the functions
## to retrieve ('get') 'x', cache the inverse of 'x' ('setinverse'), and to retrieve the
## cached inverse ('getinverse').
##
## The cacheSolve function looks for the cached inverse, and returns it if it exits
## or creates and caches it otherwise, also returning it after.

#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  
  inverse <- NULL # Sets inverse to NULL when function initiated
  
  set <- function(y){
    x <<- y
    inverse <<- NULL # clears cached inverse with new matrix set
  }
  get <- function() x # Function for retrieving matrix 'x'
  setinverse <- function(inv) inverse <<- inv # function for caching inverse
  getinverse <- function() inverse # function for retrieving cached inverse
  
  #returns list of functions relating to matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse() # Look for cached inverse
  if (!is.null(inverse)) {  # If one exists return it
    message("getting inverted matrix")
    return(inverse)
  }
  matrix <- x$get()         # If no cached inverse, retrieve matrix
  inverse <- solve(matrix)  # Calculate inverse matrix
  x$setinverse(inverse)     # Cache inverse
  inverse # Return a matrix that is the inverse of 'x'
}