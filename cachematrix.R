## This file contains 2 functions "makeCacheMatrix" creates a matrix that can cache its inverse, "cacheSolve" 
## takes a matrix x as an argument, checks if the inverse is cached, if yes uses it, if not calcualtes the inverse.



## Function 1: makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function 2: cacheSolve

cacheSolve <- function(x, ...) {
  ## Return the inverse of matrix "x"
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting Cached Data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}