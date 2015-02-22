## Function: makeCacheMatrix
## Definition: makeCacheMatrix creates a matrix with a passed in matrix, defines set and get methods and defines
## defines set and get inverse methods.
## Inputs: a matrix
## Outputs: none

## Methods:
##  1. set() - caches a matrix
##  2. get() - returns a matrix
##  3. setInverse() - caches the inverse matrix
##  4. getInverse() - returns an inverse matrix
##  5. list() - defines getters and setters

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function: cacheSolve
## Definition: creates an inverse matrix if one is not already cached, then returns that inverse matrix
## Inputs: a matrix
## Outputs: inverse matrix


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
    
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
    
  m <- solve(data)
    
  x$setInverse(m)
  
  m
  
}
