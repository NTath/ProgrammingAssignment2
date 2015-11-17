## These two functions benefit caching the inverse of a matrix 
## rather than compute it repeatedly.

## makeCacheMatrix(x = matrix()) creates a list with names: 
## "set", "get", "setInverse", "getInverse"
## 1. "set": sets the value of the matrix
## 2. "get": gets the value of the matrix
## 3. "setInverse": sets the the inverse of the matrix
## 4. "getInverse": gets the inverse of the matrix
## 'makeCacheMatrix' has an optional argument x. 
## Creates a list with no matrix value if called without argument or 
## a list with matrix value x if called with argument x. 
## x must be of class matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(invmat) inv <<- invmat
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(x) returns the inverse of the matrix value of the list 'x'. 
## If 'x' has already a value for the inverse matrix, returns the inverse without 
## computating it. If not, computates the inverse using 'solve' and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
