## The following file contains two functions which
## will create a matrix that can cache its inverse and
## a function that calculates the inverse of the matrix
## obtained before.

## This function creates a special list object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y){
    x <<- y
    m <<- matrix()
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the list 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(x)){
      message("Getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmean(m)
  m
}
