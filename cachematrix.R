## Caching the inverse of Matrix
##  cacheSolve function returns inverse of the matrix created with 
##function makeCacheMatrix. If the inverse matrix has already been calculated
## it will will find it in the cache and return it, otherwise
##it will compute, cache and return it. 


## This function creates matrix object 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv<<- NULL
  }
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## this function computes the inverse of the matrix created above
##however if the inverse is already calculated it will retrieve it from cache


cacheSolve <- function(x, ...) {
        
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}
