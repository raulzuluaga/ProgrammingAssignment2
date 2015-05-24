## cacheMatrix.R
## Allow to generate a Matrix based object which can stored its inverse
## and creates a function to calculate inverse of a matrix using the CACHE concept
## to accelerate calculation/improve performance
##
## Using example:
##   c=rbind(c(1, -1/4), c(-1/4, 1)) 
##   cc=makeCacheMatrix(c)
##   cacheSolve(cc)

## Function : makeCacheMatrix
## Parameter: x of type Matrix
## This function creates an object based on a matrix, capable of stored 
## a set of functions to calculate the Inverse matrix using the cache concept.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function : cacheSolve
## Parameter: x of type CacheMatrix contructed using function makeCacheMatrix
## This function returns the Inverse of a Matrix using the Cache Concept:
##  --If the inverse has been calcuted it returns the value
##  --Else it calculates and stores the inverse and returns the value

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  message("Calculating Inverse")
  x$setinverse(m)
  m
}


