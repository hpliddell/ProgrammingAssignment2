## Assignment 2: Caching the Inverse of a Matrix

## Create a "matrix" object that can cache its inverse
## (very similar to vector example)

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse, or retrieves the inverse from the cache

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  
  # If "inv" is null, inverse has already been calculated so we just need to fetch it
  if(!is.null(inv)) { 
    message("Data was in cache!")
    return(inv)
  }
  
  # Otherwise, solve it!
  matrixdata <- x$get()
  message("Solving ...")
  inv <- solve(matrixdata)
  x$setinv(inv)
  return(inv)
  
}

## Sample code to use for testing:
## mymatrix<-matrix(c(4,7,2,6), nrow = 2, ncol = 2)
## cachemat<-makeCacheMatrix(mymatrix)
## cacheSolve(cachemat)
## If we try it twice, we should get the "Solving ..." message the first time and
## the "Data in cache" message the second time. The result should be a matrix of the 
## following elements: 0.6, -0.2, -0.7, 0.4.
