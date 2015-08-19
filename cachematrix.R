## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. Two functions 
## are create to minimize computation. makeCacheMatrix, sets, gets, sets inverse,
## gets inverse of the Matrix. cacheSolve, gets the inverse of matrix if already 
## available, else it will calculate and save it in set inverse of matrix


##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get matrix
  get <- function() x
  
  ## set inverse of matrix
  setinverse <- function(minverse) m <<- minverse
  
  ## get inverse of matrix
  getinverse <- function() m
  
  ##To store the 4 functions in the function makeVector, we need the function 
  ##list(), so that when we assign makeVector to an object, the object has all 
  ##the 4 functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and 
##the matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
  ## get inverse matrix
  m <- x$getinverse()
  
  ## test to see if already cached, yes?, then retrieve
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  
  ## no?, get matrix
  data <- x$get()
  
  ## inverse the matrix
  m <- solve(data, ...)
  
  ## set inverse matrix result
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
