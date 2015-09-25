## x is a square, invertible matrix
## makeCacheMatrix returns a list of functions to
## 1. set matrix
## 2. get matrix
## 3. set inverse
## 4. get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  ## 1. set matrix and use <<- to store outside scope of this function
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  }
  ## 2. get matrix
  get = function() x 
  ## 3. set inverse
  setinv = function(inverse) inv <<- inverse 
  ## 4. get inverse
  getinv = function() inv 
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates and returns inverse of the matrix or stored cache

cacheSolve <- function(x,...) {
  i <- x$getinv()
  if(!is.null(i)) { ## if cached data is NOT null...
    message("Returning cached data:")
    return(i)
  }
  ## otherwise, calculate data
  data <- x$get() 
  i <- solve(data) ## calculate
  x$setinv(i)
  i      
}
