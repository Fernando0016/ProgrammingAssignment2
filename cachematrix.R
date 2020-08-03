
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x    ##get the matrix
  setinverse <- function(inverse) i <<- inverse   ##sets the inverse of the matrix
  getinverse <- function() i  ##gets the inverse of the matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if (!is.null(i)) {  ##if the inverse value is not NULL then it returns the cached data
    message("getting cached data")
    return(i)
  }
  data <- x$get()   ##if the inverse value is NULL then it finds the inverse of the matrix
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


