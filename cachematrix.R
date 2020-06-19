## These functions illustrate caching results of expensive computations that
## are called frequently. The specific functionality is caching the inverse
## of a matrix after an initial call. Future calls return the cached value instead
## of calculating the inverse with every call
##
## Example: 
##
## mtx <- makeCacheMatrix(matrix(c(0, 2, 2, 0), 2, 2))
## cacheSolve(mtx) 
##
## Returns:
##      [,1] [,2]
##[1,]  0.0  0.5
##[2,]  0.5  0.0
## 


## makeCacheMatrix creates a matrix object that has methods and properties to
## calculate the inverse of a matrix and cache the result for future property
## access
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


## cacheSolve returns the inverse of the supplied matrix that was created
## by the makeCacheMatrix function. It looks for a cached value and returns
## it if it exists or calls the solve function on the supplied matrix object and
## stores it in the new matrix object's cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
