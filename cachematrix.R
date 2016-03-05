Functions are used to find matrix inverse.
Usually it is a costly computation and there may be some benefit to caching the inverse of a matrix.


makeCacheMatrix  creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <-  NULL
    set <-  function(y) {
    x   <<- y
    inv <<- NULL
  }
    get   <-  function() x
    setinv<-  function(inverse) inv <<- inverse
    getinv<-  function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


cacheSolve  computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
         inv  <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
