makeCacheMatrix <- function(x = matrix()) {
  ## this function creates a special matrix that can cache its inverse 
   inv <- NULL  ## initalise inveverse as null
  set <- function(y) {   ## define the set function to assign new
    x <<- y              ## value of matrix in parent enviorm 
    inv <<- NULL         ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x    
  setinv <- function(inverse) inv <<- inverse  ## assigns value of inv in parent envirm
  getinv <- function() {inver <-inv(x)} 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {  ##solves the makeCacheMatrix
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}