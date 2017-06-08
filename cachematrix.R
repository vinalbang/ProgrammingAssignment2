## The functions calculate the inverse of any invertible matrix. It caches the result.
## If the value of inverse is available it is returned otherwise it is calculated, cached and returned.


## The work of makeCacheMatrix function is to get and set the matrix and to check if its
## inverse is available in cache or not

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


## It checks if matrix has its inverse in cache, if it does not, it calculates 
##it and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z<-makeCacheMatrix(matrix())
  z$set(x)
  m <- z$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- z$get()
  m <- solve(data)
  z$setinverse(m)
  m
}
