## Inversion of a large matrix is a costly computation --cpu cycles and memory utilization wise . 
##By caching the matrices along with their inverses in the first call to cacheSolve function, need for recomputation is eliminated.


##makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve would retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("Retrieving the cached inverse")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    message("Inverse computed and cached.
    Returning the newly inversed matrix.")
    m
  }

