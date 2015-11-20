## The following functions will create a "special matrix
## object that will cache its inverse and compute the inverse
## if not already cached.

## This function will cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){   
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)

}


## This function will check to see if inverse is cached and 
## and return, if not it will commpute the inverse and cache 
## the result.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setinverse(s)
  s
}

       
}
