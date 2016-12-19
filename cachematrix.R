## makeCacheMatrix creates a special "matrix" object that can cache it's inverse
## cacheSolve pulls the inverse from the chace
## or computes the inverse of said special matrix (if inverse hasn't been
## calculated already)

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() {x}
setinverse <- function(inverse) {inv <<- inverse}
getinverse <- function() {inv}
list(set = set, get = get, 
     setinverse = setinverse,
     getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached inverse")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  return(inv)
}

