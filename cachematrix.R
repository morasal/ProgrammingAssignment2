## These functions together are meant to calculate and cache the inverse of a matrix

## The first function creates the setters and getters necessary
## for the 2nd function in a named list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The 2nd function takes the list created by the 1st function and either
## returns the inverse of the matrix if it has already been calculated and saved in "inv"
## or calculates the inverse of the matrix and save it in "inv".

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinverse(inv)
  inv
}
