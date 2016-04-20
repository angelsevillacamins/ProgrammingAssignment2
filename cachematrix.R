## These functions calculate and store in cache the inverse a given matrix


## makeCacheMatrix is able to set and get a matrix and its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # Set in cache a new matrix if needed
    x <<- y
    inv <<- NULL
  }
  get <- function() x # Get the matrix stored in cache
  setinverse <- function(inverse) inv <<- inverse # Set in cache the inverse of a matrix 
  getinverse <- function() inv # Get the inverse of a matrix stored in cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates and retrieves the inverse of a matrix stored in 
## cache by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) ## Return the inverse of a matrix that is stored in cache 
  }             ## if it was previously calculated
  data <- x$get()
  inv <- solve(data, ...) 
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
