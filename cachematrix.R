## These functions will take a matrix and cache it, as well as cache the inverse,
##so that the inverse will not have to be solved everytime I need the inverse

## This function will take a matrix and cache it (used structure from 
## example and changed to matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will "solve" the matrix (get inverse) on the first call 
## and pull up the inverse in subsequent calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ##Loop to pull cached data if inverse is already found
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
