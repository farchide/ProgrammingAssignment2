## These funstions is used to cache the inverse of a matrix
#inversing a matrix is resource consuming and it is good to have a cache version of it
# in order to not to inverse it any time we need the inverse matrix
# 1. you should create a CacheMatrix first
#2. use the cacheSolve function to solve the matrix


## This function is to create a cache able matrix
# you will pass a matrix to this function to add some function capability to it
# then you can use the other method to inverse the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function use the solve method to inverse the function for the first time
# after that, because the matrix is cache able, when the user wants to inverse the matrix
# it first check if any cache version is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}
