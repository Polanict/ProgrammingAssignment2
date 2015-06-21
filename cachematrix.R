## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Function makeCacheMatrix will create an matrix that is invertable, calculate the inverse and store it to cache.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function()x
      setinv <- function(solve) m<<-solve(x)
      getinv <- function()m
      list(set = set, get = get,setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
##
## Function casheSolve will solve the inverse of the matrix if it is not already in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First check if the inverse is calculated.  If so, return the value as found              in makeCachedMatrix

            m <- x$getinv()
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinv(m)
            m
}

