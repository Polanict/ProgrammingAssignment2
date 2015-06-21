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
      setinv <- function(s) m<<-solve(s)

}


## Write a short comment describing this function
##
## Function casheSolve will solve the inverse of the matrix if it is not already in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##
      cachemean <- function(x, ...) {
            m <- x$setinv()
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinv(m)
            m
      }

}
