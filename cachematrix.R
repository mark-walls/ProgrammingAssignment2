## Programming Assignment 2: Lexical Scoping
## "R Programming" MOOC in the "Data Science Specialization"
## track @ Coursera
## edited and submitted for grading by
## Marco Valli, June 17th 2014

## function 'makeCacheMatrix' creates a matrix object that
## can cache its inverse, by
## - setting the matrix object
## - getting the matrix object
## - setting the inverse of the matrix
## - getting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## function 'cacheSolve' solves (i.e. inverts) a matrix
## created by function 'makeCacheMatrix' above, by
## - checking if the matrix has already been solved (inverted)
## - if so, getting the inverse from the cache, skipping computation
## - otherwise, solving the matrix and setting its inverse in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
          message("getting cached inverse")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
