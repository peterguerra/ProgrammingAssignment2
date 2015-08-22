## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes in a matrix and sets a cached matrix
## returns

makeCacheMatrix <- function(x = matrix()) {

  inver <- NULL

  # set and cache the values
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }

  #set 'get' function
  get <- function() x

  # Set the functions and cache
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function(inverse) inver
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## Write a short comment describing this function
## cacheSolve takes in arguments and sets the inverse of the matrix from the cached one set in makeCacheMatrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  #set matrix
  inver <- x$getInverse()

  # check to see if matrix is cached already and return if so
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }

  # if not, set inverse of matrix and store in cache
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver

}
